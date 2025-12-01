# caretSDM Workflow for Species Distribution Modeling

## Introduction

`caretSDM` is a R package that uses the powerful `caret` package as the
main engine to obtain Species Distribution Models. One of its main
attributes is the strong geoprocessing underlying its functions provided
by `stars` package. Here we show how to model species distributions
using `caretSDM` through the function `sdm_area` with a polygon. We will
also show how to apply a PCA in predictors and scenarios to avoid
multicolinearity. The aim of this modeling will be to obtain the current
and future distribution of *Araucaria angustifolia*, a keystone tree
species from South Brazil.

First, we need to open our library.

``` r
library(caretSDM)
start_time <- Sys.time()
set.seed(1)
```

## Pre-Processing

To obtain models, we will need climatic data and species records. To
easily obtain these data, we have two functions: `WorldClim_data`
function downloads climatic variables from WorldClim 2.1, a widely used
open-source database; in the same way, `GBIF_data` function downloads
species records from GBIF, also a widely used open-source database. You
can read more about them by running in the console
[`?GBIF_data`](https://luizesser.github.io/caretSDM/reference/GBIF_data.md)
and
[`?WorldClim_data`](https://luizesser.github.io/caretSDM/reference/WorldClim_data.md).

### Obtaining species records

A easy way to get species data using `caretSDM` is the function
`GBIF_data`, which retrieves species records from GBIF. Understandably,
there are other sources of species data available, as well as our own
data that can be the result of field work. In this sense, one can import
to R it’s own data in multiple ways, but be sure that the table must
always have three columns: species, decimalLongitude and
decimalLatitude. `GBIF_data` function can retrieve the data ready to be
included in caretSDM, thus if you have any doubt on how to format your
own data, use `GBIF_data` function with the parameter `as_df = TRUE` to
retrieve an example table. As standard, `GBIF_data` function sets
`as_df = FALSE`, which makes the function return a `occurrences` object
(more about that further below). An example code for this step would be:

``` r
occ <- GBIF_data(c("Araucaria angustifolia"), as_df = TRUE)
```

But we already have a `occ` object included in the package, which is the
same output, but with filtered records to match our study area. Note
that coordinates are in a metric CRS (EPSG: 6933).

``` r
occ |> head()
#>                  species decimalLongitude decimalLatitude
#> 1 Araucaria angustifolia         -5071809        -3259770
#> 2 Araucaria angustifolia         -4983891        -3283348
#> 3 Araucaria angustifolia         -4748996        -3142075
#> 4 Araucaria angustifolia         -4883188        -3092641
#> 5 Araucaria angustifolia         -4766386        -3227445
#> 6 Araucaria angustifolia         -4755861        -3139607
```

### Obtaining climatic data

For climatic data, we will first download and import current data, which
is used to build the models. `WorldClim_data` function has an argument
to set the directory in which you want to save the files. If you don’t
set it, files will be saved in your working directory (run
[`getwd()`](https://rdrr.io/r/base/getwd.html) to find out your working
directory) in the folder “input_data/WorldClim_data_current/”. If period
is set to “future”, then it is saved in
“input_data/WorldClim_data_future/”. We could run this script with a
smaller resolution, but as the aim here is to show how the package
works, we will use a resolution of 10 arc-minutes, which is very coarse,
but quicker to download and run.

``` r
# Download current bioclimatic variables
WorldClim_data(path = NULL, 
               period = "current", 
               variable = "bioc", 
               resolution = 10)

# Import current bioclimatic variables to R
bioc <- read_stars(list.files("input_data/WorldClim_data_current/", full.names = T), along = "band", normalize_path = F)
```

As in the previous section, we already have a `bioc` object included in
the package, which is the same output, but masked to match our study
area and with fewer variables.

``` r
bioc
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min.  1st Qu.   Median     Mean 3rd Qu. Max. NA's
#> current  14.58698 21.19678 298.9147 622.9417  1353.5 2368 1845
#> dimension(s):
#>      from  to offset   delta refsys point              values x/y
#> x     747 798   -180  0.1667 WGS 84 FALSE                NULL [x]
#> y     670 706     90 -0.1667 WGS 84 FALSE                NULL [y]
#> band    1   3     NA      NA     NA    NA bio1 , bio4 , bio12
```

### Defining the study area

A important step on model building in Species Distribution Models, is
the definition of accessible area (the M in BAM diagram). This area can
be, in Geographical Information Systems terms, as an example, the
delimitation of a habitat (polygon) or a river basin network (lines).
Another broadly used approach is the use of buffers around presences.
The buffer size translates the potential distribution capabilities of a
species. To educational purposes, we will use a simple polygon of Parana
state boundaries that is available in caretSDM as the `parana` object
(see
[`?parana`](https://luizesser.github.io/caretSDM/reference/parana.md)
for more information on the data)..

``` r
parana
#> Simple feature collection with 1 feature and 4 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -54.61834 ymin: -26.71679 xmax: -48.02308 ymax: -22.51621
#> Geodetic CRS:  WGS 84
#>   GID0 CODIGOIB1 NOMEUF2 SIGLAUF3                           geom
#> 1   19        41  PARANA       PR MULTIPOLYGON (((-52.06416 -...
```

``` r
parana |> select_predictors(NOMEUF2) |> plot()
```

![](Araucaria_files/figure-html/parana_view-1.png)

The `sdm_area` function is responsible to create a grid to build models,
a key aspect of caretSDM workflow. With a grid built, modelers can pass
multiple rasters with different resolutions, CRSs and extents. The
package will be responsible to rescale, transform and crop every raster
to match the grid. The grid returned by the `sdm_area` function is from
`sdm_area` class, a class that will also keep the environmental/climatic
data (*i.e.* “predictor variables”, “covariates”, “explanatory
variables”, “features” or “control variables”). With this class we will
perform analysis using only the predictors. The grid is built using
mostly the first three arguments: (1) a shape from `sf` class, but
rasters from `stars`, `rasterStack` or `SpatRaster` class are also
allowed; (2) the cell size of the grid; and (3) the Coordinate Reference
System (CRS). Note that the cell size can be metric or not depending on
the CRS informed. It is important to inform a cell size bigger than the
coarser raster that will be used, otherwise rescaling process may return
empty cells. The rescaling can be performed using GDAL (quicker but less
precise) or the `stars` package (slower but more precise). The first
will address values to cells by calculating the mean (for continuous
variables) or the median (for categorical variables) of the values
falling within the cell. The approach using `stars` will do the same
thing, but weighting for the area of each value within the cell. For
other arguments meaning see
[`?sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md).

``` r
sa <- sdm_area(parana, 
               cell_size = 25000, 
               crs = 6933, 
               variables_selected = NULL,
               gdal = TRUE, 
               crop_by = NULL, 
               lines_as_sdm_area = FALSE)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
sa
#>           caretSDM         
#> ...........................
#> Class                     : sdm_area
#> Extent                    : -5276744 -3295037 -4626744 -2795037 (xmin, xmax, ymin, ymax)
#> CRS                       : WGS 84 / NSIDC EASE- 
#> Resolution                : (25000, 25000) (x, y)
#> Number of Predictors      : 4 
#> Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3
```

Note that the function returned four predictor variables
(`Predictor Names` above). These “predictors” are actually columns
included in the `parana` shape’s data table. One can filter these
variables using `select_predictors` function, but we will not do that
here, once the package will automatically drop them further. You can
explore the grid generated and stored in the `sdm_area` object using the
functions
[`mapview_grid()`](https://luizesser.github.io/caretSDM/reference/plot_occurrences.md)
or
[`plot_grid()`](https://luizesser.github.io/caretSDM/reference/plot_occurrences.md).

``` r
plot_grid(sa)
```

![](Araucaria_files/figure-html/plot_sdm_area-1.png)

Now that we have a study area, we can assign predictor variables to it.
To do that, we use the `add_predictors` function, which usually will
only use the fist two arguments, which are the `sdm_area` build in the
previous step and the `RasterStack`, `SpatRaster` or `stars` object with
predictors data. Note that `add_predictors` also has a `gdal` argument,
which works as the previous one in `sdm_area` function.

``` r
sa <- add_predictors(sa, 
                     bioc, 
                     variables_selected = NULL, 
                     gdal = TRUE)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
sa
#>           caretSDM         
#> ...........................
#> Class                     : sdm_area
#> Extent                    : -5276744 -3295037 -4626744 -2795037 (xmin, xmax, ymin, ymax)
#> CRS                       : WGS 84 / NSIDC EASE- 
#> Resolution                : (25000, 25000) (x, y)
#> Number of Predictors      : 7 
#> Predictors Names          : GID0, CODIGOIB1, NOMEUF2, SIGLAUF3, bio1, bio4, bio12
```

Predictors variables are used to train the models. After training the
models, we need to project models into scenarios. Currently, we don’t
have any scenario in our `sdm_area` object. We can address the
predictors data as the current scenario by applying the function
`add_scenario` without considering any other argument. This happens
because the argument `pred_as_scen` is standarly set to `TRUE`.

``` r
add_scenarios(sa)
```

If we are aiming to project species distributions in other scenarios, we
can download data and add in the same way we did for current data.

``` r
WorldClim_data(path = NULL, 
               period = "future", 
               variable = "bioc",
               year = "2090",
               gcm = c("ca", "mi"),
               ssp = c("245","585"),
               resolution = 10)

scen <- read_stars(list.files("input_data/WorldClim_data_future/", full.names = T), along = "band", normalize_path = F)
```

As with current bioclimatic data, we have already included the `scen`
object in the package, which is the same output from above, but masked
to match our study area and with fewer variables (see
[`?scen`](https://luizesser.github.io/caretSDM/reference/scen.md) for
more information on the data).

``` r
scen
#> stars object with 3 dimensions and 4 attributes
#> attribute(s):
#>                 Min. 1st Qu. Median     Mean  3rd Qu.   Max. NA's
#> ca_ssp245_2090  18.4  26.100 296.50 570.0926 1188.975 2049.2 1908
#> ca_ssp585_2090  22.2  31.275 293.25 516.0384 1033.150 1862.2 1908
#> mi_ssp245_2090  16.3  23.000 314.65 660.9749 1426.800 2414.8 1908
#> mi_ssp585_2090  17.9  24.500 323.45 702.6153 1534.100 2585.0 1908
#> dimension(s):
#>      from  to offset   delta refsys point              values x/y
#> x     747 798   -180  0.1667 WGS 84 FALSE                NULL [x]
#> y     670 706     90 -0.1667 WGS 84 FALSE                NULL [y]
#> band    1   3     NA      NA     NA    NA bio1 , bio4 , bio12
```

Now we can add the current and future scenarios at once in our
`sdm_area` object. For the meaning on other parameters see the help file
at
[`?add_scenarios`](https://luizesser.github.io/caretSDM/reference/add_scenarios.md).
When adding scenarios, the function will test if all variables are
available in all scenarios, otherwise it will filter predictors (see the
warning below). See that we also provide a stationary argument, where
the modeler can inform variables that do not change between scenarios.
These variables can be, e.g., soil variables.

``` r
sa <- add_scenarios(sa, 
                    scen = scen, 
                    scenarios_names = NULL,
                    pred_as_scen = TRUE,
                    variables_selected = NULL, 
                    stationary = NULL)
#> Warning: Some variables in `variables_selected` are not present in `scen`.
#> ℹ Using only variables present in `scen`: bio1, bio4, and bio12
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
sa
#>           caretSDM         
#> ...........................
#> Class                     : sdm_area
#> Extent                    : -5276744 -3295037 -4626744 -2795037 (xmin, xmax, ymin, ymax)
#> CRS                       : WGS 84 / NSIDC EASE- 
#> Resolution                : (25000, 25000) (x, y)
#> Number of Predictors      : 3 
#> Predictors Names          : bio1, bio4, bio12 
#> Number of Scenarios      : 5 
#> Scenarios Names          : ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090, current
```

It is common that modelers need to subset variables that will inform
models. This can be due to statistical artifacts that are common in
quarter bioclimatic variables, or a causation subset, aiming for those
variables with causality effect on species distribution. The user may
also want to change scenarios names, predictors names or retrieve
predictors data. For that there are a myriad of functions that can be
found in the package, most of them under the help files of the functions
[`?add_predictors`](https://luizesser.github.io/caretSDM/reference/add_predictors.md)
and
[`?add_scenarios`](https://luizesser.github.io/caretSDM/reference/add_scenarios.md),
but also
[`?select_predictors`](https://luizesser.github.io/caretSDM/reference/tidyverse-methods.md).

### Defining the occurrences set in the study area

As `caretSDM` has a strong GIS background, it is necessary to explicitly
tell which CRS is your data in. This will assure that every GIS
transformation is correct. `occurrences_sdm` function creates a
occurrences class (*i.e.* “response variable”, “target” or “label”) that
will be used in occurrences’ transformations and functions, as
pseudoabsences generation. For a reference, GBIF data is in crs = 4326,
but our records stored in `occ` object is transformed to 6933 (see
[`?occ`](https://luizesser.github.io/caretSDM/reference/occ.md) for more
information on the data).

``` r
oc <- occurrences_sdm(occ, crs = 6933)
oc
#>         caretSDM       
#> .......................
#> Class                 : occurrences
#> Species Names         : Araucaria angustifolia 
#> Number of presences   : 420 
#> =================================
#> Data:
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -5071809 ymin: -3283348 xmax: -4748996 ymax: -3092641
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#>                  species                  geometry
#> 1 Araucaria angustifolia POINT (-5071809 -3259770)
#> 2 Araucaria angustifolia POINT (-4983891 -3283348)
#> 3 Araucaria angustifolia POINT (-4748996 -3142075)
#> 4 Araucaria angustifolia POINT (-4883188 -3092641)
#> 5 Araucaria angustifolia POINT (-4766386 -3227445)
#> 6 Araucaria angustifolia POINT (-4755861 -3139607)
```

``` r
plot_occurrences(oc)
```

![](Araucaria_files/figure-html/plot_occurrences-1.png)

This next step assigns occurrences into a study area, excluding records
outside the study area or with NAs as predictors.

``` r
oc <- join_area(oc, sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.
```

### The `input_sdm` class

In `caretSDM` we use multiple classes to perform our analysis. Every
time we perform a new analysis, objects keep the information of what we
did. Ideally, the workflow will have only one object throughout it. The
`input_sdm` class is the key class in the workflow, where every function
will orbitate. That class puts occurrences, predictors, scenarios,
models and predictions together to perform analysis that are only
possible when two or more of these classes are available. First, we
create the object by informing the occurrences and the sdm_area.

``` r
i <- input_sdm(oc, sa)
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 418 
#> --------  Predictors  ---------
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current
```

### Data cleaning routine

As the first step in our workflow with the `input_sdm` object, we will
clean our occurrences data by applying a group of functions from the
package `CoordinateCleaner`. In this function, we also provide a way to
check for environmental duplicates, by including a predictors object.
This function also checks for records in the sea if the species is
terrestrial, but note that this can be switched off if the studied
species is not terrestrial. The way `caretSDM` works, we can always
overwrite the main `input_sdm` object to update it. The function will
return a new object with all the previous information and the new
information obtained from the `data_clean` function, note that at the
end of the Data Cleaning information there is the Duplicated Cell
method. This method is only possible when we have both the `occurrence`
and `predictors` data.

``` r
i <- data_clean(i,
                capitals = TRUE,
                centroids = TRUE,
                duplicated = TRUE,
                identical = TRUE,
                institutions = TRUE,
                invalid = TRUE,
                terrestrial = TRUE)
#> Cell_ids identified, removing duplicated cell_id.
#> Testing country capitals
#> Removed 0 records.
#> Testing country centroids
#> Removed 0 records.
#> Testing duplicates
#> Removed 0 records.
#> Testing equal lat/lon
#> Removed 0 records.
#> Testing biodiversity institutions
#> Removed 1 records.
#> Testing coordinate validity
#> Removed 0 records.
#> Testing sea coordinates
#> Reading ne_110m_land.zip from naturalearth...
#> Removed 0 records.
#> 
#> Predictors identified, procceding with grid filter (removing NA and duplicated data).
```

### Removing multicolinearity from predictors’ data

There are two main methods in the SDM literature to consider
multicolinearity in predictors data. One is the use of VIFs, which in
`caretSDM` is performed using `vif_predictors`function. There, users are
able to perform variables selection through `usdm` package. The function
is a wrapper for
[`usdm::vifcor`](https://rdrr.io/pkg/usdm/man/vif.html), where variables
are kept given a maximum threshold of colinearity. The standard is 0.5.
Here is a example code for demonstration:

``` r
i <- vif_predictors(i, 
                    th = 0.5, 
                    maxobservations = 5000, 
                    variables_selected = NULL)
```

For this study, however, we will use the PCA approach to
multicolinearity, where we synthesize environmental variability into
PCA-axis and project these axis to the geographic space to use them as
predictors. `pca_predictors` does not have arguments other than the
`input_sdm` object. PCA-axis will be included in predictors together
with raw variables.

``` r
i <- pca_predictors(i, cumulative_proportion = 1)
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 6 
#> Predictors Names              : bio1, bio4, bio12, PC1, PC2, PC3 
#> PCA-transformed variables     : DONE 
#> Cummulative proportion ( 1 ) : PC1, PC2, PC3 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current
```

To better visualize PCA parameters, users can run `pca_summary` and
`get_pca_model` functions, which are very self-explanatory.

``` r
pca_summary(i)
#> Importance of components:
#>                             PC1      PC2     PC3
#> Standard deviation     204.4810 16.52253 1.55697
#> Proportion of Variance   0.9935  0.00649 0.00006
#> Cumulative Proportion    0.9935  0.99994 1.00000
get_pca_model(i)
#> Standard deviations (1, .., p=3):
#> [1] 204.480951  16.522529   1.556971
#> 
#> Rotation (n x k) = (3 x 3):
#>                PC1          PC2          PC3
#> bio1  -0.004403833 -0.006704105 -0.999967830
#> bio4   0.995793285  0.091491771 -0.004998839
#> bio12  0.091522341 -0.995783265  0.006272988
```

### Obtaining pseudoabsence data

Pseudoabsence data will be stored in the `occurrences` object (inside
the `input_sdm`). To generate them, you must inform some parameters.
Probably one of the most important arguments in this function is the
`method`. Currently, two methods are implemented: a “random”, which
takes random grid cells as pseudoabsences; and a “bioclim” method, which
creates a Surface Range Envelope (SRE) using presence records, binarizes
the projection of the SRE using the `th` threshold and then retrieves
pseudoabsences outside the envelope. The number of pseudoabsences
created can be changed using the `n_pa` parameter. When set to NULL,
`n_pa` will be equal the number of occurrences (to avoid imbalance
issues). The number of sets of pseudoabsences is adjusted with the
`n_set` parameter in the function. The argument `variables_selected`
will inform which variables you want to use to build your
pseudoabsences/models. This can either be a vector of variables names or
a previously performed selection method.

``` r
i <- pseudoabsences(i, 
                    method = "bioclim", 
                    n_set = 10,
                    n_pa = NULL,
                    variables_selected = "pca",
                    th = 0) 
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 84 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 6 
#> Predictors Names              : bio1, bio4, bio12, PC1, PC2, PC3 
#> PCA-transformed variables     : DONE 
#> Cummulative proportion ( 1 ) : PC1, PC2, PC3 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current
```

## Processing

### Modeling species relationship with variables

With the occurrences and predictors data put together, we can pass to
the modeling. As the name suggests, `caretSDM` uses the `caret` package
underlying its modeling procedure. For those who are not familiar,
`caret` is the easiest way to perform Machine Learning analysis in R. It
works by setting a modeling wrapper to pass multiple packages and can
provide a lot of automation regarding algorithms fine-tuning, data
spliting, pre-processing methods and predictions. These automated
functions from `caret` can be altered using the `ctrl` argument in
`train_sdm` function. See
[`?caret::trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html)
for all options available.

We show here how to use a repeated crossvalidation method, which is
defined through
[`caret::trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html).

Note that, when you are using an algorithm for the first time, caret
will ask you to install the relevant packages to properly run the
algorithm.

``` r
ctrl_sdm <- caret::trainControl(method = "repeatedcv", 
                                number = 4, 
                                repeats = 1, 
                                classProbs = TRUE,
                                returnResamp = "all", 
                                summaryFunction = summary_sdm, 
                                savePredictions = "all")

i <- train_sdm(i, 
               algo = c("naive_bayes", "kknn"), 
               variables_selected = "pca", 
               ctrl=ctrl_sdm) |> suppressWarnings()
#> Loading required package: ggplot2
#> Loading required package: lattice
#> 
#> Attaching package: 'caret'
#> The following object is masked from 'package:caretSDM':
#> 
#>     predictors
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 84 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 6 
#> Predictors Names              : bio1, bio4, bio12, PC1, PC2, PC3 
#> PCA-transformed variables     : DONE 
#> Cummulative proportion ( 1 ) : PC1, PC2, PC3 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : PC1 PC2 PC3 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9660175 0.8737179     0.96895    0.905850
#> 2 naive_bayes 0.9759408 0.9047619     0.98205    0.922625
```

## Post-Processing

### Predicting species distribution in given scenarios

Now that we have our models, we can make predictions in new scenarios.
The function `predict_sdm` incorporates also the prediction of ensembles
(`ensembles=TRUE` is standard). The function will only predict models
that passes a given validation threshold. This validation metric is set
using `metric` and `th` arguments. In the following example, metric is
set to be “ROC” and th is equal 0.9. This means that only models with
ROC \> 0.9 will be used in predictions and ensembles.

``` r
i <- predict_sdm(i,
                 metric = "ROC",
                 th = 0.9,
                 tp = "prob",
                 ensembles = TRUE)
#> [1] "Projecting: 1/5"
#> [1] "Projecting: 2/5"
#> [1] "Projecting: 3/5"
#> [1] "Projecting: 4/5"
#> [1] "Projecting: 5/5"
#> [1] "Ensembling..."
#> [1] "ca_ssp245_2090"
#> [1] "Araucaria angustifolia"
#> [1] "ca_ssp585_2090"
#> [1] "Araucaria angustifolia"
#> [1] "mi_ssp245_2090"
#> [1] "Araucaria angustifolia"
#> [1] "mi_ssp585_2090"
#> [1] "Araucaria angustifolia"
#> [1] "current"
#> [1] "Araucaria angustifolia"
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 84 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 6 
#> Predictors Names              : bio1, bio4, bio12, PC1, PC2, PC3 
#> PCA-transformed variables     : DONE 
#> Cummulative proportion ( 1 ) : PC1, PC2, PC3 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : PC1 PC2 PC3 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9660175 0.8737179     0.96895    0.905850
#> 2 naive_bayes 0.9759408 0.9047619     0.98205    0.922625
#> 
#> --------  Predictions  --------
#> Ensembles                     :
#>     Scenarios                 : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
#>     Methods                   : mean_occ_prob wmean_AUC committee_avg 
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.9
```

In the above print, it is possible to see the “Methods” under the
“Predictions” section, which informs which ensemble types were made:
mean occurrence probability (`mean_occ_prob`; a simple mean between
GCMs), mean occurrence probability weighted by AUC/ROC (`wmean_AUC`;
AUC/ROC values are used as weights), and the majority rule, or the
committee average (`committee_avg`; the sum of binaries).

Besides the AUC/ROC metric, users can get every available metric by
model using the following code before commit to “ROC”:

``` r
get_validation_metrics(i)
#> $`Araucaria angustifolia`
#>              algo       ROC       TSS Sensitivity Specificity Pos Pred Value
#> m1.2         kknn 0.9709631 0.9006410     1.00000     0.90075        0.94625
#> m2.2         kknn 0.9606227 0.9139194     0.95200     0.96150        0.97600
#> m3.2         kknn 0.9665751 0.8608059     0.97625     0.88450        0.93200
#> m4.2         kknn 0.9884386 0.8665293     0.97600     0.90225        0.94500
#> m5.2         kknn 0.9418117 0.8546245     0.95225     0.90225        0.94425
#> m6.2         kknn 0.9587912 0.8782051     0.95250     0.92575        0.95375
#> m7.2         kknn 0.9770408 0.8543956     0.96400     0.89025        0.93275
#> m8.2         kknn 0.9752747 0.8518773     0.98800     0.86375        0.92525
#> m9.2         kknn 0.9606227 0.8706502     0.95225     0.91825        0.95375
#> m10.2        kknn 0.9600340 0.8855311     0.97625     0.90925        0.94325
#> m1.1  naive_bayes 0.9617674 0.8559982     0.97600     0.87975        0.93450
#> m2.1  naive_bayes 0.9830586 0.9377289     0.97600     0.96150        0.97675
#> m3.1  naive_bayes 0.9734432 0.8727106     0.98800     0.88450        0.93300
#> m4.1  naive_bayes 0.9505495 0.8592033     0.97625     0.88300        0.93375
#> m5.1  naive_bayes 0.9970238 0.9271978     0.98800     0.93900        0.96600
#> m6.1  naive_bayes 0.9895343 0.9510073     0.98800     0.96300        0.97675
#> m7.1  naive_bayes 0.9927394 0.9079670     0.96425     0.94375        0.96425
#> m8.1  naive_bayes 0.9851954 0.8903388     0.98800     0.90225        0.94550
#> m9.1  naive_bayes 0.9684829 0.9361264     0.97600     0.96000        0.97675
#> m10.1 naive_bayes 0.9576138 0.9093407     1.00000     0.90950        0.94625
#>       Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
#> m1.2         1.00000   0.94625 1.00000 0.97175     0.6315        0.63150
#> m2.2         0.92600   0.97600 0.95200 0.96400     0.6180        0.58800
#> m3.2         0.96150   0.93200 0.97625 0.95350     0.6180        0.60325
#> m4.2         0.96000   0.94500 0.97600 0.95350     0.6225        0.60750
#> m5.2         0.93075   0.94425 0.95225 0.94650     0.6270        0.59725
#> m6.2         0.93100   0.95375 0.95250 0.95225     0.6135        0.58450
#> m7.2         0.94375   0.93275 0.96400 0.94725     0.6090        0.58675
#> m8.2         0.97500   0.92525 0.98800 0.95525     0.6225        0.61500
#> m9.2         0.92425   0.95375 0.95225 0.95250     0.6315        0.60175
#> m10.2        0.96425   0.94325 0.97625 0.95925     0.6045        0.59025
#> m1.1         0.95800   0.93450 0.97600 0.95425     0.6315        0.61650
#> m2.1         0.96300   0.97675 0.97600 0.97625     0.6180        0.60300
#> m3.1         0.97925   0.93300 0.98800 0.95975     0.6180        0.61050
#> m4.1         0.96425   0.93375 0.97625 0.95350     0.6225        0.60775
#> m5.1         0.97725   0.96600 0.98800 0.97675     0.6270        0.61950
#> m6.1         0.98225   0.97675 0.98800 0.98225     0.6135        0.60625
#> m7.1         0.95000   0.96425 0.96425 0.96350     0.6090        0.58675
#> m8.1         0.98075   0.94550 0.98800 0.96550     0.6225        0.61500
#> m9.1         0.96000   0.97675 0.97600 0.97625     0.6315        0.61650
#> m10.1        1.00000   0.94625 1.00000 0.97175     0.6045        0.60450
#>       Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
#> m1.2               0.66900           0.95025  0.96300 0.91775       0.83525
#> m2.2               0.60300           0.95700  0.95600 0.90700       0.82500
#> m3.2               0.64700           0.93050  0.94125 0.87425       0.80550
#> m4.2               0.65175           0.93350  0.94075 0.87200       0.80350
#> m5.2               0.63450           0.92725  0.93350 0.85800       0.79300
#> m6.2               0.61325           0.93925  0.94225 0.87875       0.80750
#> m7.2               0.63050           0.92725  0.93475 0.86150       0.79575
#> m8.2               0.66675           0.92575  0.94100 0.86975       0.80775
#> m9.2               0.63175           0.93550  0.94000 0.87125       0.80025
#> m10.2              0.62600           0.94300  0.95000 0.89425       0.82200
#> m1.1               0.69950           0.92800  0.94025 0.86900       0.80175
#> m2.1               0.64700           0.96900  0.97075 0.93775       0.84850
#> m3.1               0.66175           0.93650  0.94875 0.88900       0.81500
#> m4.1               0.67400           0.92975  0.94125 0.87275       0.80375
#> m5.1               0.65700           0.96375  0.97000 0.93425       0.84875
#> m6.1               0.67125           0.97550  0.97850 0.95450       0.86225
#> m7.1               0.65200           0.95400  0.95600 0.90875       0.83075
#> m8.1               0.66675           0.94550  0.95550 0.90275       0.82625
#> m9.1               0.67600           0.96825  0.97000 0.93550       0.84525
#> m10.1              0.64775           0.95475  0.96400 0.92300       0.84175
#>       AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
#> m1.2        0.99475       0.6315        0.00000     0.7493333       21    12.25
#> m2.2        0.99600       0.6180        0.00000     1.0000000       21    13.00
#> m3.2        0.98950       0.6180        0.00025     0.8700000       21    13.00
#> m4.2        0.99150       0.6225        0.00000     0.8120000       21    12.75
#> m5.2        0.98650       0.6270        0.00025     0.7162500       21    12.50
#> m6.2        0.98975       0.6135        0.00000     0.8700000       21    13.25
#> m7.2        0.99025       0.6090        0.00000     0.8120000       21    13.50
#> m8.2        0.98550       0.6225        0.00075     0.6170000       21    12.75
#> m9.2        0.99150       0.6315        0.00000     0.8700000       21    12.25
#> m10.2       0.99000       0.6045        0.00000     0.8266667       21    13.75
#> m1.1        0.98950       0.6315        0.00125     0.7742500       21    12.25
#> m2.1        0.99775       0.6180        0.00000     1.0000000       21    13.00
#> m3.1        0.99300       0.6180        0.00075     0.8700000       21    13.00
#> m4.1        0.99000       0.6225        0.00050     0.8120000       21    12.75
#> m5.1        0.99500       0.6270        0.00250     1.0000000       21    12.50
#> m6.1        0.99800       0.6135        0.00125     1.0000000       21    13.25
#> m7.1        0.99125       0.6090        0.00075     0.8723333       21    13.50
#> m8.1        0.99325       0.6225        0.00000     0.7493333       21    12.75
#> m9.1        0.99775       0.6315        0.00525     1.0000000       21    12.25
#> m10.1       0.99500       0.6045        0.00250     0.8625000       21    13.75
#>       True Positive False Positive True Negative False Negative       CBI pAUC
#> m1.2          21.00           0.25         11.00           1.25 0.7052500  NaN
#> m2.2          20.00           1.00         12.50           0.50 0.4620000  NaN
#> m3.2          20.50           0.50         11.50           1.50 0.6305000  NaN
#> m4.2          20.50           0.75         11.50           1.50 0.4277500  NaN
#> m5.2          20.00           1.00         11.25           1.25 1.0000000  NaN
#> m6.2          20.00           1.00         12.25           1.00 1.0000000  NaN
#> m7.2          20.25           0.75         12.00           1.50 0.6017500  NaN
#> m8.2          20.75           0.25         11.00           1.75 0.4845000  NaN
#> m9.2          20.00           1.00         11.25           1.00 0.6580000  NaN
#> m10.2         20.50           0.50         12.50           1.25 0.6666667  NaN
#> m1.1          20.50           0.75         10.75           3.00 0.5865000  NaN
#> m2.1          20.50           0.75         12.50           1.75 1.0000000  NaN
#> m3.1          20.75           0.50         11.50           2.00 0.6007500  NaN
#> m4.1          20.50           0.75         11.25           2.50 0.7790000  NaN
#> m5.1          20.75           1.50         11.75           2.50 0.7345000  NaN
#> m6.1          20.75           1.00         12.75           3.00 0.7032500  NaN
#> m7.1          20.25           1.00         12.75           2.50 0.5352500  NaN
#> m8.1          20.75           0.25         11.50           1.75 0.6975000  NaN
#> m9.1          20.50           0.75         11.75           2.25 1.0000000  NaN
#> m10.1         21.00           0.75         12.50           2.25 0.8322500  NaN
#>       Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
#> m1.2         0.08325 0.03430299 0.09631361    0.02400000    0.09615396
#> m2.2         0.04800 0.02710375 0.04441156    0.00000000    0.04445597
#> m3.2         0.09500 0.04821119 0.08174327    0.04750000    0.04445597
#> m4.2         0.08325 0.01240707 0.08576950    0.04552197    0.09653799
#> m5.2         0.04775 0.06620172 0.09351240    0.06741105    0.09653799
#> m6.2         0.09500 0.05488397 0.07801281    0.05484828    0.05846580
#> m7.2         0.08325 0.01659437 0.04586075    0.02400000    0.06955753
#> m8.2         0.08325 0.01821554 0.15176505    0.02400000    0.13065572
#> m9.2         0.05975 0.03509803 0.05746839    0.03878466    0.06825137
#> m10.2        0.05975 0.05612245 0.10099865    0.04750000    0.06803124
#> m1.1         0.09500 0.05275527 0.10254059    0.02771281    0.10049668
#> m2.1         0.09500 0.03388278 0.05223260    0.04552197    0.07372189
#> m3.1         0.09500 0.04005647 0.12264226    0.04750000    0.08891194
#> m4.1         0.09500 0.07004547 0.07407353    0.04750000    0.09792982
#> m5.1         0.09500 0.02756226 0.10105261    0.02713546    0.08381328
#> m6.1         0.09500 0.03248127 0.08301910    0.03878466    0.05426171
#> m7.1         0.09500 0.02644311 0.12655819    0.07150000    0.08878204
#> m8.1         0.09500 0.04448938 0.09698425    0.02400000    0.09653799
#> m9.1         0.09500 0.03657180 0.18927987    0.04552197    0.16492220
#> m10.1        0.09500 0.08411826 0.17836860    0.04552197    0.13402985
#>       Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
#> m1.2        0.05202163      0.041500000  0.05202163 0.02400000 0.02921757
#> m2.2        0.02771281      0.003464102  0.02771281 0.00000000 0.01385641
#> m3.2        0.02675818      0.077000000  0.02675818 0.04750000 0.03395585
#> m4.2        0.05180090      0.073259243  0.05180090 0.04552197 0.02696139
#> m5.2        0.05172604      0.094422367  0.05172604 0.06741105 0.03717078
#> m6.2        0.03881044      0.079778861  0.03881044 0.05484828 0.03397425
#> m7.2        0.03850000      0.037606515  0.03850000 0.02400000 0.00950000
#> m8.2        0.07102288      0.050000000  0.07102288 0.02400000 0.04783566
#> m9.2        0.03566861      0.058636593  0.03566861 0.03878466 0.02007486
#> m10.2       0.04372928      0.071500000  0.04372928 0.04750000 0.04056579
#> m1.1        0.05577634      0.048833049  0.05577634 0.02771281 0.03182635
#> m2.1        0.03639139      0.068987922  0.03639139 0.04552197 0.01960230
#> m3.1        0.04956729      0.083500000  0.04956729 0.04750000 0.04431986
#> m4.1        0.04825885      0.071500000  0.04825885 0.04750000 0.02724579
#> m5.1        0.04352011      0.045500000  0.04352011 0.02713546 0.03299874
#> m6.1        0.03293428      0.068314835  0.03293428 0.03878466 0.03235609
#> m7.1        0.05502121      0.100000000  0.05502121 0.07150000 0.04692547
#> m8.1        0.05187485      0.041500000  0.05187485 0.02400000 0.02921757
#> m9.1        0.08504705      0.094633944  0.08504705 0.04552197 0.05776028
#> m10.1       0.07695182      0.086183428  0.07695182 0.04552197 0.06271098
#>       PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
#> m1.2     0.0090000       0.01469694             0.03322022          0.04793398
#> m2.2     0.0000000       0.00000000             0.01732051          0.02193931
#> m3.2     0.0000000       0.02950000             0.02367840          0.04131586
#> m4.2     0.0090000       0.02135416             0.05235376          0.04272782
#> m5.2     0.0103923       0.04714075             0.06501538          0.04652866
#> m6.2     0.0090000       0.03923009             0.04241364          0.03911841
#> m7.2     0.0103923       0.01192686             0.03796051          0.02285279
#> m8.2     0.0090000       0.01989975             0.03652739          0.07599287
#> m9.2     0.0090000       0.03181588             0.04779383          0.02854820
#> m10.2    0.0090000       0.03262284             0.03029851          0.05049752
#> m1.1     0.0090000       0.02368544             0.04311612          0.05103594
#> m2.1     0.0000000       0.02828869             0.05367805          0.02594867
#> m3.1     0.0000000       0.02950000             0.02950000          0.06151694
#> m4.1     0.0090000       0.03388584             0.06231640          0.03691770
#> m5.1     0.0103923       0.01980530             0.04768648          0.05045377
#> m6.1     0.0090000       0.02559948             0.01944865          0.04154917
#> m7.1     0.0103923       0.03942398             0.03840464          0.06326136
#> m8.1     0.0090000       0.01989975             0.03809090          0.04834942
#> m9.1     0.0090000       0.02521904             0.04872371          0.09463042
#> m10.1    0.0090000       0.03295451             0.03598611          0.08913426
#>       AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
#> m1.2  0.03809965 0.08379091      0.05662964     0.009178780      0.0090000
#> m2.2  0.01732051 0.03695042      0.02540341     0.003464102      0.0000000
#> m3.2  0.04195533 0.08769407      0.05753550     0.015264338      0.0000000
#> m4.2  0.03495116 0.07621898      0.05028916     0.010392305      0.0090000
#> m5.2  0.04389001 0.09040280      0.05866856     0.015524175      0.0103923
#> m6.2  0.04007805 0.08249596      0.05382379     0.014773287      0.0090000
#> m7.2  0.01386542 0.03112876      0.01798842     0.005500000      0.0103923
#> m8.2  0.06341924 0.14058064      0.08805822     0.023867691      0.0090000
#> m9.2  0.02370654 0.04933812      0.03235609     0.007549834      0.0090000
#> m10.2 0.04860041 0.10126656      0.07029936     0.014988885      0.0090000
#> m1.1  0.04149197 0.09034379      0.05521096     0.015264338      0.0090000
#> m2.1  0.02408838 0.05103838      0.03841441     0.006928203      0.0000000
#> m3.1  0.05648820 0.12162339      0.07611176     0.023113849      0.0000000
#> m4.1  0.03377746 0.07203876      0.04709830     0.012543258      0.0090000
#> m5.1  0.04290299 0.09429873      0.06531143     0.013098982      0.0103923
#> m6.1  0.04058222 0.08707277      0.05200961     0.019841035      0.0090000
#> m7.1  0.05893782 0.12565661      0.07583040     0.027452990      0.0103923
#> m8.1  0.03823175 0.08431044      0.05840876     0.008732125      0.0090000
#> m9.1  0.07786473 0.17495976      0.10603301     0.031691219      0.0090000
#> m10.1 0.08061017 0.17467112      0.10284454     0.039488395      0.0090000
#>       AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
#> m1.2      0.0000000000       0.4341674          0  0.5000000       0.5000000
#> m2.2      0.0000000000       0.0000000          0  0.0000000       0.0000000
#> m3.2      0.0005000000       0.2600000          0  0.0000000       1.0000000
#> m4.2      0.0000000000       0.3760000          0  0.5000000       0.9574271
#> m5.2      0.0005000000       0.3606183          0  0.5773503       1.4142136
#> m6.2      0.0000000000       0.2600000          0  0.5000000       1.1547005
#> m7.2      0.0000000000       0.3760000          0  0.5773503       0.5000000
#> m8.2      0.0015000000       0.3361354          0  0.5000000       0.5000000
#> m9.2      0.0000000000       0.2600000          0  0.5000000       0.8164966
#> m10.2     0.0000000000       0.3002221          0  0.5000000       1.0000000
#> m1.1      0.0015000000       0.4422822          0  0.5000000       0.5773503
#> m2.1      0.0000000000       0.3792132          0  0.0000000       0.9574271
#> m3.1      0.0015000000       0.3760000          0  0.0000000       1.0000000
#> m4.1      0.0005773503       0.3760000          0  0.5000000       1.0000000
#> m5.1      0.0031091264       0.2211252          0  0.5773503       0.5773503
#> m6.1      0.0012583057       0.2744534          0  0.5000000       0.8164966
#> m7.1      0.0015000000       0.2211252          0  0.5773503       1.5000000
#> m8.1      0.0000000000       0.4341674          0  0.5000000       0.5000000
#> m9.1      0.0098446263       0.4381488          0  0.5000000       0.9574271
#> m10.1     0.0050000000       0.4341674          0  0.5000000       0.9574271
#>       False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
#> m1.2         0.5000000       0.8164966        1.2583057 0.3429357     NA
#> m2.2         0.0000000       0.5773503        0.5773503        NA     NA
#> m3.2         1.0000000       0.5773503        0.5773503 0.7071068     NA
#> m4.2         0.9574271       1.2909944        1.2583057 0.4578940     NA
#> m5.2         1.4142136       0.9574271        1.2583057 0.0000000     NA
#> m6.2         1.1547005       0.5000000        0.8164966 0.0000000     NA
#> m7.2         0.5000000       0.8164966        1.0000000 0.2948835     NA
#> m8.2         0.5000000       1.6329932        1.7078251 0.5960548     NA
#> m9.2         0.8164966       0.9574271        0.8164966 0.4836610     NA
#> m10.2        1.0000000       1.0000000        0.9574271 0.5773503     NA
#> m1.1         0.5773503       0.9574271        1.2909944 0.2550510     NA
#> m2.1         0.9574271       0.9574271        0.9574271 0.2302367     NA
#> m3.1         1.0000000       1.1547005        1.1547005 0.2672843     NA
#> m4.1         1.0000000       1.2583057        1.2909944 0.3227445     NA
#> m5.1         0.5773503       1.4142136        1.0000000 0.5310000     NA
#> m6.1         0.8164966       0.5000000        0.8164966 0.3510588     NA
#> m7.1         1.5000000       0.9574271        1.2909944 0.3509049     NA
#> m8.1         0.5000000       1.2909944        1.2583057 0.3552028     NA
#> m9.1         0.9574271       1.8257419        2.0615528 0.2486757     NA
#> m10.1        0.9574271       1.7320508        1.8929694 0.2357122     NA
#>       Omission_10pctSD
#> m1.2        0.02713546
#> m2.2        0.00000000
#> m3.2        0.04543494
#> m4.2        0.02713546
#> m5.2        0.03878466
#> m6.2        0.02350000
#> m7.2        0.02350000
#> m8.2        0.04750000
#> m9.2        0.02350000
#> m10.2       0.02350000
#> m1.1        0.00000000
#> m2.1        0.00000000
#> m3.1        0.00000000
#> m4.1        0.00000000
#> m5.1        0.00000000
#> m6.1        0.00000000
#> m7.1        0.00000000
#> m8.1        0.00000000
#> m9.1        0.00000000
#> m10.1       0.00000000
```

Otherwise, the mean validation metric values per algorithm can also be
obtained with the following code:

``` r
mean_validation_metrics(i)
#> $`Araucaria angustifolia`
#> # A tibble: 2 × 59
#>   algo       ROC   TSS Sensitivity Specificity `Pos Pred Value` `Neg Pred Value`
#>   <chr>    <dbl> <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
#> 1 kknn     0.966 0.874       0.969       0.906            0.945            0.952
#> 2 naive_b… 0.976 0.905       0.982       0.923            0.955            0.971
#> # ℹ 52 more variables: Precision <dbl>, Recall <dbl>, F1 <dbl>,
#> #   Prevalence <dbl>, `Detection Rate` <dbl>, `Detection Prevalence` <dbl>,
#> #   `Balanced Accuracy` <dbl>, Accuracy <dbl>, Kappa <dbl>,
#> #   AccuracyLower <dbl>, AccuracyUpper <dbl>, AccuracyNull <dbl>,
#> #   AccuracyPValue <dbl>, McnemarPValue <dbl>, Positive <dbl>, Negative <dbl>,
#> #   `True Positive` <dbl>, `False Positive` <dbl>, `True Negative` <dbl>,
#> #   `False Negative` <dbl>, CBI <dbl>, pAUC <dbl>, Omission_10pct <dbl>, …
```

After building predictions, it is possible to ensemble GCMs using
`gcms_ensembles` function and informing in the parameter `gcms` which
part of `scenarios_names(i)` should be used to ensemble gcms. In this
example, scenarios names are:
`c("ca_ssp245_2090", "ca_ssp585_2090", "mi_ssp245_2090", "mi_ssp585_2090")`.
Thus, if we set the parameter to `c("ca", "mi")` the function searches
through scenarios names for `"ca"` and `"mi"` and remove these parts of
scenarios names. What remains, in the example, is:
`c("_ssp245_2090", "_ssp585_2090", "_ssp245_2090", "_ssp585_2090")`.
Then, the function ensembles scenarios with the same new names (note
that, by removing the gcms abbreviation, the remaining name repeats
itself two times). At the end, ensembles will be named after the new
names generated in this last step and are included in object `i`
scenarios.

``` r
i <- gcms_ensembles(i, gcms = c("ca", "mi"))
#> New names:
#> New names:
#> • `cell_id` -> `cell_id...1`
#> • `mean_occ_prob` -> `mean_occ_prob...2`
#> • `wmean_AUC` -> `wmean_AUC...3`
#> • `committee_avg` -> `committee_avg...4`
#> • `cell_id` -> `cell_id...5`
#> • `mean_occ_prob` -> `mean_occ_prob...6`
#> • `wmean_AUC` -> `wmean_AUC...7`
#> • `committee_avg` -> `committee_avg...8`
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 84 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 6 
#> Predictors Names              : bio1, bio4, bio12, PC1, PC2, PC3 
#> PCA-transformed variables     : DONE 
#> Cummulative proportion ( 1 ) : PC1, PC2, PC3 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : PC1 PC2 PC3 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9660175 0.8737179     0.96895    0.905850
#> 2 naive_bayes 0.9759408 0.9047619     0.98205    0.922625
#> 
#> --------  Predictions  --------
#> Ensembles                     :
#>     Scenarios                 : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current _ssp245_2090 _ssp585_2090 
#>     Methods                   : mean_occ_prob wmean_AUC committee_avg 
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.9
```

Note that now the section “Predictions” has two scenarios called
\_ssp245_2090 and \_ssp585_2090, which are the GCM’s ensembles that we
have calculated.

### Plotting results

To plot results, we prepared plot and mapview functions. Here we present
only the plot versions due to mapview limitations for markdown, but we
encourage users to use the mapview alternatives every time it is
possible. To do that, simply alternate the “plot” portion of functions
to “mapview”. As an example, `plot_occurrences` has its counterpart
function `mapview_occurrences` with the same set of arguments an
functioning. For plot_predictions, we can set some parameters to control
what is being plotted. Probably the most important parameter is the
`scenario`, which user can change to plot every different scenario
projected. If you are modeling more than one species you can inform the
correct species to be plotted using the `spp_name` parameter and if you
are wealling to debate separate projections you can plot them informing
the model `id` (see row names of `get_validation_metrics` above to
retrieve models ids).

``` r
plot_predictions(i,
                 spp_name = NULL,
                 scenario = "current",
                 id = NULL,
                 ensemble = TRUE,
                 ensemble_type = "mean_occ_prob")
```

![](Araucaria_files/figure-html/plot_current_results-1.png)

``` r
plot_predictions(i,
                 spp_name = NULL,
                 scenario = "_ssp245_2090",
                 id = NULL,
                 ensemble = TRUE,
                 ensemble_type = "mean_occ_prob")
```

![](Araucaria_files/figure-html/ssp245_2090-1.png)

Another plot widely used in SDM studies is the Partial Dependence Plot,
which informs the response curves to each variable. Here we are using
PCA axes as predictors, so there is not much sense in plotting these
curves, but if someone want to do that, it is possible through the
`pdp_sdm` function.

``` r
pdp_sdm(i)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](Araucaria_files/figure-html/pdp_sdm-1.png)

### Writing results

To export `caretSDM` objects and outputs from R you can use the write
functions. For all possibilities see the help file
[`?write_ensembles`](https://luizesser.github.io/caretSDM/reference/write_ensembles.md).
We encourage users to use standard path configuration, which organizes
outputs in a straightforward fashion. Common functions are the
following:

``` r
write_occurrences(i, path = "results/occurrences.csv", grid = FALSE)
write_pseudoabsences(i, path = "results/pseudoabsences", ext = ".csv", centroid = FALSE)
write_grid(i, path = "results/grid_study_area.gpkg", centroid = FALSE)
write_ensembles(i, path = "results/ensembles", ext = ".tif")
```

## Conclusion

This vignette demonstrates how to build Species Distribution Models
using `caretSDM`. This vignette aimed to terrestrial uses highlights the
use of the package using a grid in the sdm_area. Alternative to that can
be seen in vignettes(“Salminus”, “caretSDM”) where we build SDMs for a
fish species using river lines in a simplefeatures object instead of
cells in a grid.

``` r
end_time <- Sys.time()
end_time - start_time
#> Time difference of 1.988463 mins
```
