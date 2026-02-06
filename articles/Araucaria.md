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
#>                    species decimalLongitude decimalLatitude
#> 327 Araucaria angustifolia         -4700678        -3065133
#> 405 Araucaria angustifolia         -4711827        -3146727
#> 404 Araucaria angustifolia         -4711885        -3147170
#> 310 Araucaria angustifolia         -4717665        -3142767
#> 49  Araucaria angustifolia         -4726011        -3148963
#> 124 Araucaria angustifolia         -4727265        -3148517
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
#> Number of presences   : 419 
#> =================================
#> Data:
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -4727265 ymin: -3148963 xmax: -4700678 ymax: -3065133
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#>                    species                  geometry
#> 327 Araucaria angustifolia POINT (-4700678 -3065133)
#> 405 Araucaria angustifolia POINT (-4711827 -3146727)
#> 404 Araucaria angustifolia POINT (-4711885 -3147170)
#> 310 Araucaria angustifolia POINT (-4717665 -3142767)
#> 49  Araucaria angustifolia POINT (-4726011 -3148963)
#> 124 Araucaria angustifolia POINT (-4727265 -3148517)
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
#> Number of presences           : 417 
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
#> Removed 0 records.
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
#> Number of presences           : 82 
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
#> Number of presences           : 82 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 82 
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
#> Number of presences           : 82 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 82 
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
#> 1        kknn 0.9644196 0.8776677    0.963475    0.917025
#> 2 naive_bayes 0.9781148 0.9006589    0.981550    0.920225
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
#> Number of presences           : 82 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 82 
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
#> 1        kknn 0.9644196 0.8776677    0.963475    0.917025
#> 2 naive_bayes 0.9781148 0.9006589    0.981550    0.920225
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
#> m1.2         kknn 0.9537698 0.8678571     0.95125     0.91675        0.95475
#> m2.2         kknn 0.9774038 0.8902930     0.96300     0.94500        0.96375
#> m3.2         kknn 0.9305174 0.8125916     0.93975     0.88450        0.92825
#> m4.2         kknn 0.9603251 0.8986722     0.97550     0.92300        0.95450
#> m5.2         kknn 0.9826465 0.9236264     0.96350     0.96000        0.97550
#> m6.2         kknn 0.9788004 0.9059982     0.96350     0.94225        0.96525
#> m7.2         kknn 0.9577915 0.8028388     0.96300     0.83950        0.90950
#> m8.2         kknn 0.9770681 0.8960623     0.97625     0.92000        0.95225
#> m9.2         kknn 0.9590233 0.8992965     0.96350     0.93550        0.96525
#> m10.2        kknn 0.9668498 0.8794414     0.97550     0.90375        0.94150
#> m1.1  naive_bayes 0.9619544 0.8422619     0.98800     0.85425        0.93100
#> m2.1  naive_bayes 0.9802427 0.9027930     0.95100     0.96300        0.97475
#> m3.1  naive_bayes 0.9712912 0.8846154     1.00000     0.88450        0.93350
#> m4.1  naive_bayes 0.9688645 0.8986722     0.97550     0.92300        0.95275
#> m5.1  naive_bayes 0.9875000 0.9365385     0.97500     0.96150        0.97500
#> m6.1  naive_bayes 0.9832189 0.9496337     0.98800     0.96150        0.97675
#> m7.1  naive_bayes 0.9846383 0.8657051     0.98750     0.87825        0.93450
#> m8.1  naive_bayes 0.9816163 0.9089744     0.98750     0.92150        0.95375
#> m9.1  naive_bayes 0.9935065 0.9193182     0.98800     0.93175        0.96875
#> m10.1 naive_bayes 0.9683150 0.8980769     0.97500     0.92300        0.95175
#>       Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
#> m1.2         0.91875   0.95475 0.95125 0.95200    0.63050        0.59975
#> m2.2         0.94900   0.96375 0.96300 0.95750    0.60750        0.58475
#> m3.2         0.90650   0.92825 0.93975 0.92775    0.61200        0.57475
#> m4.2         0.96150   0.95450 0.97550 0.96450    0.61200        0.59700
#> m5.2         0.94225   0.97550 0.96350 0.96950    0.62125        0.59875
#> m6.2         0.94525   0.96525 0.96350 0.96350    0.61200        0.58950
#> m7.2         0.93525   0.90950 0.96300 0.93525    0.62125        0.59850
#> m8.2         0.96150   0.95225 0.97625 0.96400    0.62100        0.60600
#> m9.2         0.93725   0.96525 0.96350 0.96350    0.64050        0.61725
#> m10.2        0.96000   0.94150 0.97550 0.95825    0.61200        0.59700
#> m1.1         0.98075   0.93100 0.98800 0.95575    0.63050        0.62300
#> m2.1         0.91450   0.97475 0.95100 0.95650    0.60750        0.57775
#> m3.1         1.00000   0.93350 1.00000 0.96550    0.61200        0.61200
#> m4.1         0.96150   0.95275 0.97550 0.96350    0.61200        0.59700
#> m5.1         0.96150   0.97500 0.97500 0.97500    0.62125        0.60600
#> m6.1         0.98225   0.97675 0.98800 0.98225    0.61200        0.60450
#> m7.1         0.97925   0.93450 0.98750 0.95950    0.62125        0.61350
#> m8.1         0.98075   0.95375 0.98750 0.97025    0.62125        0.61375
#> m9.1         0.98075   0.96875 0.98800 0.97675    0.64050        0.63275
#> m10.1        0.96150   0.95175 0.97500 0.96325    0.61200        0.59675
#>       Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
#> m1.2               0.63025           0.93375  0.93900 0.86775       0.80150
#> m2.2               0.61400           0.94525  0.94825 0.89100       0.81375
#> m3.2               0.62725           0.90650  0.91075 0.81225       0.76200
#> m4.2               0.62650           0.94925  0.95525 0.90450       0.82525
#> m5.2               0.63600           0.96200  0.96225 0.92025       0.83350
#> m6.2               0.62725           0.95300  0.95525 0.90525       0.82250
#> m7.2               0.65875           0.90175  0.91650 0.81925       0.76775
#> m8.2               0.63625           0.94825  0.95475 0.90300       0.82075
#> m9.2               0.64075           0.94975  0.95350 0.89725       0.81500
#> m10.2              0.63400           0.94000  0.94800 0.88900       0.81250
#> m1.1               0.70025           0.92100  0.93800 0.85625       0.80275
#> m2.1               0.65900           0.95125  0.94850 0.89325       0.81525
#> m3.1               0.65675           0.94275  0.95575 0.90350       0.82350
#> m4.1               0.67175           0.94950  0.95475 0.90450       0.82400
#> m5.1               0.65200           0.96825  0.96975 0.93650       0.85000
#> m6.1               0.65700           0.97500  0.97800 0.95300       0.85750
#> m7.1               0.70375           0.93275  0.94700 0.88275       0.81150
#> m8.1               0.66675           0.95450  0.96225 0.91925       0.83325
#> m9.1               0.70325           0.95975  0.96875 0.92800       0.84250
#> m10.1              0.65675           0.94925  0.95500 0.90475       0.82600
#>       AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
#> m1.2        0.98500      0.63050        0.00175     0.8266667     20.5    12.00
#> m2.2        0.99300      0.60750        0.00000     0.8700000     20.5    13.25
#> m3.2        0.97825      0.61200        0.00050     0.8085000     20.5    13.00
#> m4.2        0.99325      0.61200        0.00000     0.8266667     20.5    13.00
#> m5.2        0.99625      0.62125        0.00025     1.0000000     20.5    12.50
#> m6.2        0.99600      0.61200        0.00000     0.8700000     20.5    13.00
#> m7.2        0.98325      0.62125        0.00025     0.7742500     20.5    12.50
#> m8.2        0.99450      0.62100        0.00100     1.0000000     20.5    12.50
#> m9.2        0.99550      0.64050        0.00000     0.8700000     20.5    11.50
#> m10.2       0.99300      0.61200        0.00000     1.0000000     20.5    13.00
#> m1.1        0.98150      0.63050        0.04575     0.6997500     20.5    12.00
#> m2.1        0.99100      0.60750        0.00650     0.9042500     20.5    13.25
#> m3.1        0.99450      0.61200        0.00025     0.8120000     20.5    13.00
#> m4.1        0.99475      0.61200        0.00050     0.8266667     20.5    13.00
#> m5.1        0.99150      0.62125        0.01175     1.0000000     20.5    12.50
#> m6.1        0.99925      0.61200        0.00250     1.0000000     20.5    13.00
#> m7.1        0.99150      0.62125        0.02725     0.5790000     20.5    12.50
#> m8.1        0.99625      0.62125        0.00075     0.8266667     20.5    12.50
#> m9.1        0.99475      0.64050        0.00175     0.6877500     20.5    11.50
#> m10.1       0.99100      0.61200        0.00075     1.0000000     20.5    13.00
#>       True Positive False Positive True Negative False Negative       CBI pAUC
#> m1.2          19.50           1.00         11.00           1.00 0.5966667  NaN
#> m2.2          19.75           1.25         12.50           1.00 1.0000000  NaN
#> m3.2          19.25           1.50         11.50           1.75 0.7060000  NaN
#> m4.2          20.00           0.50         12.00           1.00 0.8206667  NaN
#> m5.2          19.75           1.00         12.00           1.50 0.5147500  NaN
#> m6.2          19.75           0.75         12.25           1.25 0.5790000  NaN
#> m7.2          19.75           0.75         10.50           2.00 0.6780000  NaN
#> m8.2          20.00           1.25         11.50           1.25 0.9333333  NaN
#> m9.2          19.75           0.75         10.75           0.75 1.0000000  NaN
#> m10.2         20.00           0.50         11.75           1.25 0.7527500  NaN
#> m1.1          20.25           0.75         10.25           3.00 0.6755000  NaN
#> m2.1          19.50           1.25         12.75           2.75 0.7932500  NaN
#> m3.1          20.50           0.25         11.50           1.75 0.5507500  NaN
#> m4.1          20.00           0.75         12.00           2.75 0.8160000  NaN
#> m5.1          20.00           1.50         12.00           2.50 0.8695000  NaN
#> m6.1          20.25           1.25         12.50           2.75 0.8602500  NaN
#> m7.1          20.25           0.75         11.00           3.50 0.6232500  NaN
#> m8.1          20.25           0.50         11.50           2.00 0.7967500  NaN
#> m9.1          20.25           0.25         10.75           2.25 1.0000000  NaN
#> m10.1         20.00           0.75         12.00           2.25 0.8602500  NaN
#>       Omission_10pct      ROCSD      TSSSD SensitivitySD SpecificitySD
#> m1.2         0.09750 0.08432796 0.14864594    0.05632865    0.11785125
#> m2.2         0.09750 0.05683037 0.08669642    0.04805552    0.10092035
#> m3.2         0.06125 0.06596473 0.08603035    0.06120457    0.07700000
#> m4.2         0.08500 0.04502251 0.09384410    0.02830194    0.08891194
#> m5.2         0.08575 0.02927222 0.12315167    0.02435159    0.10893576
#> m6.2         0.09750 0.01904901 0.06535552    0.02435159    0.07372189
#> m7.2         0.08500 0.04481667 0.06542582    0.02468468    0.06331666
#> m8.2         0.08500 0.05465961 0.09758478    0.07144462    0.04409082
#> m9.2         0.06150 0.02139882 0.06102089    0.02435159    0.08068251
#> m10.2        0.08575 0.03795321 0.05886284    0.02830194    0.03850000
#> m1.1         0.09750 0.05285415 0.28293289    0.04770744    0.23938585
#> m2.1         0.09750 0.06140340 0.20337608    0.05981290    0.16694410
#> m3.1         0.09750 0.05976760 0.13012688    0.02400000    0.11550000
#> m4.1         0.09750 0.04026733 0.06884650    0.02830194    0.06287024
#> m5.1         0.09750 0.05192722 0.15248794    0.05067544    0.10778373
#> m6.1         0.09750 0.03360011 0.11893800    0.05981290    0.13150253
#> m7.1         0.09750 0.09299259 0.23658812    0.04571196    0.20332404
#> m8.1         0.09750 0.02740366 0.12105459    0.02830194    0.09302105
#> m9.1         0.09750 0.02716026 0.13853765    0.02500000    0.13650000
#> m10.1        0.09750 0.05622919 0.15084819    0.05000000    0.13150253
#>       Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
#> m1.2        0.06411123       0.09516083  0.06411123 0.05632865 0.05025933
#> m2.2        0.06128825       0.06602209  0.06128825 0.04805552 0.03612017
#> m3.2        0.04288259       0.08493331  0.04288259 0.06120457 0.03474071
#> m4.2        0.05264029       0.04472509  0.05264029 0.02830194 0.03103224
#> m5.2        0.06412488       0.04165233  0.06412488 0.02435159 0.04054935
#> m6.2        0.04442597       0.03881044  0.04442597 0.02435159 0.02000625
#> m7.2        0.03282783       0.04366826  0.03282783 0.02468468 0.02132878
#> m8.2        0.02278889       0.11164639  0.02278889 0.07144462 0.04248431
#> m9.2        0.04379783       0.04222460  0.04379783 0.02435159 0.01330413
#> m10.2       0.02176388       0.04625293  0.02176388 0.02830194 0.02260347
#> m1.1        0.11011623       0.13484188  0.11011623 0.04770744 0.08331666
#> m2.1        0.09550567       0.08334867  0.09550567 0.05981290 0.06959107
#> m3.1        0.06425989       0.04550000  0.06425989 0.02400000 0.04163632
#> m4.1        0.03721447       0.04445597  0.03721447 0.02830194 0.02435159
#> m5.1        0.06089540       0.09356638  0.06089540 0.05067544 0.05355994
#> m6.1        0.06320865       0.08205892  0.06320865 0.05981290 0.03389567
#> m7.1        0.09573227       0.11785125  0.09573227 0.04571196 0.07023473
#> m8.1        0.05001583       0.05525924  0.05001583 0.02830194 0.04016113
#> m9.1        0.06250000       0.05550000  0.06250000 0.02500000 0.03897328
#> m10.1       0.07325924       0.07700000  0.07325924 0.05000000 0.05138985
#>       PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
#> m1.2   0.006350853       0.03624339             0.04962106          0.07446867
#> m2.2   0.014177447       0.04055449             0.05879342          0.04316152
#> m3.2   0.006928203       0.03289757             0.04825281          0.04309292
#> m4.2   0.006928203       0.01865476             0.03848376          0.04692814
#> m5.2   0.012579746       0.02140677             0.03260879          0.06134805
#> m6.2   0.006928203       0.01236932             0.04054935          0.03264327
#> m7.2   0.012579746       0.02609598             0.03894761          0.03281641
#> m8.2   0.017320508       0.02915905             0.03000000          0.04905099
#> m9.2   0.017897858       0.01550000             0.04040936          0.03028063
#> m10.2  0.006928203       0.01865476             0.01937352          0.02976575
#> m1.1   0.006350853       0.03261263             0.09321838          0.14146466
#> m2.1   0.014177447       0.03358943             0.03961481          0.10173945
#> m3.1   0.006928203       0.01236932             0.03616052          0.06499744
#> m4.1   0.006928203       0.01982423             0.02709705          0.03435598
#> m5.1   0.012579746       0.04133602             0.02325224          0.07650000
#> m6.1   0.006928203       0.03289757             0.07277362          0.05936539
#> m7.1   0.012579746       0.03730058             0.06724272          0.11825115
#> m8.1   0.012579746       0.02623452             0.02986079          0.06052203
#> m9.1   0.013178265       0.02709705             0.06633438          0.06935176
#> m10.1  0.006928203       0.03552816             0.05203444          0.07546467
#>       AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
#> m1.2  0.06573685 0.14382715      0.08992775     0.024262454    0.006350853
#> m2.2  0.04197122 0.08533219      0.05766209     0.015264338    0.014177447
#> m3.2  0.04221670 0.08876326      0.05687999     0.015435349    0.006928203
#> m4.2  0.03912693 0.08341263      0.05874450     0.008732125    0.006928203
#> m5.2  0.05187405 0.11210226      0.07413726     0.015968719    0.012579746
#> m6.2  0.02532456 0.05392819      0.03680919     0.007549834    0.006928203
#> m7.2  0.02776689 0.06110306      0.03639025     0.012446552    0.012579746
#> m8.2  0.05253887 0.11177768      0.07159143     0.019052559    0.017320508
#> m9.2  0.01789786 0.04129064      0.02655811     0.004041452    0.017897858
#> m10.2 0.02822528 0.06058052      0.03966947     0.008485281    0.006928203
#> m1.1  0.11813940 0.27698436      0.14506407     0.064406910    0.006350853
#> m2.1  0.09071751 0.19644062      0.11210263     0.048689492    0.014177447
#> m3.1  0.05423637 0.11833392      0.07860609     0.015798207    0.006928203
#> m4.1  0.03018140 0.06369982      0.04872371     0.008660254    0.006928203
#> m5.1  0.06913513 0.15048893      0.08803408     0.038134630    0.012579746
#> m6.1  0.04696807 0.10949734      0.05852065     0.025013330    0.006928203
#> m7.1  0.09931767 0.22945297      0.11855062     0.057116110    0.012579746
#> m8.1  0.05290479 0.11631280      0.07214049     0.019070046    0.012579746
#> m9.1  0.05427707 0.12617811      0.07370380     0.019629909    0.013178265
#> m10.1 0.06559662 0.14142371      0.09432700     0.021710213    0.006928203
#>       AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
#> m1.2      0.0035000000       0.3002221  0.5773503  0.0000000       1.2909944
#> m2.2      0.0000000000       0.3792132  0.5773503  0.5000000       1.5000000
#> m3.2      0.0005773503       0.2211252  0.5773503  0.0000000       0.9574271
#> m4.2      0.0000000000       0.3002221  0.5773503  0.0000000       0.8164966
#> m5.2      0.0005000000       0.2211252  0.5773503  0.5773503       0.5773503
#> m6.2      0.0000000000       0.2600000  0.5773503  0.0000000       0.5000000
#> m7.2      0.0005000000       0.2666063  0.5773503  0.5773503       0.9574271
#> m8.2      0.0011547005       0.1915000  0.5773503  0.5773503       0.9574271
#> m9.2      0.0000000000       0.2600000  0.5773503  0.5773503       0.5000000
#> m10.2     0.0000000000       0.0000000  0.5773503  0.0000000       0.8164966
#> m1.1      0.0901715957       0.5536789  0.5773503  0.0000000       1.2583057
#> m2.1      0.0123423391       0.3430262  0.5773503  0.5000000       1.0000000
#> m3.1      0.0005000000       0.3760217  0.5773503  0.0000000       0.5773503
#> m4.1      0.0005773503       0.3070293  0.5773503  0.0000000       0.9574271
#> m5.1      0.0228381990              NA  0.5773503  0.5773503       1.4142136
#> m6.1      0.0036968455       0.1866929  0.5773503  0.0000000       0.9574271
#> m7.1      0.0531750255       0.4865237  0.5773503  0.5773503       0.9574271
#> m8.1      0.0009574271       0.3002221  0.5773503  0.5773503       0.9574271
#> m9.1      0.0023629078       0.5317443  0.5773503  0.5773503       0.9574271
#> m10.1     0.0009574271       0.4033762  0.5773503  0.0000000       1.5000000
#>       False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
#> m1.2         1.1547005       1.4142136        1.4142136 0.5527118     NA
#> m2.2         0.9574271       0.9574271        1.4142136 0.0000000     NA
#> m3.2         1.2909944       1.0000000        1.0000000 0.4801255     NA
#> m4.2         0.5773503       1.1547005        1.1547005 0.3106144     NA
#> m5.2         0.5000000       1.2909944        1.4142136 0.5002253     NA
#> m6.2         0.5000000       0.9574271        0.9574271 0.2937641     NA
#> m7.2         0.5000000       1.0000000        0.8164966 0.2518908     NA
#> m8.2         1.5000000       0.9574271        0.5000000 0.4611705     NA
#> m9.2         0.5000000       0.9574271        0.9574271        NA     NA
#> m10.2        0.5773503       0.5000000        0.5000000 0.2894678     NA
#> m1.1         0.9574271       2.8722813        2.8722813 0.3939556     NA
#> m2.1         1.2583057       1.9148542        2.3629078 0.4135000     NA
#> m3.1         0.5000000       1.5000000        1.5000000 0.3576367     NA
#> m4.1         0.5773503       0.8164966        0.8164966 0.2124649     NA
#> m5.1         1.0000000       1.6329932        1.2909944 0.2610000     NA
#> m6.1         1.2583057       1.7078251        1.7078251 0.2795000     NA
#> m7.1         0.9574271       2.4494897        2.6457513 0.3307682     NA
#> m8.1         0.5773503       1.2909944        1.1547005 0.2502843     NA
#> m9.1         0.5000000       1.8929694        1.5000000 0.3389346     NA
#> m10.1        1.0000000       1.7078251        1.7078251 0.3016713     NA
#>       Omission_10pctSD
#> m1.2       0.025276801
#> m2.2       0.049223131
#> m3.2       0.046614554
#> m4.2       0.023452079
#> m5.2       0.028087660
#> m6.2       0.002886751
#> m7.2       0.025980762
#> m8.2       0.025980762
#> m9.2       0.025683977
#> m10.2      0.025276801
#> m1.1       0.002886751
#> m2.1       0.002886751
#> m3.1       0.002886751
#> m4.1       0.002886751
#> m5.1       0.002886751
#> m6.1       0.002886751
#> m7.1       0.002886751
#> m8.1       0.002886751
#> m9.1       0.002886751
#> m10.1      0.002886751
```

Otherwise, the mean validation metric values per algorithm can also be
obtained with the following code:

``` r
mean_validation_metrics(i)
#> $`Araucaria angustifolia`
#> # A tibble: 2 × 59
#>   algo       ROC   TSS Sensitivity Specificity `Pos Pred Value` `Neg Pred Value`
#>   <chr>    <dbl> <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
#> 1 kknn     0.964 0.878       0.963       0.917            0.951            0.942
#> 2 naive_b… 0.978 0.901       0.982       0.920            0.955            0.970
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
#> Number of presences           : 82 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 82 
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
#> 1        kknn 0.9644196 0.8776677    0.963475    0.917025
#> 2 naive_bayes 0.9781148 0.9006589    0.981550    0.920225
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
#> Time difference of 2.043002 mins
```
