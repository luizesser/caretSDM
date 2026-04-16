# 3. caretSDM Workflow for Species Distribution Modeling

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

### The `input_sdm` class

In `caretSDM` we use multiple classes to perform our analysis. Every
time we perform a new analysis, objects keep the information of what we
did. Ideally, the workflow will have only one object throughout it. The
`input_sdm` class is the key class in the workflow, where every function
will orbitate. That class puts occurrences, predictors, scenarios,
models and predictions together to perform analysis that are only
possible when two or more of these classes are available. First, we
create the object by informing the occurrences and the sdm_area. This
step assigns occurrences into a study area, excluding records outside
the study area or with NAs as predictors.

``` r
i <- input_sdm(oc, sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.
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
#> Variable Selection            : pca 
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
#> Standard deviation     205.8700 16.72893 1.56458
#> Proportion of Variance   0.9934  0.00656 0.00006
#> Cumulative Proportion    0.9934  0.99994 1.00000
get_pca_model(i)
#> Standard deviations (1, .., p=3):
#> [1] 205.869986  16.728931   1.564584
#> 
#> Rotation (n x k) = (3 x 3):
#>                PC1          PC2          PC3
#> bio1  -0.004184116 -0.006178523 -0.999972159
#> bio4   0.995998468  0.089245633 -0.004718910
#> bio12  0.089272304 -0.995990483  0.005780386
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
#> Variable Selection            : pca 
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
#> Variable Selection            : pca 
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
#> 1        kknn 0.9600632 0.8702531     0.95955      0.9106
#> 2 naive_bayes 0.9783813 0.8554804     0.97560      0.8934
```

## Post-Processing

### Predicting species distribution in given scenarios

Now that we have our models, we can make predictions in new scenarios.
The function `predict_sdm` will only predict models that passes a given
validation threshold. This validation metric is set using `metric` and
`th` arguments. In the following example, metric is set to be “ROC” and
`th` is equal 0.9. This means that only models with ROC \> 0.9 will be
used in predictions and ensembles.

``` r
i <- predict_sdm(i,
                 metric = "ROC",
                 th = 0.9,
                 tp = "prob")
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
#> Variable Selection            : pca 
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
#> 1        kknn 0.9600632 0.8702531     0.95955      0.9106
#> 2 naive_bayes 0.9783813 0.8554804     0.97560      0.8934
#> 
#> --------  Predictions  --------
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.9
```

Finally, we can ensemble the predictions using:

``` r
i <- ensemble_sdm(i,
                  method = "average")
#> Ensemble function: average
#>   current
#>   ca_ssp245_2090
#>   ca_ssp585_2090
#>   mi_ssp245_2090
#>   mi_ssp585_2090
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
#> Variable Selection            : pca 
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
#> 1        kknn 0.9600632 0.8702531     0.95955      0.9106
#> 2 naive_bayes 0.9783813 0.8554804     0.97560      0.8934
#> 
#> --------  Predictions  --------
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.9 
#> ---------  Ensembles  ---------
#> Ensembles                     :
#>     Methods                   : average
```

In the above print, it is possible to see the “Methods” under the
“Ensembles” section, which informs which ensemble types were made: mean
occurrence probability (`average`; a simple mean between GCMs), mean
occurrence probability weighted by AUC/ROC (`weighted_average`; AUC/ROC
values are used as weights), and the majority rule, or the committee
average (`committee_average`; the sum of binaries).

Besides the AUC/ROC metric, users can get every available metric by
model using the following code before commit to “ROC”:

``` r
get_validation_metrics(i)
#> $`Araucaria angustifolia`
#>              algo       ROC       TSS Sensitivity Specificity Pos Pred Value
#> m1.2         kknn 0.9703711 0.8689394     0.97500     0.89400        0.94200
#> m2.2         kknn 0.9592491 0.8576007     0.97600     0.88150        0.93150
#> m3.2         kknn 0.9299222 0.8617674     0.93875     0.92300        0.95025
#> m4.2         kknn 0.9783845 0.8512363     0.95050     0.90050        0.94000
#> m5.2         kknn 0.9691468 0.8886905     0.95100     0.93750        0.96525
#> m6.2         kknn 0.9323260 0.8742674     0.95100     0.92300        0.95325
#> m7.2         kknn 0.9619849 0.8954670     0.97550     0.92000        0.95225
#> m8.2         kknn 0.9671807 0.9207251     0.96425     0.95650        0.97600
#> m9.2         kknn 0.9798230 0.8518315     0.95100     0.90050        0.94025
#> m10.2        kknn 0.9522436 0.8320055     0.96250     0.86950        0.91800
#> m1.1  naive_bayes 0.9644187 0.8712121     1.00000     0.87125        0.93200
#> m2.1  naive_bayes 0.9729739 0.8564103     0.97500     0.88125        0.93250
#> m3.1  naive_bayes 0.9768010 0.7968864     0.93975     0.88125        0.93250
#> m4.1  naive_bayes 0.9857372 0.8714744     0.96350     0.92150        0.95450
#> m5.1  naive_bayes 0.9721726 0.8833333     0.98750     0.89600        0.94500
#> m6.1  naive_bayes 0.9895604 0.8669414     0.97550     0.90375        0.94075
#> m7.1  naive_bayes 0.9794414 0.8434982     0.98800     0.87975        0.93000
#> m8.1  naive_bayes 0.9811688 0.8326299     0.97500     0.89375        0.93850
#> m9.1  naive_bayes 0.9811355 0.8688645     0.97550     0.91825        0.95350
#> m10.1 naive_bayes 0.9804029 0.8635531     0.97625     0.88725        0.93075
#>       Neg Pred Value Precision  Recall      F1 Prevalence Detection Rate
#> m1.2         0.95850   0.94200 0.97500 0.95800    0.63550        0.62000
#> m2.2         0.95800   0.93150 0.97600 0.95325    0.62150        0.60650
#> m3.2         0.91150   0.95025 0.93875 0.94400    0.61650        0.57900
#> m4.2         0.91950   0.94000 0.95050 0.94500    0.62125        0.59075
#> m5.2         0.91800   0.96525 0.95100 0.95750    0.63050        0.60000
#> m6.2         0.92825   0.95325 0.95100 0.95075    0.61200        0.58175
#> m7.2         0.96000   0.95225 0.97550 0.96375    0.62150        0.60625
#> m8.2         0.94650   0.97600 0.96425 0.96875    0.64050        0.61700
#> m9.2         0.91950   0.94025 0.95100 0.94550    0.61650        0.58650
#> m10.2        0.94075   0.91800 0.96250 0.93975    0.60750        0.58525
#> m1.1         1.00000   0.93200 1.00000 0.96450    0.63550        0.63550
#> m2.1         0.95825   0.93250 0.97500 0.95250    0.62150        0.60575
#> m3.1         0.90300   0.93250 0.93975 0.91825    0.61650        0.57925
#> m4.1         0.93150   0.95450 0.96350 0.95025    0.62125        0.59850
#> m5.1         0.97925   0.94500 0.98750 0.96500    0.63050        0.62275
#> m6.1         0.95450   0.94075 0.97550 0.95100    0.61200        0.59700
#> m7.1         0.97725   0.93000 0.98800 0.94625    0.62100        0.61350
#> m8.1         0.95450   0.93850 0.97500 0.94650    0.64050        0.62500
#> m9.1         0.95650   0.95350 0.97550 0.95025    0.61675        0.60175
#> m10.1        0.96425   0.93075 0.97625 0.95200    0.60750        0.59275
#>       Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
#> m1.2               0.66650           0.93450  0.94600 0.88175       0.80500
#> m2.2               0.65150           0.92875  0.94025 0.87050       0.80100
#> m3.2               0.60875           0.93100  0.93275 0.86000       0.79475
#> m4.2               0.62875           0.92575  0.93150 0.85400       0.78925
#> m5.2               0.62275           0.94450  0.94650 0.88425       0.80775
#> m6.2               0.61200           0.93725  0.94000 0.87400       0.80050
#> m7.2               0.63650           0.94775  0.95475 0.90275       0.82000
#> m8.2               0.64050           0.96050  0.96075 0.91625       0.82875
#> m9.2               0.63150           0.92625  0.93200 0.85525       0.78900
#> m10.2              0.63700           0.91625  0.92600 0.84350       0.78500
#> m1.1               0.68300           0.93550  0.95325 0.89525       0.81550
#> m2.1               0.68175           0.92850  0.93950 0.86875       0.80275
#> m3.1               0.66250           0.89875  0.90250 0.79700       0.75025
#> m4.1               0.67375           0.93575  0.93900 0.87200       0.79825
#> m5.1               0.69200           0.94150  0.95425 0.89800       0.82050
#> m6.1               0.67900           0.93375  0.94000 0.87350       0.80125
#> m7.1               0.68975           0.92200  0.93175 0.85350       0.78850
#> m8.1               0.67975           0.91650  0.92975 0.84450       0.78250
#> m9.1               0.70000           0.93450  0.93900 0.87025       0.80300
#> m10.1              0.66600           0.93200  0.94075 0.87475       0.80275
#>       AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
#> m1.2        0.99400      0.63550        0.00000     0.8700000     20.5    11.75
#> m2.2        0.98950      0.62150        0.00025     0.9042500     20.5    12.50
#> m3.2        0.98300      0.61650        0.00050     0.8723333     20.5    12.75
#> m4.2        0.98600      0.62125        0.00050     1.0000000     20.5    12.50
#> m5.2        0.99300      0.63050        0.00000     1.0000000     20.5    12.00
#> m6.2        0.99300      0.61200        0.00000     0.7400000     20.5    13.00
#> m7.2        0.99575      0.62150        0.00000     1.0000000     20.5    12.50
#> m8.2        0.99450      0.64050        0.00025     0.8120000     20.5    11.50
#> m9.2        0.99000      0.61650        0.00000     1.0000000     20.5    12.75
#> m10.2       0.98225      0.60750        0.00025     1.0000000     20.5    13.25
#> m1.1        0.99550      0.63550        0.00125     0.7400000     20.5    11.75
#> m2.1        0.98625      0.62150        0.00200     0.7493333     20.5    12.50
#> m3.1        0.97625      0.61650        0.00250     0.5155000     20.5    12.75
#> m4.1        0.99125      0.62125        0.00075     0.6820000     20.5    12.50
#> m5.1        0.99300      0.63050        0.00225     0.7493333     20.5    12.00
#> m6.1        0.99150      0.61200        0.00050     0.8700000     20.5    13.00
#> m7.1        0.98850      0.62100        0.00050     0.8700000     20.5    12.50
#> m8.1        0.98775      0.64050        0.00175     0.8700000     20.5    11.50
#> m9.1        0.98625      0.61675        0.00500     0.6216667     20.5    12.75
#> m10.1       0.99150      0.60750        0.00100     0.7400000     20.5    13.25
#>       True Positive False Positive True Negative False Negative     CBI pAUC
#> m1.2          20.00           0.50         10.50           1.50 0.75675  NaN
#> m2.2          20.00           0.50         11.00           1.50 0.80400  NaN
#> m3.2          19.25           1.50         11.75           1.00 1.00000  NaN
#> m4.2          19.50           1.25         11.25           1.25 0.48500  NaN
#> m5.2          19.50           1.00         11.25           0.75 0.60000  NaN
#> m6.2          19.50           1.00         12.00           1.00 1.00000  NaN
#> m7.2          20.00           0.50         11.50           1.00 0.65800  NaN
#> m8.2          19.75           0.75         11.00           0.75 0.72350  NaN
#> m9.2          19.50           1.00         11.50           1.50 0.45650  NaN
#> m10.2         19.75           1.00         11.50           1.75 0.47350  NaN
#> m1.1          20.50           0.50         10.25           2.00 0.41075  NaN
#> m2.1          20.00           0.75         11.00           2.75 0.64800  NaN
#> m3.1          19.25           1.75         11.25           2.75 0.58075  NaN
#> m4.1          19.75           1.00         11.50           2.50 0.50275  NaN
#> m5.1          20.25           0.75         10.75           2.75 0.51325  NaN
#> m6.1          20.00           0.75         11.75           2.75 0.51100  NaN
#> m7.1          20.25           0.75         11.00           2.50 0.67900  NaN
#> m8.1          20.00           1.25         10.25           1.75 0.59850  NaN
#> m9.1          20.00           1.00         11.75           3.25 0.77950  NaN
#> m10.1         20.00           0.75         11.75           2.75 0.62825  NaN
#>       Omission_10pct       ROCSD      TSSSD SensitivitySD SpecificitySD
#> m1.2         0.04950 0.021405827 0.05315822   0.028867513   0.046303348
#> m2.2         0.06075 0.049605757 0.09271724   0.027712813   0.075053314
#> m3.2         0.06075 0.059246908 0.11975141   0.074204110   0.062870237
#> m4.2         0.07325 0.050205557 0.12031582   0.063055531   0.077315371
#> m5.2         0.06075 0.024443446 0.07854437   0.001154701   0.079902023
#> m6.2         0.07400 0.019848219 0.02202554   0.040841156   0.062870237
#> m7.2         0.06075 0.024376880 0.02852810   0.028301943   0.003464102
#> m8.2         0.01200 0.030877792 0.05910171   0.071500000   0.050335541
#> m9.2         0.09750 0.043398736 0.04703003   0.001154701   0.048500000
#> m10.2        0.07325 0.073201041 0.11441805   0.047871355   0.066455499
#> m1.1         0.09750 0.037386487 0.08913247   0.050000000   0.068607580
#> m2.1         0.09750 0.042191321 0.12624137   0.050000000   0.101279728
#> m3.1         0.09750 0.028910540 0.12672778   0.112669132   0.164190946
#> m4.1         0.09750 0.035453650 0.09245687   0.070710678   0.095520940
#> m5.1         0.09750 0.052027118 0.10715829   0.047707442   0.124889284
#> m6.1         0.09750 0.007261622 0.05937916   0.047707442   0.038500000
#> m7.1         0.09750 0.016549184 0.08461162   0.045711961   0.087548082
#> m8.1         0.09750 0.021370988 0.12994267   0.074204110   0.079378314
#> m9.1         0.09750 0.038874325 0.15280942   0.070717749   0.164228246
#> m10.1        0.09750 0.039259236 0.11965865   0.047871355   0.101500000
#>       Pos Pred ValueSD Neg Pred ValueSD PrecisionSD    RecallSD       F1SD
#> m1.2       0.025408332       0.05033554 0.025408332 0.028867513 0.02051625
#> m2.2       0.041000000       0.04883305 0.041000000 0.027712813 0.03159509
#> m3.2       0.039007478       0.10390220 0.039007478 0.074204110 0.05190376
#> m4.2       0.048541220       0.08855319 0.048541220 0.063055531 0.04980629
#> m5.2       0.043797831       0.00663325 0.043797831 0.001154701 0.02106340
#> m6.2       0.037178623       0.05463439 0.037178623 0.040841156 0.00250000
#> m7.2       0.002061553       0.04625293 0.002061553 0.028301943 0.01475071
#> m8.2       0.027712813       0.10700000 0.027712813 0.071500000 0.03253075
#> m9.2       0.023671361       0.00700000 0.023671361 0.001154701 0.01154701
#> m10.2      0.047003546       0.07430287 0.047003546 0.047871355 0.04743680
#> m1.1       0.033420303       0.09100000 0.033420303 0.050000000 0.03246023
#> m2.1       0.056050572       0.08712587 0.056050572 0.050000000 0.04469526
#> m3.1       0.077401550       0.13344506 0.077401550 0.112669132 0.03806464
#> m4.1       0.050721462       0.09442237 0.050721462 0.070710678 0.02615977
#> m5.1       0.057697487       0.07848726 0.057697487 0.047707442 0.02954093
#> m6.1       0.021422340       0.06898792 0.021422340 0.047707442 0.02207563
#> m7.1       0.034500000       0.07430287 0.034500000 0.045711961 0.02382401
#> m8.1       0.047176972       0.11782614 0.047176972 0.074204110 0.05328149
#> m9.1       0.066188493       0.09430580 0.066188493 0.070717749 0.04690682
#> m10.1      0.058654355       0.07656588 0.058654355 0.047871355 0.04463556
#>       PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
#> m1.2   0.014617341      0.031112698             0.03914397          0.02655027
#> m2.2   0.004041452      0.021361960             0.02424184          0.04614018
#> m3.2   0.014177447      0.053147593             0.04333878          0.05998333
#> m4.2   0.012579746      0.046542991             0.03719655          0.06018582
#> m5.2   0.006350853      0.006928203             0.03552816          0.03917057
#> m6.2   0.006928203      0.025927784             0.05028585          0.01105667
#> m7.2   0.004041452      0.018006943             0.01793507          0.01438460
#> m8.2   0.013178265      0.037567717             0.05303144          0.02937686
#> m9.2   0.014177447      0.014177447             0.03055596          0.02379776
#> m10.2  0.014177447      0.042405778             0.01429161          0.05740136
#> m1.1   0.008185353      0.026185238             0.03490344          0.04449251
#> m2.1   0.004041452      0.029788141             0.03826552          0.06300529
#> m3.1   0.014177447      0.072931475             0.10895680          0.06331601
#> m4.1   0.012579746      0.046821469             0.06698756          0.04657968
#> m5.1   0.006350853      0.032612625             0.06884282          0.05349766
#> m6.1   0.006928203      0.031972645             0.03950527          0.02952259
#> m7.1   0.017320508      0.028722813             0.04550000          0.04242641
#> m8.1   0.017897858      0.053382269             0.04040936          0.06484083
#> m9.1   0.007889867      0.050295792             0.07701082          0.07637190
#> m10.1  0.014177447      0.038531372             0.04242641          0.05999097
#>        AccuracySD     KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
#> m1.2  0.025729361 0.053922012     0.036013886     0.007958224    0.014617341
#> m2.2  0.041015241 0.089145948     0.053285395     0.015264338    0.004041452
#> m3.2  0.061575834 0.127417947     0.087252030     0.019061305    0.014177447
#> m4.2  0.058948424 0.120941790     0.079653416     0.023599082    0.012579746
#> m5.2  0.028722813 0.063981117     0.038733061     0.008485281    0.006350853
#> m6.2  0.001154701 0.002708013     0.002886751     0.000000000    0.006928203
#> m7.2  0.017670597 0.036863487     0.026620794     0.003774917    0.004041452
#> m8.2  0.039407064 0.081192672     0.059337313     0.009678154    0.013178265
#> m9.2  0.016500000 0.039471509     0.022075628     0.006928203    0.014177447
#> m10.2 0.056768536 0.117568420     0.076785415     0.023113849    0.014177447
#> m1.1  0.042150524 0.092312422     0.058931457     0.015253415    0.008185353
#> m2.1  0.056477134 0.121623394     0.081504090     0.026874709    0.004041452
#> m3.1  0.042936969 0.102879784     0.055210959     0.020808652    0.014177447
#> m4.1  0.035752622 0.081258846     0.044962948     0.018312109    0.012579746
#> m5.1  0.039381679 0.093114177     0.058529195     0.018997807    0.006350853
#> m6.1  0.028248894 0.061732622     0.037049516     0.012816006    0.006928203
#> m7.1  0.029159047 0.072302605     0.040566817     0.012816006    0.017320508
#> m8.1  0.065012819 0.136561036     0.091237328     0.023329166    0.017897858
#> m9.1  0.056077328 0.136255887     0.083278649     0.028241518    0.007889867
#> m10.1 0.056340483 0.121071260     0.072010995     0.028948230    0.014177447
#>       AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
#> m1.2      0.0000000000       0.2600000  0.5773503  0.5000000       1.1547005
#> m2.2      0.0005000000       0.1915000  0.5773503  0.5773503       0.0000000
#> m3.2      0.0005773503       0.2211252  0.5773503  0.5000000       1.7078251
#> m4.2      0.0010000000       0.0000000  0.5773503  0.5773503       1.7078251
#> m5.2      0.0000000000       0.0000000  0.5773503  0.0000000       0.5773503
#> m6.2      0.0000000000       0.3002221  0.5773503  0.0000000       1.0000000
#> m7.2      0.0000000000       0.0000000  0.5773503  0.5773503       0.8164966
#> m8.2      0.0005000000       0.4341674  0.5773503  0.5773503       1.2583057
#> m9.2      0.0000000000       0.0000000  0.5773503  0.5000000       0.5773503
#> m10.2     0.0005000000       0.0000000  0.5773503  0.5000000       1.5000000
#> m1.1      0.0025000000       0.3792132  0.5773503  0.5000000       1.4142136
#> m2.1      0.0040000000       0.4341674  0.5773503  0.5773503       1.4142136
#> m3.1      0.0031091264       0.4023179  0.5773503  0.5000000       2.2173558
#> m4.1      0.0015000000       0.4018104  0.5773503  0.5773503       1.9148542
#> m5.1      0.0033040379       0.4341674  0.5773503  0.0000000       1.2583057
#> m6.1      0.0005773503       0.2600000  0.5773503  0.0000000       1.2583057
#> m7.1      0.0010000000       0.3570934  0.5773503  0.5773503       0.9574271
#> m8.1      0.0028722813       0.3002221  0.5773503  0.5773503       1.7078251
#> m9.1      0.0093452305       0.3958152  0.5773503  0.5000000       1.7320508
#> m10.1     0.0014142136       0.3268286  0.5773503  0.5000000       1.5000000
#>       False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
#> m1.2         0.5773503       0.5773503        0.5773503 0.2989582     NA
#> m2.2         0.5773503       0.8164966        1.0000000 0.3394820     NA
#> m3.2         1.5000000       0.5000000        0.8164966 0.0000000     NA
#> m4.2         1.2583057       0.9574271        0.9574271        NA     NA
#> m5.2         0.0000000       0.9574271        0.9574271        NA     NA
#> m6.2         0.8164966       0.8164966        0.8164966        NA     NA
#> m7.2         0.5773503       0.5773503        0.0000000 0.4836610     NA
#> m8.2         1.5000000       0.8164966        0.5773503 0.3910300     NA
#> m9.2         0.0000000       1.0000000        0.5773503 0.1783206     NA
#> m10.2        0.9574271       0.5773503        0.9574271 0.3339975     NA
#> m1.1         1.0000000       0.9574271        0.8164966 0.1453742     NA
#> m2.1         1.0000000       1.1547005        1.2909944 0.3710750     NA
#> m3.1         2.3629078       2.3094011        2.0615528 0.4894714     NA
#> m4.1         1.4142136       0.8164966        1.2909944 0.3448926     NA
#> m5.1         0.9574271       1.5000000        1.5000000 0.3371324     NA
#> m6.1         0.9574271       0.5000000        0.5000000 0.3415855     NA
#> m7.1         0.9574271       1.4142136        1.0000000 0.3722419     NA
#> m8.1         1.5000000       0.5000000        0.9574271 0.2688302     NA
#> m9.1         1.4142136       2.3804761        1.8929694 0.2591287     NA
#> m10.1        1.0000000       1.0000000        1.5000000 0.2947206     NA
#>       Omission_10pctSD
#> m1.2       0.040837075
#> m2.2       0.022852790
#> m3.2       0.046785860
#> m4.2       0.040837075
#> m5.2       0.022852790
#> m6.2       0.030022214
#> m7.2       0.046785860
#> m8.2       0.024000000
#> m9.2       0.028087660
#> m10.2      0.028087660
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
#> 1 kknn     0.960 0.870       0.960       0.911            0.947            0.936
#> 2 naive_b… 0.978 0.855       0.976       0.893            0.939            0.958
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
#> • `average` -> `average...2`
#> • `cell_id` -> `cell_id...3`
#> • `average` -> `average...4`
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
#> Variable Selection            : pca 
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
#> 1        kknn 0.9600632 0.8702531     0.95955      0.9106
#> 2 naive_bayes 0.9783813 0.8554804     0.97560      0.8934
#> 
#> --------  Predictions  --------
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.9 
#> ---------  Ensembles  ---------
#> Ensembles                     :
#>     Methods                   : average
```

Note that now the “Ensembles” has two scenarios called \_ssp245_2090 and
\_ssp585_2090, which are the GCM’s ensembles that we have calculated.

### Plotting results

To plot results, we prepared plot and mapview functions. Here we present
only the plot versions due to mapview limitations for markdown, but we
encourage users to use the mapview alternatives every time it is
possible. To do that, simply alternate the “plot” portion of functions
to “mapview”. As an example, `plot_occurrences` has its counterpart
function `mapview_occurrences` with the same set of arguments an
functioning. For plot_ensembles, we can set some parameters to control
what is being plotted. Probably the most important parameter is the
`scenario`, which user can change to plot every different scenario
projected. If you are modeling more than one species you can inform the
correct species to be plotted using the `spp_name` parameter and if you
are wealling to debate separate projections you can plot them informing
the model `id` (see row names of `get_validation_metrics` above to
retrieve models ids).

``` r
plot_ensembles(i,
               scenario = "current",
               ensemble_type = "average")
```

![](Araucaria_files/figure-html/plot_current_results-1.png)

``` r
plot_ensembles(i,
               scenario = "_ssp245_2090",
               ensemble_type = "average")
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
#> Time difference of 23.70356 secs
```
