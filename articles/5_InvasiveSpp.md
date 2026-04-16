# 5. Projecting Non-native Distribution using SDMs

## Introduction

One of the main uses for Species Distribution Models is to project the
impact of invasive species. To facilitate the use of `caretSDM` in
invasiveness assessments, we put this document together. Here, we will
build the Ecological Niche Model, i.e. the mathematical model that
describes the hypervolume that encompasses the limiting values
permitting a species to persist through time, for *Araucaria
angustifolia* using records on Paraná. We then project the models into
the Rio Grande do Sul state, further to the South, to assess the
invasiveness potential of the species.

This article may read repetitive, when comparing with others, but our
aim here is to have a complete tutorial, thus some steps will be exactly
as from other articles.

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

As we are aiming for a climate change assessment, we will also download
future data.

``` r
WorldClim_data(path = NULL, 
               period = "future", 
               variable = "bioc",
               year = "2090",
               gcm = c("ca", "mi"),
               ssp = c("245","585"),
               resolution = 10)
```

#### KEY CHANGE FOR INVASIVENESS ASSESSMENT:

In caretSDM, we will set the study area to match the region where the
model will be trained. The region where models will be projected can be
adapted from scenarios data. One can work around with this environmental
data to adapt it using any package. We already have included in the
package an object with current and future data for the Rio Grande do Sul
state. To build it, we used an adaptation of the following code:

``` r
# THIS IS AN EXAMPLE CODE, IT MUST BE ADAPTED TO RUN CORRECTLY!!!
# Import aim area shape
rs <- st_read("Rio_Grande_do_Sul.shp")

# Import current data
current <- read_stars(list.files("WorldClim_data_current_folder", full.names=T), along = "band")

# Change name of current scenario
names(current) <- "current"

# Select variables and crop using the shape of the aim area
current <- sf::st_crop(current[,,,c("1.tif", "4.tif", "12.tif")], rs)

# Set names of variables to match future scenario
current <- stars::st_set_dimensions(current, "band", values = c("bio1", "bio4","bio12"))

# Import future data
future <- read_stars(list.files("WorldClim_data_future_folder", full.names=T))

# Change names of future scenarios
names(future) <- c("ca_ssp245_2090", "ca_ssp585_2090", 
                   "mi_ssp245_2090", "mi_ssp585_2090")

# Select variables and crop using the shape of the aim area
future <- sf::st_crop(future[,,,c("bio01", "bio04", "bio12")], rs)

# Set names of variables to match current scenario
future <- stars::st_set_dimensions(future, "band", values = c("bio1", "bio4","bio12"))

# Sum scenarios
scen_rs <- c(current, future)
```

The result should resemble the included object (see
[`?scen_rs`](https://luizesser.github.io/caretSDM/reference/scen_rs.md)
for more information on the data).:

``` r
scen_rs
#> stars object with 3 dimensions and 5 attributes
#> attribute(s):
#>                     Min. 1st Qu.   Median     Mean  3rd Qu.   Max. NA's
#> current         14.38403 19.5185 397.1582 668.0422 1467.000 2079.0 3204
#> ca_ssp245_2090  17.60000 23.0000 414.1000 677.0502 1516.225 2134.2 3264
#> ca_ssp585_2090  20.70000 26.6000 446.2500 688.5646 1551.125 2098.2 3264
#> mi_ssp245_2090  15.70000 20.8000 407.8500 721.1667 1608.125 2284.4 3264
#> mi_ssp585_2090  16.90000 22.1000 407.7500 754.2061 1679.425 2439.8 3264
#> dimension(s):
#>      from  to offset   delta refsys point              values x/y
#> x     735 782   -180  0.1667 WGS 84 FALSE                NULL [x]
#> y     703 743     90 -0.1667 WGS 84 FALSE                NULL [y]
#> band    1   3     NA      NA     NA    NA bio1 , bio4 , bio12
```

### Defining the study area

A important step on model building in Species Distribution Models, is
the definition of accessible area (the M in BAM diagram). This area can
be, in Geographical Information Systems terms, as an example, the
delimitation of a habitat (polygon) or a river basin network (lines).
Another broadly used approach is the use of buffers around presences.
The buffer size translates the potential distribution capabilities of a
species. As our invasiveness assessment aims to build models in the
Parana state and project in the Rio Grande do Sul state, we will use a
simple polygon of Parana state boundaries as study area. This shape is
available in caretSDM as the `parana` object (see
[`?parana`](https://luizesser.github.io/caretSDM/reference/parana.md)
for more information on the data).

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

![](5_InvasiveSpp_files/figure-html/parana_view-1.png)

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

![](5_InvasiveSpp_files/figure-html/plot_sdm_area-1.png)

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
because the argument `pred_as_scen` is standarly set to `TRUE`. However,
in invasiveness assessments, we want to project models into another
region. This region was previously set as the `scen_rs` object.

``` r
sa <- add_scenarios(sa, 
                    scen = scen_rs, 
                    scenarios_names = NULL,
                    pred_as_scen = FALSE,
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
#> Scenarios Names          : current, ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090
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

![](5_InvasiveSpp_files/figure-html/plot_occurrences-1.png)

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
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090
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
                    variables_selected = "vif",
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090
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
               variables_selected = "vif", 
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : bio1 bio12 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9788004 0.9407967     0.98775    0.953000
#> 2 naive_bayes 0.9965948 0.9482630     0.99265    0.960525
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : bio1 bio12 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9788004 0.9407967     0.98775    0.953000
#> 2 naive_bayes 0.9965948 0.9482630     0.99265    0.960525
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : bio1 bio12 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9788004 0.9407967     0.98775    0.953000
#> 2 naive_bayes 0.9965948 0.9482630     0.99265    0.960525
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
#>                         algo       ROC       TSS Sensitivity Specificity
#> kknn_pa1                kknn 0.9721726 0.9255952      0.9880     0.93750
#> kknn_pa2                kknn 0.9662775 0.9206502      0.9755     0.94500
#> kknn_pa3                kknn 0.9798535 0.9615385      1.0000     0.96150
#> kknn_pa4                kknn 0.9798077 0.9423077      1.0000     0.94225
#> kknn_pa5                kknn 0.9769002 0.8970696      0.9755     0.92150
#> kknn_pa6                kknn 0.9818452 0.9490385      0.9875     0.96150
#> kknn_pa7                kknn 0.9778846 0.9349359      0.9750     0.96000
#> kknn_pa8                kknn 0.9899038 0.9688645      0.9880     0.98075
#> kknn_pa9                kknn 0.9885417 0.9583333      1.0000     0.95850
#> kknn_pa10               kknn 0.9748168 0.9496337      0.9880     0.96150
#> naive_bayes_pa1  naive_bayes 0.9888393 0.9250000      1.0000     0.93750
#> naive_bayes_pa2  naive_bayes 1.0000000 0.9458333      0.9755     0.98225
#> naive_bayes_pa3  naive_bayes 0.9963370 0.9496337      0.9880     0.96150
#> naive_bayes_pa4  naive_bayes 0.9923535 0.9298077      0.9875     0.94225
#> naive_bayes_pa5  naive_bayes 1.0000000 0.9541667      1.0000     0.97925
#> naive_bayes_pa6  naive_bayes 0.9990385 0.9496337      0.9880     0.96150
#> naive_bayes_pa7  naive_bayes 0.9954212 0.9423077      1.0000     0.94225
#> naive_bayes_pa8  naive_bayes 1.0000000 0.9807692      1.0000     0.98075
#> naive_bayes_pa9  naive_bayes 0.9968434 0.9564394      1.0000     0.95650
#> naive_bayes_pa10 naive_bayes 0.9971154 0.9490385      0.9875     0.96150
#>                  Pos Pred Value Neg Pred Value Precision Recall      F1
#> kknn_pa1                0.96525        0.97925   0.96525 0.9880 0.97600
#> kknn_pa2                0.96600        0.96450   0.96600 0.9755 0.96975
#> kknn_pa3                0.97675        1.00000   0.97675 1.0000 0.98825
#> kknn_pa4                0.96625        1.00000   0.96625 1.0000 0.98275
#> kknn_pa5                0.95400        0.96000   0.95400 0.9755 0.96375
#> kknn_pa6                0.97675        0.98225   0.97675 0.9875 0.98175
#> kknn_pa7                0.97750        0.96300   0.97750 0.9750 0.97550
#> kknn_pa8                0.98800        0.98075   0.98800 0.9880 0.98800
#> kknn_pa9                0.97600        1.00000   0.97600 1.0000 0.98800
#> kknn_pa10               0.97600        0.98075   0.97600 0.9880 0.98200
#> naive_bayes_pa1         0.96600        1.00000   0.96600 1.0000 0.97575
#> naive_bayes_pa2         0.98800        0.96450   0.98800 0.9755 0.97550
#> naive_bayes_pa3         0.97825        0.98225   0.97825 0.9880 0.98275
#> naive_bayes_pa4         0.96425        0.98075   0.96425 0.9875 0.97575
#> naive_bayes_pa5         0.98875        1.00000   0.98875 1.0000 0.98275
#> naive_bayes_pa6         0.97725        0.98225   0.97725 0.9880 0.98200
#> naive_bayes_pa7         0.96875        1.00000   0.96875 1.0000 0.98325
#> naive_bayes_pa8         0.98800        1.00000   0.98800 1.0000 0.99400
#> naive_bayes_pa9         0.97750        1.00000   0.97750 1.0000 0.98850
#> naive_bayes_pa10        0.97725        0.98225   0.97725 0.9875 0.98150
#>                  Prevalence Detection Rate Detection Prevalence
#> kknn_pa1            0.63050        0.62300              0.64625
#> kknn_pa2            0.60750        0.59250              0.61450
#> kknn_pa3            0.61200        0.61200              0.62675
#> kknn_pa4            0.61200        0.61200              0.63400
#> kknn_pa5            0.62125        0.60600              0.63600
#> kknn_pa6            0.61200        0.60450              0.61925
#> kknn_pa7            0.62125        0.60600              0.62100
#> kknn_pa8            0.62100        0.61350              0.62100
#> kknn_pa9            0.64050        0.64050              0.65600
#> kknn_pa10           0.61200        0.60450              0.61950
#> naive_bayes_pa1     0.63050        0.63050              0.66950
#> naive_bayes_pa2     0.60750        0.59250              0.60725
#> naive_bayes_pa3     0.61200        0.60450              0.61900
#> naive_bayes_pa4     0.61200        0.60450              0.62675
#> naive_bayes_pa5     0.62125        0.62125              0.64425
#> naive_bayes_pa6     0.61200        0.60450              0.61975
#> naive_bayes_pa7     0.62125        0.62125              0.64325
#> naive_bayes_pa8     0.62125        0.62125              0.62875
#> naive_bayes_pa9     0.64050        0.64050              0.65625
#> naive_bayes_pa10    0.61200        0.60450              0.62700
#>                  Balanced Accuracy Accuracy   Kappa AccuracyLower AccuracyUpper
#> kknn_pa1                   0.96300  0.96925 0.93275       0.84375       0.99625
#> kknn_pa2                   0.96050  0.96325 0.92225       0.83475       0.99750
#> kknn_pa3                   0.98100  0.98525 0.96825       0.87000       0.99950
#> kknn_pa4                   0.97125  0.97775 0.95200       0.85900       0.99800
#> kknn_pa5                   0.94875  0.95450 0.90250       0.82025       0.99600
#> kknn_pa6                   0.97475  0.97775 0.95250       0.85700       0.99925
#> kknn_pa7                   0.96750  0.97000 0.93525       0.84225       0.99900
#> kknn_pa8                   0.98450  0.98500 0.96800       0.86800       0.99950
#> kknn_pa9                   0.97900  0.98450 0.96600       0.86450       0.99950
#> kknn_pa10                  0.97500  0.97775 0.95275       0.85900       0.99800
#> naive_bayes_pa1            0.96250  0.96925 0.93225       0.84150       0.99750
#> naive_bayes_pa2            0.97275  0.97050 0.93875       0.84725       0.99775
#> naive_bayes_pa3            0.97475  0.97800 0.95275       0.85950       0.99800
#> naive_bayes_pa4            0.96525  0.97000 0.93650       0.84600       0.99775
#> naive_bayes_pa5            0.97700  0.97725 0.95100       0.85675       0.99925
#> naive_bayes_pa6            0.97475  0.97750 0.95225       0.85900       0.99800
#> naive_bayes_pa7            0.97125  0.97800 0.95125       0.86050       0.99525
#> naive_bayes_pa8            0.99050  0.99250 0.98400       0.88100       0.99975
#> naive_bayes_pa9            0.97825  0.98475 0.96550       0.86475       0.99950
#> naive_bayes_pa10           0.97450  0.97725 0.95175       0.85850       0.99800
#>                  AccuracyNull AccuracyPValue McnemarPValue Positive Negative
#> kknn_pa1              0.63050          0e+00     0.8266667     20.5    12.00
#> kknn_pa2              0.60750          0e+00     0.8700000     20.5    13.25
#> kknn_pa3              0.61200          0e+00     1.0000000     20.5    13.00
#> kknn_pa4              0.61200          0e+00     0.7400000     20.5    13.00
#> kknn_pa5              0.62125          0e+00     0.8700000     20.5    12.50
#> kknn_pa6              0.61200          0e+00     1.0000000     20.5    13.00
#> kknn_pa7              0.62125          0e+00     1.0000000     20.5    12.50
#> kknn_pa8              0.62100          0e+00     1.0000000     20.5    12.50
#> kknn_pa9              0.64050          0e+00     1.0000000     20.5    11.50
#> kknn_pa10             0.61200          0e+00     1.0000000     20.5    13.00
#> naive_bayes_pa1       0.63050          5e-04     0.8266667     20.5    12.00
#> naive_bayes_pa2       0.60750          0e+00     0.8266667     20.5    13.25
#> naive_bayes_pa3       0.61200          0e+00     0.8266667     20.5    13.00
#> naive_bayes_pa4       0.61200          0e+00     1.0000000     20.5    13.00
#> naive_bayes_pa5       0.62125          0e+00     1.0000000     20.5    12.50
#> naive_bayes_pa6       0.61200          0e+00     0.7400000     20.5    13.00
#> naive_bayes_pa7       0.62125          0e+00     0.2480000     20.5    12.50
#> naive_bayes_pa8       0.62125          0e+00     1.0000000     20.5    12.50
#> naive_bayes_pa9       0.64050          0e+00     1.0000000     20.5    11.50
#> naive_bayes_pa10      0.61200          0e+00     0.8266667     20.5    13.00
#>                  True Positive False Positive True Negative False Negative
#> kknn_pa1                 20.25           0.50         11.25           0.75
#> kknn_pa2                 20.00           0.50         12.50           0.75
#> kknn_pa3                 20.50           0.00         12.50           0.50
#> kknn_pa4                 20.50           0.00         12.25           0.75
#> kknn_pa5                 20.00           0.50         11.50           1.00
#> kknn_pa6                 20.25           0.25         12.50           0.50
#> kknn_pa7                 20.00           0.50         12.00           0.50
#> kknn_pa8                 20.25           0.25         12.25           0.25
#> kknn_pa9                 20.50           0.00         11.00           0.50
#> kknn_pa10                20.25           0.25         12.50           0.50
#> naive_bayes_pa1          20.50           0.25         11.25           1.25
#> naive_bayes_pa2          20.00           0.75         13.00           0.50
#> naive_bayes_pa3          20.25           0.50         12.50           0.50
#> naive_bayes_pa4          20.25           0.50         12.25           0.75
#> naive_bayes_pa5          20.50           0.50         12.25           0.75
#> naive_bayes_pa6          20.25           0.75         12.50           0.50
#> naive_bayes_pa7          20.50           0.00         11.75           0.75
#> naive_bayes_pa8          20.50           0.50         12.25           0.25
#> naive_bayes_pa9          20.50           0.50         11.00           0.50
#> naive_bayes_pa10         20.25           0.25         12.50           0.75
#>                        CBI pAUC Omission_10pct       ROCSD      TSSSD
#> kknn_pa1         1.0000000  NaN        0.04825 0.033117837 0.08714362
#> kknn_pa2               NaN  NaN        0.03625 0.029762926 0.04437463
#> kknn_pa3         1.0000000  NaN        0.06000 0.023311213 0.04441156
#> kknn_pa4         0.6666667  NaN        0.03700 0.037863947 0.07364824
#> kknn_pa5         1.0000000  NaN        0.06150 0.030448993 0.04780395
#> kknn_pa6         0.7500000  NaN        0.04825 0.017027120 0.03626755
#> kknn_pa7         1.0000000  NaN        0.04875 0.016051262 0.01759029
#> kknn_pa8         1.0000000  NaN        0.02450 0.020192308 0.03789040
#> kknn_pa9         0.6666667  NaN        0.06000 0.020234448 0.04811252
#> kknn_pa10              NaN  NaN        0.01200 0.030660566 0.06132113
#> naive_bayes_pa1  0.8750000  NaN        0.09750 0.035360349 0.15774745
#> naive_bayes_pa2  1.0000000  NaN        0.09750 0.002144207 0.05977329
#> naive_bayes_pa3  1.0000000  NaN        0.09750 0.014652015 0.07254689
#> naive_bayes_pa4  0.7250000  NaN        0.09750 0.029708885 0.07253510
#> naive_bayes_pa5  1.0000000  NaN        0.09750 0.002349746 0.07978559
#> naive_bayes_pa6  1.0000000  NaN        0.09750 0.003846154 0.07254689
#> naive_bayes_pa7  0.8290000  NaN        0.09750 0.021978022 0.11538462
#> naive_bayes_pa8  1.0000000  NaN        0.09750 0.000000000 0.03846154
#> naive_bayes_pa9  0.8000000  NaN        0.09750 0.006124793 0.05818741
#> naive_bayes_pa10 1.0000000  NaN        0.09750 0.015384615 0.07252661
#>                  SensitivitySD SpecificitySD Pos Pred ValueSD Neg Pred ValueSD
#> kknn_pa1            0.02771281    0.07990202       0.04379783       0.04625293
#> kknn_pa2            0.02830194    0.06898792       0.04352011       0.04099187
#> kknn_pa3            0.00000000    0.04445597       0.02687471       0.00000000
#> kknn_pa4            0.00000000    0.07372189       0.04209810       0.00000000
#> kknn_pa5            0.02830194    0.06294177       0.03717526       0.04625293
#> kknn_pa6            0.02500000    0.04445597       0.02687471       0.03550000
#> kknn_pa7            0.02886751    0.04625293       0.02598076       0.04279408
#> kknn_pa8            0.02400000    0.03850000       0.02400000       0.03850000
#> kknn_pa9            0.00000000    0.04792007       0.02771281       0.00000000
#> kknn_pa10           0.02400000    0.04445597       0.02771281       0.03850000
#> naive_bayes_pa1     0.02500000    0.15760076       0.07890501       0.03850000
#> naive_bayes_pa2     0.04571196    0.07150000       0.04550000       0.06409888
#> naive_bayes_pa3     0.02830194    0.07700000       0.04350000       0.04099187
#> naive_bayes_pa4     0.05000000    0.03850000       0.02455606       0.07150000
#> naive_bayes_pa5     0.02886751    0.07990202       0.04209810       0.04279408
#> naive_bayes_pa6     0.04571196    0.07700000       0.04550000       0.06409888
#> naive_bayes_pa7     0.00000000    0.11550000       0.06250000       0.00000000
#> naive_bayes_pa8     0.02830194    0.03850000       0.02400000       0.04279408
#> naive_bayes_pa9     0.02830194    0.05033554       0.02687471       0.04883305
#> naive_bayes_pa10    0.02500000    0.07700000       0.04550000       0.03550000
#>                  PrecisionSD   RecallSD        F1SD PrevalenceSD
#> kknn_pa1          0.04379783 0.02771281 0.027712813  0.006350853
#> kknn_pa2          0.04352011 0.02830194 0.011898879  0.014177447
#> kknn_pa3          0.02687471 0.00000000 0.013573872  0.006928203
#> kknn_pa4          0.04209810 0.00000000 0.021685248  0.006928203
#> kknn_pa5          0.03717526 0.02830194 0.013622897  0.012579746
#> kknn_pa6          0.02687471 0.02500000 0.012230427  0.006928203
#> kknn_pa7          0.02598076 0.02886751 0.001732051  0.012579746
#> kknn_pa8          0.02400000 0.02400000 0.013856406  0.017320508
#> kknn_pa9          0.02771281 0.00000000 0.013856406  0.017897858
#> kknn_pa10         0.02771281 0.02400000 0.022978251  0.006928203
#> naive_bayes_pa1   0.07890501 0.02500000 0.043054229  0.006350853
#> naive_bayes_pa2   0.04550000 0.04571196 0.020428738  0.014177447
#> naive_bayes_pa3   0.04350000 0.02830194 0.021685248  0.006928203
#> naive_bayes_pa4   0.02455606 0.05000000 0.032608792  0.006928203
#> naive_bayes_pa5   0.04209810 0.02886751 0.021685248  0.012579746
#> naive_bayes_pa6   0.04550000 0.04571196 0.023352373  0.006928203
#> naive_bayes_pa7   0.06250000 0.00000000 0.033500000  0.012579746
#> naive_bayes_pa8   0.02400000 0.02830194 0.012369317  0.012579746
#> naive_bayes_pa9   0.02687471 0.02830194 0.019636276  0.013178265
#> naive_bayes_pa10  0.04550000 0.02500000 0.023173260  0.006928203
#>                  Detection RateSD Detection PrevalenceSD Balanced AccuracySD
#> kknn_pa1              0.012463279             0.03509392         0.043351278
#> kknn_pa2              0.017916473             0.03784618         0.021946906
#> kknn_pa3              0.006928203             0.01828251         0.021939310
#> kknn_pa4              0.006928203             0.03059412         0.036818247
#> kknn_pa5              0.009797959             0.03080043         0.023921747
#> kknn_pa6              0.019824228             0.03121298         0.017914147
#> kknn_pa7              0.026381812             0.04299612         0.008812869
#> kknn_pa8              0.015000000             0.01732051         0.018788294
#> kknn_pa9              0.017897858             0.00000000         0.024248711
#> kknn_pa10             0.012369317             0.01236932         0.030484969
#> naive_bayes_pa1       0.019855730             0.05653612         0.079020567
#> naive_bayes_pa2       0.024864634             0.03184729         0.029676028
#> naive_bayes_pa3       0.018654758             0.04473254         0.036307713
#> naive_bayes_pa4       0.034960692             0.03121298         0.036092474
#> naive_bayes_pa5       0.026381812             0.04112076         0.039777506
#> naive_bayes_pa6       0.027060118             0.04820097         0.036307713
#> naive_bayes_pa7       0.012579746             0.04363007         0.057500000
#> naive_bayes_pa8       0.021354157             0.02609598         0.019000000
#> naive_bayes_pa9       0.022226111             0.03227486         0.029017236
#> naive_bayes_pa10      0.019824228             0.03950527         0.036299679
#>                    AccuracySD     KappaSD AccuracyLowerSD AccuracyUpperSD
#> kknn_pa1         0.0355093884 0.077706177     0.056358229    0.0043493295
#> kknn_pa2         0.0148408221 0.030847744     0.021297496    0.0030000000
#> kknn_pa3         0.0170367250 0.036664015     0.029540932    0.0005773503
#> kknn_pa4         0.0282886903 0.061275335     0.045070315    0.0033665016
#> kknn_pa5         0.0179164729 0.038126981     0.025953484    0.0034641016
#> kknn_pa6         0.0148408221 0.031670175     0.026770631    0.0005000000
#> kknn_pa7         0.0008164966 0.002061553     0.003685557    0.0000000000
#> kknn_pa8         0.0173205081 0.036950417     0.030022214    0.0005773503
#> kknn_pa9         0.0178978583 0.039259818     0.030599564    0.0005773503
#> kknn_pa10        0.0282886903 0.059974300     0.045070315    0.0033665016
#> naive_bayes_pa1  0.0592135964 0.135563946     0.086195031    0.0173397424
#> naive_bayes_pa2  0.0240901086 0.050629372     0.038560558    0.0032015621
#> naive_bayes_pa3  0.0282016548 0.061032095     0.043699733    0.0033665016
#> naive_bayes_pa4  0.0382840263 0.078745899     0.058076817    0.0091787799
#> naive_bayes_pa5  0.0292460824 0.065105043     0.047647141    0.0033665016
#> naive_bayes_pa6  0.0290803485 0.061915938     0.046669048    0.0037749172
#> naive_bayes_pa7  0.0440000000 0.097500000     0.065015383    0.0095000000
#> naive_bayes_pa8  0.0150222058 0.032000000     0.026115130    0.0005000000
#> naive_bayes_pa9  0.0253163320 0.056774407     0.039225842    0.0036968455
#> naive_bayes_pa10 0.0291590466 0.062066497     0.047947888    0.0033665016
#>                  AccuracyNullSD AccuracyPValueSD McnemarPValueSD PositiveSD
#> kknn_pa1            0.006350853            0.000       0.3676955  0.5773503
#> kknn_pa2            0.014177447            0.000       0.2600000  0.5773503
#> kknn_pa3            0.006928203            0.000       0.0000000  0.5773503
#> kknn_pa4            0.006928203            0.000       0.3676955  0.5773503
#> kknn_pa5            0.012579746            0.000       0.2600000  0.5773503
#> kknn_pa6            0.006928203            0.000       0.0000000  0.5773503
#> kknn_pa7            0.012579746            0.000       0.0000000  0.5773503
#> kknn_pa8            0.017320508            0.000       0.0000000  0.5773503
#> kknn_pa9            0.017897858            0.000       0.0000000  0.5773503
#> kknn_pa10           0.006928203            0.000       0.0000000  0.5773503
#> naive_bayes_pa1     0.006350853            0.001       0.6123545  0.5773503
#> naive_bayes_pa2     0.014177447            0.000       0.3002221  0.5773503
#> naive_bayes_pa3     0.006928203            0.000       0.3676955  0.5773503
#> naive_bayes_pa4     0.006928203            0.000       0.0000000  0.5773503
#> naive_bayes_pa5     0.012579746            0.000       0.3676955  0.5773503
#> naive_bayes_pa6     0.006928203            0.000       0.3676955  0.5773503
#> naive_bayes_pa7     0.012579746            0.000              NA  0.5773503
#> naive_bayes_pa8     0.012579746            0.000              NA  0.5773503
#> naive_bayes_pa9     0.013178265            0.000       0.0000000  0.5773503
#> naive_bayes_pa10    0.006928203            0.000       0.3676955  0.5773503
#>                  NegativeSD True PositiveSD False PositiveSD True NegativeSD
#> kknn_pa1          0.0000000       0.5000000        0.5773503       0.9574271
#> kknn_pa2          0.5000000       0.8164966        0.5773503       0.5773503
#> kknn_pa3          0.0000000       0.5773503        0.0000000       0.5773503
#> kknn_pa4          0.0000000       0.5773503        0.0000000       0.9574271
#> kknn_pa5          0.5773503       0.8164966        0.5773503       0.5773503
#> kknn_pa6          0.0000000       0.9574271        0.5000000       0.5773503
#> kknn_pa7          0.5773503       1.1547005        0.5773503       0.8164966
#> kknn_pa8          0.5773503       0.5000000        0.5000000       0.5000000
#> kknn_pa9          0.5773503       0.5773503        0.0000000       0.0000000
#> kknn_pa10         0.0000000       0.5000000        0.5000000       0.5773503
#> naive_bayes_pa1   0.0000000       0.9574271        0.5000000       1.8929694
#> naive_bayes_pa2   0.5000000       0.9574271        0.9574271       0.5000000
#> naive_bayes_pa3   0.0000000       0.8164966        0.5773503       1.0000000
#> naive_bayes_pa4   0.0000000       1.4142136        1.0000000       0.5000000
#> naive_bayes_pa5   0.5773503       1.1547005        0.5773503       1.5000000
#> naive_bayes_pa6   0.0000000       0.9574271        0.9574271       1.0000000
#> naive_bayes_pa7   0.5773503       0.5773503        0.0000000       1.2583057
#> naive_bayes_pa8   0.5773503       0.8164966        0.5773503       0.5000000
#> naive_bayes_pa9   0.5773503       0.8164966        0.5773503       0.8164966
#> naive_bayes_pa10  0.0000000       0.9574271        0.5000000       1.0000000
#>                  False NegativeSD     CBISD pAUCSD Omission_10pctSD
#> kknn_pa1                0.9574271        NA     NA      0.038801847
#> kknn_pa2                0.9574271        NA     NA      0.045711961
#> kknn_pa3                0.5773503 0.0000000     NA      0.045276926
#> kknn_pa4                0.9574271 0.5773503     NA      0.024684678
#> kknn_pa5                0.8164966 0.0000000     NA      0.025683977
#> kknn_pa6                0.5773503 0.3535534     NA      0.038801847
#> kknn_pa7                0.5773503        NA     NA      0.038810437
#> kknn_pa8                0.5000000 0.0000000     NA      0.028301943
#> kknn_pa9                0.5773503 0.5773503     NA      0.045276926
#> kknn_pa10               0.5773503        NA     NA      0.024000000
#> naive_bayes_pa1         1.8929694 0.3949076     NA      0.002886751
#> naive_bayes_pa2         1.0000000 0.4836610     NA      0.002886751
#> naive_bayes_pa3         1.0000000 0.3000000     NA      0.002886751
#> naive_bayes_pa4         0.5000000 0.3722419     NA      0.002886751
#> naive_bayes_pa5         0.9574271 0.3464102     NA      0.002886751
#> naive_bayes_pa6         1.0000000 0.3975000     NA      0.002886751
#> naive_bayes_pa7         1.5000000 0.3695000     NA      0.002886751
#> naive_bayes_pa8         0.5000000 0.0000000     NA      0.002886751
#> naive_bayes_pa9         0.5773503 0.4000000     NA      0.002886751
#> naive_bayes_pa10        1.0000000 0.3985000     NA      0.002886751
```

Otherwise, the mean validation metric values per algorithm can also be
obtained with the following code:

``` r
mean_validation_metrics(i)
#> $`Araucaria angustifolia`
#> # A tibble: 2 × 59
#>   algo       ROC   TSS Sensitivity Specificity `Pos Pred Value` `Neg Pred Value`
#>   <chr>    <dbl> <dbl>       <dbl>       <dbl>            <dbl>            <dbl>
#> 1 kknn     0.979 0.941       0.988       0.953            0.972            0.981
#> 2 naive_b… 0.997 0.948       0.993       0.961            0.977            0.989
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes kknn 
#> Variables Names               : bio1 bio12 
#> Model Validation              :
#>     Method                    : repeatedcv 
#>     Number                    : 4 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1        kknn 0.9788004 0.9407967     0.98775    0.953000
#> 2 naive_bayes 0.9965948 0.9482630     0.99265    0.960525
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
functioning. For plot_predictions, we can set some parameters to control
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

![](5_InvasiveSpp_files/figure-html/plot_current_results-1.png)

``` r
plot_ensembles(i,
               scenario = "_ssp585_2090",
               ensemble_type = "average")
```

![](5_InvasiveSpp_files/figure-html/ssp245_2090-1.png)

Another plot widely used in SDM studies is the Partial Dependence Plot,
which informs the response curves to each variable. Here we are using
PCA axes as predictors, so there is not much sense in plotting these
curves, but if someone want to do that, it is possible through the
`pdp_sdm` function.

``` r
pdp_sdm(i)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](5_InvasiveSpp_files/figure-html/pdp_sdm-1.png)

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
#> Time difference of 20.49651 secs
```
