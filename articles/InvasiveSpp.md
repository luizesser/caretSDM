# Projecting Non-native Distribution using SDMs

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

![](InvasiveSpp_files/figure-html/parana_view-1.png)

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

![](InvasiveSpp_files/figure-html/plot_sdm_area-1.png)

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

![](InvasiveSpp_files/figure-html/plot_occurrences-1.png)

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
#> Number of presences           : 84 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : bioclim 
#>     Number of PA sets         : 10 
#>     Number of PAs in each set : 84 
#> Data Cleaning                 : NAs, Capitals, Centroids, Geographically Duplicated, Identical Lat/Long, Institutions, Invalid, Non-terrestrial, Duplicated Cell (grid) 
#> --------  Predictors  ---------
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Area (VIF)                    : all
#> Threshold                     : 0.5
#> Selected Variables (VIF)      : bio1, bio12 
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Area (VIF)                    : all
#> Threshold                     : 0.5
#> Selected Variables (VIF)      : bio1, bio12 
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
#> 1        kknn 0.9857309 0.9703630      0.9928    0.977475
#> 2 naive_bayes 0.9978861 0.9578601      0.9964    0.965025
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
#> [1] "current"
#> [1] "Araucaria angustifolia"
#> [1] "ca_ssp245_2090"
#> [1] "Araucaria angustifolia"
#> [1] "ca_ssp585_2090"
#> [1] "Araucaria angustifolia"
#> [1] "mi_ssp245_2090"
#> [1] "Araucaria angustifolia"
#> [1] "mi_ssp585_2090"
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Area (VIF)                    : all
#> Threshold                     : 0.5
#> Selected Variables (VIF)      : bio1, bio12 
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
#> 1        kknn 0.9857309 0.9703630      0.9928    0.977475
#> 2 naive_bayes 0.9978861 0.9578601      0.9964    0.965025
#> 
#> --------  Predictions  --------
#> Ensembles                     :
#>     Scenarios                 : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 
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
#> m1.2         kknn 1.0000000 1.0000000       1.000     1.00000        1.00000
#> m2.2         kknn 0.9903846 0.9807692       1.000     0.98075        0.98875
#> m3.2         kknn 0.9844322 0.9688645       0.988     0.98075        0.98875
#> m4.2         kknn 0.9748168 0.9496337       0.988     0.96150        0.97750
#> m5.2         kknn 1.0000000 1.0000000       1.000     1.00000        1.00000
#> m6.2         kknn 0.9732143 0.9464286       0.988     0.95825        0.97825
#> m7.2         kknn 0.9732143 0.9464286       0.988     0.95825        0.97825
#> m8.2         kknn 0.9772727 0.9426407       0.988     0.95450        0.97825
#> m9.2         kknn 0.9899267 0.9807692       1.000     0.98075        0.98875
#> m10.2        kknn 0.9940476 0.9880952       0.988     1.00000        1.00000
#> m1.1  naive_bayes 1.0000000 0.9791667       1.000     0.97925        0.98875
#> m2.1  naive_bayes 0.9990079 0.9791667       1.000     0.97925        0.98875
#> m3.1  naive_bayes 1.0000000 0.9672619       1.000     0.97925        0.98875
#> m4.1  naive_bayes 0.9954212 0.9496337       1.000     0.96150        0.97675
#> m5.1  naive_bayes 1.0000000 0.9807692       1.000     0.98075        0.98875
#> m6.1  naive_bayes 0.9950397 0.9445346       0.988     0.95650        0.97750
#> m7.1  naive_bayes 1.0000000 0.9111722       0.988     0.92300        0.95650
#> m8.1  naive_bayes 0.9940476 0.9380952       0.988     0.95000        0.97750
#> m9.1  naive_bayes 0.9963370 0.9615385       1.000     0.96150        0.97750
#> m10.1 naive_bayes 0.9990079 0.9672619       1.000     0.97925        0.98875
#>       Neg Pred Value Precision Recall      F1 Prevalence Detection Rate
#> m1.2         1.00000   1.00000  1.000 1.00000    0.64600        0.64600
#> m2.2         1.00000   0.98875  1.000 0.99425    0.63150        0.63150
#> m3.2         0.98075   0.98875  0.988 0.98825    0.62700        0.61950
#> m4.2         0.98225   0.97750  0.988 0.98250    0.61800        0.61050
#> m5.2         1.00000   1.00000  1.000 1.00000    0.62250        0.62250
#> m6.2         0.97925   0.97825  0.988 0.98275    0.64100        0.63325
#> m7.2         0.98075   0.97825  0.988 0.98275    0.62700        0.61950
#> m8.2         0.97725   0.97825  0.988 0.98275    0.67175        0.66375
#> m9.2         1.00000   0.98875  1.000 0.99425    0.61800        0.61800
#> m10.2        0.98075   1.00000  0.988 0.99400    0.64100        0.63350
#> m1.1         1.00000   0.98875  1.000 0.99425    0.64600        0.64600
#> m2.1         1.00000   0.98875  1.000 0.99425    0.63150        0.63150
#> m3.1         1.00000   0.98875  1.000 0.98875    0.62700        0.62700
#> m4.1         1.00000   0.97675  1.000 0.98275    0.61800        0.61800
#> m5.1         1.00000   0.98875  1.000 0.99425    0.62250        0.62250
#> m6.1         0.98075   0.97750  0.988 0.98250    0.64100        0.63350
#> m7.1         0.98075   0.95650  0.988 0.97150    0.62700        0.61950
#> m8.1         0.97725   0.97750  0.988 0.98250    0.67175        0.66375
#> m9.1         1.00000   0.97750  1.000 0.98850    0.61800        0.61800
#> m10.1        1.00000   0.98875  1.000 0.98825    0.64100        0.64100
#>       Detection Prevalence Balanced Accuracy Accuracy   Kappa AccuracyLower
#> m1.2               0.64600           1.00000  1.00000 1.00000       0.89250
#> m2.2               0.63875           0.99050  0.99275 0.98425       0.88225
#> m3.2               0.62675           0.98450  0.98525 0.96825       0.87000
#> m4.2               0.62500           0.97500  0.97825 0.95325       0.85950
#> m5.2               0.62250           1.00000  1.00000 1.00000       0.89625
#> m6.2               0.64850           0.97325  0.97700 0.94900       0.85600
#> m7.2               0.63475           0.97325  0.97725 0.95000       0.85850
#> m8.2               0.67950           0.97125  0.97650 0.94575       0.85025
#> m9.2               0.62525           0.99050  0.99275 0.98425       0.88450
#> m10.2              0.63350           0.99400  0.99250 0.98400       0.88025
#> m1.1               0.66175           0.98950  0.99250 0.98325       0.87950
#> m2.1               0.64700           0.98950  0.99250 0.98325       0.88175
#> m3.1               0.64225           0.98350  0.98525 0.96800       0.87150
#> m4.1               0.63975           0.97500  0.97825 0.95300       0.86100
#> m5.1               0.62975           0.99050  0.99275 0.98425       0.88375
#> m6.1               0.64925           0.97225  0.97725 0.94950       0.85400
#> m7.1               0.64850           0.95550  0.96300 0.92000       0.83550
#> m8.1               0.68025           0.96900  0.97600 0.94400       0.84750
#> m9.1               0.63250           0.98100  0.98550 0.96850       0.87200
#> m10.1              0.66400           0.98350  0.98500 0.96725       0.86725
#>       AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue Positive Negative
#> m1.2        1.00000      0.64600              0           NaN       21    11.50
#> m2.2        0.99975      0.63150              0     1.0000000       21    12.25
#> m3.2        0.99950      0.62700              0     1.0000000       21    12.50
#> m4.2        0.99925      0.61800              0     1.0000000       21    13.00
#> m5.2        1.00000      0.62250              0           NaN       21    12.75
#> m6.2        0.99800      0.64100              0     0.7400000       21    11.75
#> m7.2        0.99800      0.62700              0     0.7400000       21    12.50
#> m8.2        0.99775      0.67175              0     0.7400000       21    10.25
#> m9.2        0.99975      0.61800              0     1.0000000       21    13.00
#> m10.2       0.99975      0.64100              0     1.0000000       21    11.75
#> m1.1        0.99975      0.64600              0     1.0000000       21    11.50
#> m2.1        0.99975      0.63150              0     1.0000000       21    12.25
#> m3.1        0.99950      0.62700              0     1.0000000       21    12.50
#> m4.1        0.99925      0.61800              0     1.0000000       21    13.00
#> m5.1        0.99975      0.62250              0     1.0000000       21    12.75
#> m6.1        0.99925      0.64100              0     1.0000000       21    11.75
#> m7.1        0.99625      0.62700              0     0.6533333       21    12.50
#> m8.1        0.99925      0.67175              0     1.0000000       21    10.25
#> m9.1        0.99950      0.61800              0     1.0000000       21    13.00
#> m10.1       0.99950      0.64100              0     1.0000000       21    11.75
#>       True Positive False Positive True Negative False Negative     CBI pAUC
#> m1.2          21.00           0.00         11.50           0.00     NaN  NaN
#> m2.2          21.00           0.00         12.00           0.25     NaN  NaN
#> m3.2          20.75           0.25         12.25           0.25     NaN  NaN
#> m4.2          20.75           0.25         12.50           0.50     NaN  NaN
#> m5.2          21.00           0.00         12.75           0.00     NaN  NaN
#> m6.2          20.75           0.25         11.25           0.50 1.00000  NaN
#> m7.2          20.75           0.25         12.00           0.50     NaN  NaN
#> m8.2          20.75           0.25          9.75           0.50 1.00000  NaN
#> m9.2          21.00           0.00         12.75           0.25 1.00000  NaN
#> m10.2         20.75           0.50         11.75           0.00     NaN  NaN
#> m1.1          21.00           0.00         11.25           0.50 1.00000  NaN
#> m2.1          21.00           0.00         12.00           0.50 1.00000  NaN
#> m3.1          21.00           0.25         12.25           0.50 1.00000  NaN
#> m4.1          21.00           0.25         12.50           0.75 1.00000  NaN
#> m5.1          21.00           0.00         12.50           0.25     NaN  NaN
#> m6.1          20.75           0.25         11.25           0.50 1.00000  NaN
#> m7.1          20.75           0.25         11.50           1.00 1.00000  NaN
#> m8.1          20.75           0.25          9.75           0.50 1.00000  NaN
#> m9.1          21.00           0.00         12.50           0.50 1.00000  NaN
#> m10.1         21.00           0.25         11.50           0.75 0.86175  NaN
#>       Omission_10pct       ROCSD      TSSSD SensitivitySD SpecificitySD
#> m1.2         0.00000 0.000000000 0.00000000    0.00000000    0.00000000
#> m2.2         0.00000 0.019230769 0.03846154    0.00000000    0.03850000
#> m3.2         0.03575 0.018945202 0.03789040    0.02400000    0.03850000
#> m4.2         0.02400 0.018154055 0.03630811    0.02400000    0.04445597
#> m5.2         0.00000 0.000000000 0.00000000    0.00000000    0.00000000
#> m6.2         0.02400 0.039333785 0.07866757    0.02400000    0.08350000
#> m7.2         0.01200 0.039333785 0.07866757    0.02400000    0.08350000
#> m8.2         0.02375 0.045454545 0.08595554    0.02400000    0.09100000
#> m9.2         0.03575 0.020146520 0.03846154    0.00000000    0.03850000
#> m10.2        0.03575 0.011904762 0.02749287    0.02771281    0.00000000
#> m1.1         0.09500 0.000000000 0.05039445    0.00000000    0.05033554
#> m2.1         0.09500 0.011904762 0.04811252    0.00000000    0.04792007
#> m3.1         0.09500 0.000000000 0.08333333    0.02400000    0.08350000
#> m4.1         0.09500 0.009157509 0.06132113    0.02400000    0.04445597
#> m5.1         0.09500 0.000000000 0.03846154    0.00000000    0.03850000
#> m6.1         0.09500 0.009920635 0.04151647    0.02400000    0.05033554
#> m7.1         0.09500 0.010521177 0.07755280    0.02400000    0.08891194
#> m8.1         0.09500 0.014285714 0.04809288    0.02400000    0.05773503
#> m9.1         0.09500 0.010521177 0.04441156    0.00000000    0.04445597
#> m10.1        0.09500 0.003968254 0.07978559    0.02400000    0.07990202
#>       Pos Pred ValueSD Neg Pred ValueSD PrecisionSD   RecallSD       F1SD
#> m1.2        0.00000000       0.00000000  0.00000000 0.00000000 0.00000000
#> m2.2        0.02250000       0.00000000  0.02250000 0.00000000 0.01150000
#> m3.2        0.02250000       0.03850000  0.02250000 0.02400000 0.01357387
#> m4.2        0.02598076       0.03550000  0.02598076 0.02400000 0.01167619
#> m5.2        0.00000000       0.00000000  0.00000000 0.00000000 0.00000000
#> m6.2        0.04350000       0.04150000  0.04350000 0.02400000 0.02168525
#> m7.2        0.04350000       0.03850000  0.04350000 0.02400000 0.02168525
#> m8.2        0.04350000       0.04550000  0.04350000 0.02400000 0.02168525
#> m9.2        0.02250000       0.00000000  0.02250000 0.00000000 0.01150000
#> m10.2       0.00000000       0.04625293  0.00000000 0.02771281 0.01385641
#> m1.1        0.02598076       0.00000000  0.02598076 0.00000000 0.01327906
#> m2.1        0.02598076       0.00000000  0.02598076 0.00000000 0.01327906
#> m3.1        0.04350000       0.03550000  0.04350000 0.02400000 0.02250000
#> m4.1        0.02687471       0.03850000  0.02687471 0.02400000 0.02289651
#> m5.1        0.02250000       0.00000000  0.02250000 0.00000000 0.01150000
#> m6.1        0.02598076       0.03850000  0.02598076 0.02400000 0.01167619
#> m7.1        0.05022947       0.03850000  0.05022947 0.02400000 0.02142429
#> m8.1        0.02598076       0.04550000  0.02598076 0.02400000 0.01167619
#> m9.1        0.02598076       0.00000000  0.02598076 0.00000000 0.01327906
#> m10.1       0.04178516       0.03850000  0.04178516 0.02400000 0.02158703
#>       PrevalenceSD Detection RateSD Detection PrevalenceSD Balanced AccuracySD
#> m1.2    0.01154701       0.01154701             0.01154701          0.00000000
#> m2.2    0.00900000       0.00900000             0.00550000          0.01900000
#> m3.2    0.01039230       0.01236932             0.01828251          0.01878829
#> m4.2    0.00000000       0.01500000             0.02820165          0.01792577
#> m5.2    0.00900000       0.00900000             0.00900000          0.00000000
#> m6.2    0.01000000       0.00550000             0.03274650          0.03916950
#> m7.2    0.01039230       0.01236932             0.04188377          0.03916950
#> m8.2    0.01050000       0.01594522             0.03034798          0.04301453
#> m9.2    0.00000000       0.00000000             0.01450000          0.01900000
#> m10.2   0.01000000       0.02061553             0.02061553          0.01385641
#> m1.1    0.01154701       0.01154701             0.02170061          0.02514458
#> m2.1    0.00900000       0.00900000             0.02423496          0.02424871
#> m3.1    0.01039230       0.02264950             0.03747332          0.04150000
#> m4.1    0.00000000       0.01500000             0.01450000          0.03048497
#> m5.1    0.00900000       0.00900000             0.01429161          0.01900000
#> m6.1    0.01000000       0.02061553             0.03588291          0.02069420
#> m7.1    0.01039230       0.01236932             0.03403430          0.03878574
#> m8.1    0.01050000       0.01594522             0.03464462          0.02402776
#> m9.1    0.00000000       0.00000000             0.01674316          0.02193931
#> m10.1   0.01000000       0.02061553             0.02677530          0.03977751
#>       AccuracySD    KappaSD AccuracyLowerSD AccuracyUpperSD AccuracyNullSD
#> m1.2  0.00000000 0.00000000     0.001732051    0.0000000000     0.01154701
#> m2.2  0.01450000 0.03150000     0.023500000    0.0005000000     0.00900000
#> m3.2  0.01703673 0.03666402     0.029540932    0.0005773503     0.01039230
#> m4.2  0.01450000 0.03118092     0.025000000    0.0005000000     0.00000000
#> m5.2  0.00000000 0.00000000     0.001500000    0.0000000000     0.00900000
#> m6.2  0.02924608 0.06510504     0.046818800    0.0033665016     0.01000000
#> m7.2  0.02915905 0.06478683     0.047947888    0.0033665016     0.01039230
#> m8.2  0.02977135 0.06937038     0.046693147    0.0038622101     0.01050000
#> m9.2  0.01450000 0.03150000     0.025000000    0.0005000000     0.00000000
#> m10.2 0.01761391 0.03814009     0.031219652    0.0005773503     0.01000000
#> m1.1  0.01761391 0.03987062     0.030379544    0.0005773503     0.01154701
#> m2.1  0.01732051 0.03868247     0.030912511    0.0005773503     0.00900000
#> m3.1  0.03050000 0.06800000     0.049020404    0.0035000000     0.01039230
#> m4.1  0.02820165 0.05988322     0.045284287    0.0033665016     0.00000000
#> m5.1  0.01450000 0.03150000     0.024540782    0.0005000000     0.00900000
#> m6.1  0.01517399 0.03378856     0.026733250    0.0005000000     0.01000000
#> m7.1  0.02820165 0.06127533     0.043116122    0.0037749172     0.01039230
#> m8.1  0.01600000 0.03738092     0.029000000    0.0005000000     0.01050000
#> m9.1  0.01674316 0.03637307     0.028867513    0.0005773503     0.00000000
#> m10.1 0.02915905 0.06501987     0.045565886    0.0033665016     0.01000000
#>       AccuracyPValueSD McnemarPValueSD PositiveSD NegativeSD True PositiveSD
#> m1.2                 0              NA          0  0.5773503       0.0000000
#> m2.2                 0              NA          0  0.5000000       0.0000000
#> m3.2                 0       0.0000000          0  0.5773503       0.5000000
#> m4.2                 0       0.0000000          0  0.0000000       0.5000000
#> m5.2                 0              NA          0  0.5000000       0.0000000
#> m6.2                 0       0.3676955          0  0.5000000       0.5000000
#> m7.2                 0       0.3676955          0  0.5773503       0.5000000
#> m8.2                 0       0.3676955          0  0.5000000       0.5000000
#> m9.2                 0              NA          0  0.0000000       0.0000000
#> m10.2                0              NA          0  0.5000000       0.5773503
#> m1.1                 0              NA          0  0.5773503       0.0000000
#> m2.1                 0              NA          0  0.5000000       0.0000000
#> m3.1                 0              NA          0  0.5773503       0.5000000
#> m4.1                 0       0.0000000          0  0.0000000       0.5000000
#> m5.1                 0              NA          0  0.5000000       0.0000000
#> m6.1                 0       0.0000000          0  0.5000000       0.5000000
#> m7.1                 0       0.3002221          0  0.5773503       0.5000000
#> m8.1                 0       0.0000000          0  0.5000000       0.5000000
#> m9.1                 0       0.0000000          0  0.0000000       0.0000000
#> m10.1                0       0.3676955          0  0.5000000       0.5000000
#>       False PositiveSD True NegativeSD False NegativeSD     CBISD pAUCSD
#> m1.2         0.0000000       0.5773503        0.0000000        NA     NA
#> m2.2         0.0000000       0.0000000        0.5000000        NA     NA
#> m3.2         0.5000000       0.5000000        0.5000000        NA     NA
#> m4.2         0.5000000       0.5773503        0.5773503        NA     NA
#> m5.2         0.0000000       0.5000000        0.0000000        NA     NA
#> m6.2         0.5000000       0.9574271        1.0000000        NA     NA
#> m7.2         0.5000000       1.4142136        1.0000000        NA     NA
#> m8.2         0.5000000       0.5000000        1.0000000        NA     NA
#> m9.2         0.0000000       0.5000000        0.5000000        NA     NA
#> m10.2        0.5773503       0.5000000        0.0000000        NA     NA
#> m1.1         0.0000000       0.8164966        0.5773503        NA     NA
#> m2.1         0.0000000       0.9574271        0.5773503 0.3000000     NA
#> m3.1         0.5000000       1.4142136        1.0000000 0.0000000     NA
#> m4.1         0.5000000       0.5773503        0.5773503 0.3000000     NA
#> m5.1         0.0000000       0.5773503        0.5000000        NA     NA
#> m6.1         0.5000000       0.9574271        0.5773503 0.2500000     NA
#> m7.1         0.5000000       0.5773503        1.1547005 0.3949076     NA
#> m8.1         0.5000000       0.9574271        0.5773503 0.3066544     NA
#> m9.1         0.0000000       0.5773503        0.5773503 0.2500000     NA
#> m10.1        0.5000000       0.8164966        0.9574271 0.3535534     NA
#>       Omission_10pctSD
#> m1.2        0.00000000
#> m2.2        0.00000000
#> m3.2        0.04552197
#> m4.2        0.02771281
#> m5.2        0.00000000
#> m6.2        0.02771281
#> m7.2        0.02400000
#> m8.2        0.04750000
#> m9.2        0.04552197
#> m10.2       0.04552197
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
#> 1 kknn     0.986 0.970       0.993       0.977            0.988            0.988
#> 2 naive_b… 0.998 0.958       0.996       0.965            0.981            0.994
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
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Area (VIF)                    : all
#> Threshold                     : 0.5
#> Selected Variables (VIF)      : bio1, bio12 
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
#> 1        kknn 0.9857309 0.9703630      0.9928    0.977475
#> 2 naive_bayes 0.9978861 0.9578601      0.9964    0.965025
#> 
#> --------  Predictions  --------
#> Ensembles                     :
#>     Scenarios                 : current ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 _ssp245_2090 _ssp585_2090 
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

![](InvasiveSpp_files/figure-html/plot_current_results-1.png)

``` r
plot_predictions(i,
                 spp_name = NULL,
                 scenario = "_ssp245_2090",
                 id = NULL,
                 ensemble = TRUE,
                 ensemble_type = "mean_occ_prob")
```

![](InvasiveSpp_files/figure-html/ssp245_2090-1.png)

Another plot widely used in SDM studies is the Partial Dependence Plot,
which informs the response curves to each variable. Here we are using
PCA axes as predictors, so there is not much sense in plotting these
curves, but if someone want to do that, it is possible through the
`pdp_sdm` function.

``` r
pdp_sdm(i)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](InvasiveSpp_files/figure-html/pdp_sdm-1.png)

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
#> Time difference of 1.875676 mins
```
