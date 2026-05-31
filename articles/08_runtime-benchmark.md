# 8. Benchmarking SDM Package Performance in R

## Motivation

Species distribution models (SDMs) are implemented in several R
packages, each providing different workflows, abstractions, and
computational strategies. Despite their widespread use, **direct and
reproducible comparisons of computational performance are rare**,
largely because differences in data preparation, validation strategies,
and algorithm implementations obscure fair comparisons.

This document provides a **fully reproducible benchmark** comparing the
runtime performance of four widely used SDM frameworks:

- **biomod2**
- **sdm**
- **caretSDM**

All packages are evaluated under **identical conditions**, using the
same data, algorithms, and validation structure.

------------------------------------------------------------------------

## Benchmark design

To ensure fairness and reproducibility, we enforced the following
constraints:

- Identical pseudoabsence method
- Identical environmental predictors
- Identical validation method
- Same algorithms across packages (Artificial Neural Network,
  Classification Tree Analysis, Flexible Discriminant Analysis, Boosted
  Regression Trees, Multiple Adaptive Regression Splines and Random
  Forest)
- No parallelization
- Runtime measured using the `bench` package

Benchmarks included three stages: preprocessing (data formatting),
processing (model fitting), and postprocessing (projection). A complete
end-to-end benchmark was also conducted. Preprocessing included data
formatting and pseudoabsence generation, processing included model
fitting with cross-validation and ensembling, and postprocessing
included projection to future scenarios.

------------------------------------------------------------------------

## Packages

``` r

library(terra)
library(sf)
library(bench)

library(biomod2)
library(sdm)
library(caretSDM)

library(RSNNS)
library(rpart)
library(mda)
library(gbm)
library(earth)
library(randomForest)

set.seed(1)
```

------------------------------------------------------------------------

## Data

### Presence–absence data

``` r

head(occ)
#>                    species decimalLongitude decimalLatitude
#> 327 Araucaria angustifolia         -4700678        -3065133
#> 405 Araucaria angustifolia         -4711827        -3146727
#> 404 Araucaria angustifolia         -4711885        -3147170
#> 310 Araucaria angustifolia         -4717665        -3142767
#> 49  Araucaria angustifolia         -4726011        -3148963
#> 124 Araucaria angustifolia         -4727265        -3148517
```

### Environmental predictors

``` r

bioc
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min.  1st Qu.   Median     Mean 3rd Qu. Max.  NAs
#> current  14.58698 21.19678 298.9147 622.9417  1353.5 2368 1845
#> dimension(s):
#>      from  to offset   delta refsys point              values x/y
#> x     747 798   -180  0.1667 WGS 84 FALSE                NULL [x]
#> y     670 706     90 -0.1667 WGS 84 FALSE                NULL [y]
#> band    1   3     NA      NA     NA    NA bio1 , bio4 , bio12
```

------------------------------------------------------------------------

## Package-specific benchmark functions

### biomod2

``` r

# ---------------------------
# biomod2 - Pre-processing
# ---------------------------

prep_biomod2 <- function() {
  occ_biomod <- occ
  occ_biomod$species <- rep(1, nrow(occ))

  env <- terra::rast(stars::st_warp(bioc, crs = 6933))

  sc <- list(
    scen1 = terra::rast(stars::st_warp(scen[1], crs = 6933)),
    scen2 = terra::rast(stars::st_warp(scen[2], crs = 6933)),
    scen3 = terra::rast(stars::st_warp(scen[3], crs = 6933)),
    scen4 = terra::rast(stars::st_warp(scen[4], crs = 6933))
  )

  biomod_data <- BIOMOD_FormatingData(
    resp.var = occ_biomod$species,
    expl.var = env,
    resp.xy = occ_biomod[, c("decimalLongitude", "decimalLatitude")],
    resp.name = "species",
    PA.nb.rep = 1,
    PA.nb.absences = nrow(occ_biomod),
    PA.strategy = "random"
  )

  list(data = biomod_data, scen = sc)
}

# ---------------------------
# biomod2 - Processing
# ---------------------------

fit_biomod2 <- function(prep) {
  biomod_model <- BIOMOD_Modeling(
    prep$data,
    modeling.id = "AllModels",
    models = c("ANN", "CTA", "FDA", "GBM", "MARS", "RF"),
    CV.strategy = "kfold",
    CV.nb.rep = 1,
    CV.k = 5,
    metric.eval = c("TSS", "AUCroc"),
    do.progress = FALSE
  )

  biomod_ensmodel <- BIOMOD_EnsembleModeling(
    bm.mod = biomod_model,
    models.chosen = "all",
    em.by = "all",
    em.algo = c("EMmean", "EMwmean", "EMca"),
    metric.select = c("AUCroc"),
    metric.select.thresh = c(0.5)
  )

  list(
    model = biomod_model,
    ensemble = biomod_ensmodel,
    scen = prep$scen
  )
}

# ---------------------------
# biomod2 - Post-processing
# ---------------------------

post_biomod2 <- function(fit) {
  biomod_proj <- lapply(names(fit$scen), function(nm) {
    BIOMOD_Projection(
      bm.mod = fit$model,
      proj.name = nm,
      new.env = fit$scen[[nm]],
      models.chosen = "all",
      overwrite = TRUE,
      do.stack = TRUE,
      nb.cpu = 1
    )
  })

  mapply(function(proj, nm) {
    BIOMOD_EnsembleForecasting(
      bm.em = fit$ensemble,
      bm.proj = proj,
      proj.name = nm,
      models.chosen = "all",
      overwrite = TRUE
    )
  }, biomod_proj, names(fit$scen))
}

# ---------------------------
# biomod2 - Complete
# ---------------------------

run_biomod2 <- function() {
  occ_biomod <- occ
  occ_biomod$species <- rep(1, nrow(occ))
  env <- terra::rast(stars::st_warp(bioc, crs = 6933))
  sc <- list(
    scen1 = terra::rast(stars::st_warp(scen[1], crs = 6933)),
    scen2 = terra::rast(stars::st_warp(scen[2], crs = 6933)),
    scen3 = terra::rast(stars::st_warp(scen[3], crs = 6933)),
    scen4 = terra::rast(stars::st_warp(scen[4], crs = 6933))
  )

  biomod_data <- BIOMOD_FormatingData(
    resp.var = occ_biomod$species,
    expl.var = env,
    resp.xy = occ_biomod[, c("decimalLongitude", "decimalLatitude")],
    resp.name = "species",
    PA.nb.rep = 1,
    PA.nb.absences = nrow(occ_biomod),
    PA.strategy = "random"
  )

  biomod_model <- BIOMOD_Modeling(
    biomod_data,
    modeling.id = "AllModels",
    models = c("ANN", "CTA", "FDA", "GBM", "MARS", "RF"),
    CV.strategy = "kfold",
    CV.nb.rep = 1,
    CV.k = 5,
    metric.eval = c("TSS", "AUCroc"),
    do.progress = FALSE
  )

  biomod_ensmodel <- BIOMOD_EnsembleModeling(
    bm.mod = biomod_model,
    models.chosen = "all",
    em.by = "all",
    em.algo = c("EMmean", "EMwmean", "EMca"),
    metric.select = c("AUCroc"),
    metric.select.thresh = c(0.5)
  )

  biomod_proj <- lapply(names(sc), function(nm) {
    BIOMOD_Projection(
      bm.mod = biomod_model,
      proj.name = nm,
      new.env = sc[[nm]],
      models.chosen = "all",
      overwrite = TRUE,
      do.stack = TRUE,
      nb.cpu = 1
    )
  })

  biomod_ensforecast <- mapply(function(proj, nm) {
    BIOMOD_EnsembleForecasting(
      bm.em = biomod_ensmodel,
      bm.proj = proj,
      proj.name = nm,
      models.chosen = "all",
      overwrite = TRUE
    )
  }, biomod_proj, names(sc))
}
```

------------------------------------------------------------------------

### sdm

``` r

# ---------------------------
# sdm - Pre-processing
# ---------------------------

prep_sdm <- function() {
  coords <- occ[, c("decimalLongitude", "decimalLatitude")]
  names(coords) <- c("x", "y")

  env <- terra::rast(stars::st_warp(bioc, crs = 6933))

  pres_vals <- terra::extract(env, vect(coords))
  pres_vals <- pres_vals[, -1]

  pres_data <- cbind(species = 1, pres_vals)

  bg <- sdm::background(env, n = nrow(pres_data), method = "gRandom")
  bg$species <- 0

  train <- rbind(dplyr::select(bg, -c("x", "y")), pres_data)

  sc <- list(
    scen1 = terra::rast(stars::st_warp(scen[1], crs = 6933)),
    scen2 = terra::rast(stars::st_warp(scen[2], crs = 6933)),
    scen3 = terra::rast(stars::st_warp(scen[3], crs = 6933)),
    scen4 = terra::rast(stars::st_warp(scen[4], crs = 6933))
  )

  d <- sdmData(
    formula = species ~ bio1 + bio4 + bio12,
    train = train
  )

  list(data = d, scen = sc)
}

# ---------------------------
# sdm - Processing
# ---------------------------

fit_sdm <- function(prep) {
  m <- sdm(
    formula = species ~ bio1 + bio4 + bio12,
    data = prep$data,
    methods = c("mlp", "rpart", "fda", "brt", "mars", "rf"),
    replication = "cv",
    cv.folds = 5
  )

  list(model = m, scen = prep$scen)
}

# ---------------------------
# sdm - Post-processing
# ---------------------------

post_sdm <- function(fit) {
  list(
    unweighted = lapply(fit$scen, function(r) {
      ensemble(fit$model,
        newdata = r,
        setting = list(method = "unweighted")
      )
    }),

    weighted = lapply(fit$scen, function(r) {
      ensemble(fit$model,
        newdata = r,
        setting = list(
          method = "weighted", stat = "AUC",
          expr = "auc > 0.5"
        )
      )
    }),

    pa = lapply(fit$scen, function(r) {
      ensemble(fit$model,
        newdata = r,
        setting = list(method = "pa", opt = 2)
      )
    })
  )
}

# ---------------------------
# sdm - Complete
# ---------------------------

run_sdm <- function() {
  coords <- occ[, c("decimalLongitude", "decimalLatitude")]
  names(coords) <- c("x", "y")

  env <- terra::rast(stars::st_warp(bioc, crs = 6933))
  sc <- list(
    scen1 = terra::rast(stars::st_warp(scen[1], crs = 6933)),
    scen2 = terra::rast(stars::st_warp(scen[2], crs = 6933)),
    scen3 = terra::rast(stars::st_warp(scen[3], crs = 6933)),
    scen4 = terra::rast(stars::st_warp(scen[4], crs = 6933))
  )

  pres_vals <- terra::extract(env, vect(coords))
  pres_vals <- pres_vals[, -1]
  pres_data <- cbind(species = 1, pres_vals)

  bg <- sdm::background(env, n = nrow(pres_data), method = "gRandom")
  bg$species <- 0

  train <- rbind(dplyr::select(bg, -c("x", "y")), pres_data)

  d <- sdmData(
    formula = species ~ bio1 + bio4 + bio12,
    train = train
  )

  m <- sdm(
    formula = species ~ bio1 + bio4 + bio12,
    data = d,
    methods = c("mlp", "rpart", "fda", "brt", "mars", "rf"),
    replication = "cv",
    cv.folds = 5
  )

  list(
    unweighted = lapply(sc, function(r) {
      ensemble(m,
        newdata = r,
        setting = list(method = "unweighted")
      )
    }),

    weighted = lapply(sc, function(r) {
      ensemble(m,
        newdata = r,
        setting = list(
          method = "weighted", stat = "AUC",
          expr = "auc > 0.5"
        )
      )
    }),

    pa = lapply(sc, function(r) {
      ensemble(m,
        newdata = r,
        setting = list(method = "pa", opt = 2)
      )
    })
  )
}
```

> **Note**: The `sdm` package does not natively accept externally
> defined folds. Therefore, although the number of folds is controlled,
> the exact split structure may differ slightly.

------------------------------------------------------------------------

### caretSDM

``` r

# ---------------------------
# caretSDM - Pre-processing
# ---------------------------

prep_caretSDM <- function() {
  sa <- sdm_area(bioc) |>
    add_scenarios(scen)
  oc <- occurrences_sdm(occ, occ_crs = 6933)
  input_sdm(oc, sa) |>
    pseudoabsences(method = "random", n_set = 1)
}

# ---------------------------
# caretSDM - Processing
# ---------------------------

fit_caretSDM <- function(prep) {
  prep |>
    train_sdm(
      algo = c("mlp", "rpart", "fda", "gbm", "gcvEarth", "rf"),
      tuneLength = 1,
      ctrl = caret::trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        returnResamp = "none",
        savePredictions = "final",
        summaryFunction = summary_sdm
      )
    )
}

# ---------------------------
# caretSDM - Post-processing
# ---------------------------

post_caretSDM <- function(fit) {
  fit |>
    predict_sdm(th = 0.5) |>
    ensemble_sdm(c("average", "weighted_average", "committee_average"))
}


# ---------------------------
# caretSDM - Complete
# ---------------------------
run_caretSDM <- function() {
  sa <- sdm_area(bioc) |>
    add_scenarios(scen)

  oc <- occurrences_sdm(occ, occ_crs = 6933)

  input_sdm(oc, sa) |>
    pseudoabsences(method = "random", n_set = 1) |>
    train_sdm(
      algo = c("mlp", "rpart", "fda", "gbm", "gcvEarth", "rf"),
      tuneLength = 1,
      ctrl = caret::trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        returnResamp = "none",
        savePredictions = "final",
        summaryFunction = summary_sdm
      )
    ) |>
    predict_sdm(th = 0.5) |>
    ensemble_sdm(c("average", "weighted_average", "committee_average"))
}
```

------------------------------------------------------------------------

## Benchmark execution

Before benchmarking, users are encouraged to run each function once to
avoid first-run overhead (e.g. package initialization).

``` r

prep_biomod2_res <- prep_biomod2()
prep_sdm_res <- prep_sdm()
prep_caretSDM_res <- prep_caretSDM()
```

``` r

bench_res_prep <- bench::mark(
  biomod2 = prep_biomod2(),
  sdm = prep_sdm(),
  caretSDM = prep_caretSDM(),
  iterations = 5,
  check = FALSE
)
```

``` r

fit_biomod2_res <- fit_biomod2(prep_biomod2_res)
fit_sdm_res <- fit_sdm(prep_sdm_res)
fit_caretSDM_res <- fit_caretSDM(prep_caretSDM_res)
```

``` r

bench_res_fit <- bench::mark(
  biomod2 = fit_biomod2(prep_biomod2_res),
  sdm = fit_sdm(prep_sdm_res),
  caretSDM = fit_caretSDM(prep_caretSDM_res),
  iterations = 5,
  check = FALSE
)
```

``` r

post_biomod2(fit_biomod2_res)
post_sdm(fit_sdm_res)
post_caretSDM(fit_caretSDM_res)
```

``` r

bench_res_post <- bench::mark(
  biomod2 = post_biomod2(fit_biomod2_res),
  sdm = post_sdm(fit_sdm_res),
  caretSDM = post_caretSDM(fit_caretSDM_res),
  iterations = 5,
  check = FALSE
)
```

``` r

bench_res_complete <- bench::mark(
  biomod2 = run_biomod2(),
  sdm = run_sdm(),
  caretSDM = run_caretSDM(),
  iterations = 5,
  check = FALSE
)
```

------------------------------------------------------------------------

## Results

``` r

bench_res_prep
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2    163.04ms  166.9ms     5.50     7.94MB    1.10 
#> 2 sdm        130.32ms 134.11ms     4.05     6.02MB    0.811
#> 3 caretSDM      1.62s    1.62s     0.611     7.2MB    1.22
```

``` r

bench_res_fit
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2       6.04s    6.08s    0.163   712.09MB    0.392
#> 2 sdm          16.53s   16.57s    0.0603    2.38GB    0.302
#> 3 caretSDM     19.75s    23.3s    0.0436  801.23MB    0.786
```

``` r

bench_res_post
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2       7.82s    7.83s    0.125    718.9MB    0.475
#> 2 sdm           17.1s   17.13s    0.0582   610.2MB    0    
#> 3 caretSDM   988.04ms    1.01s    0.995     44.7MB    0
```

``` r

bench_res_complete
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2       13.7s      14s    0.0687    1.37GB   0.439 
#> 2 sdm           39.7s    40.4s    0.0249    2.88GB   0.0846
#> 3 caretSDM      23.1s      26s    0.0367  859.31MB   0.778
```

The table above summarizes the median runtime, iteration rate, and
memory allocation for each package under identical conditions.

------------------------------------------------------------------------

## Interpretation

The benchmark results reveal clear differences in computational
performance across the evaluated SDM frameworks, with the magnitude of
these differences varying substantially among workflow stages.

### Pre-processing

During the preprocessing stage, both biomod2 and sdm completed data
preparation rapidly. In contrast, caretSDM required substantially more
time, despite still being a very low running time (\< 1s).

This difference primarily reflects the greater amount of internal data
structuring performed by caretSDM, including the creation of
standardized SDM objects, explicit pseudoabsence management, and
preparation of workflow metadata used in later modeling and prediction
steps. By contrast, biomod2 and sdm perform less structural
preprocessing, relying more directly on raw data objects, with sdm
package being the most efficient package in this stage.

### Model fitting

Model fitting is more computationally demanding. Here, biomod2 exhibited
the fastest performance, followed by sdm and caretSDM. Memory allocation
during this stage also differed substantially across packages. The sdm
package required the largest memory allocation, whereas caretSDM and
biomod2 used a more close amount, with caretSDM allocating slightly more
memory than biomod2.

These differences largely arise from how each framework orchestrates
model training. The modeling functions in biomod2 appear to implement
relatively efficient internal routines for coordinating algorithms and
cross-validation. In contrast, caretSDM relies on the training
infrastructure of the caret framework, which provides highly flexible
resampling and tuning capabilities but introduces additional overhead
during model training.

### Post-processing (projection and ensembling)

The package caretSDM excels during the post-processing stage, which is
known to be the stage that requires the highest amount of processing
time. The sdm and biomod2 packages did not differed in processing time,
with biomod2 allocating slightly more memory.

The improvement demonstrated by caretSDM is result of its Machine
Learning approach, where the package projects models built with complete
data and trained with optimized configurations infered from the internal
validation process. In the other hand, biomod2 and sdm packages project
all submodels generated through the pipeline, costing more memory and
time to obtain results during this stage.

### End-to-end workflow

When considering the complete workflow (preprocessing, model fitting,
and projection), caretSDM was the fastest framework, followed by the
biomod2 package and finally the sdm package. Memory usage followed the
same pattern, with caretSDM allocating the fewest memory, followed by
biomod2 and sdm.

### Overall implications

These results highlight that differences among SDM frameworks arise not
only from algorithm implementation but also from workflow architecture.
Packages such as biomod2 and sdm prioritize computational efficiency by
performing fewer intermediate abstractions and operating closer to the
underlying modeling functions. In contrast, caretSDM emphasizes workflow
standardization, object consistency, and integration with a general
machine-learning framework, which introduces additional computational
overhead but may provide advantages in reproducibility, extensibility,
and workflow transparency. Moreover, caretSDM also needed substantial
less lines of code to perform the same tasks, while also using only
functions from its own package. This may be an advantage for users who
prefer a more streamlined workflow or want to spend more coding time in
other stages of the modeling process.

During preprocessing, this benchmark avoided the use of multiple sets of
pseudoabsences to ensure that all packages were evaluated under
identical conditions. However, in practice, users may choose to generate
multiple sets of pseudoabsences to improve model robustness, which would
likely amplify the observed differences in preprocessing time among
packages. This approach was intentional due to the lack of easy way to
perform this task in sdm package, which would require users to implement
custom code to generate multiple sets of pseudoabsences and manage them
across modeling iterations. This would probably reflect more a
difference in user coding ability than in package performance, which is
not the focus of this benchmark.

The use of multiple sets of pseudoabsences would likely amplify the
observed differences in model fitting time among packages in the
processing step. In the same way, hyperparameter tuning was let out of
this benchmark to ensure that all packages were evaluated under
identical conditions. However, users may choose to perform tuning to
optimize model performance particularly when using caretSDM, which
provides more extensive and auto-tuning capabilities. As previously
stated, to include these methods in other packages would probably
reflect more a difference in user coding ability than in package
performance, which is not the focus of this benchmark.

Finally, in postprocessing, the differences between packages reflect the
design choices made by each framework regarding how projections and
ensemble predictions are managed. While the more structured approach of
caretSDM provides advantages in terms of workflow consistency and
downstream analysis, it introduces additional computational overhead
compared with the more direct implementations used in biomod2 and sdm.
Despite that, the projection of optimized models shrinks memory usage
and processing time, allowing for more robust projections (fine-tuned
models) to be obtained using the same amount of time and memory.

Moreover, considering the complete workflow, caretSDM excels in time and
memory management. Despite not being the most efficient package during
neither the pre-processing of data, due to internal standardization
functions, nor the processing of models, caretSDM accelerates the
post-processing step, through a Machine Learning approach.

------------------------------------------------------------------------

## Reproducibility

This benchmark is fully reproducible and can be executed on any system
with the required packages installed. Users are encouraged to rerun the
analysis on their own hardware to assess absolute performance
differences.

------------------------------------------------------------------------

## Limitations

- No parallel processing was used;
- Performance may vary with dataset size, predictor dimensionality, and
  hardware;
- If you want to see your package here or if you think I am missing some
  coding or function from sdm and/or biomod2, please contact me on
  \[authors name\]@gmail.com.

------------------------------------------------------------------------

## Session information

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] splines   stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] caret_7.0-1          lattice_0.22-9       ggplot2_4.0.3       
#>  [4] kernlab_0.9-33       glmnet_5.0           Matrix_1.7-5        
#>  [7] dismo_1.3-16         raster_3.6-32        sp_2.2-1            
#> [10] RSNNS_0.4-18         Rcpp_1.1.1-1.1       caretSDM_1.9.4      
#> [13] sdm_1.2-59           xgboost_3.2.1.1      randomForest_4.7-1.2
#> [16] maxnet_0.1.4         earth_5.3.5          plotmo_3.7.0        
#> [19] plotrix_3.8-14       Formula_1.2-5        gbm_2.2.3           
#> [22] mgcv_1.9-4           nlme_3.1-169         gam_1.22-7          
#> [25] foreach_1.5.2        mda_0.5-5            class_7.3-23        
#> [28] cito_1.1             rpart_4.1.27         nnet_7.3-20         
#> [31] biomod2_4.3-4-6      bench_1.1.4          sf_1.1-1            
#> [34] terra_1.9-27        
#> 
#> loaded via a namespace (and not attached):
#>   [1] RColorBrewer_1.1-3      torch_0.17.0            wk_0.9.5               
#>   [4] shape_1.4.6.1           ECDFniche_0.5           jsonlite_2.0.0         
#>   [7] magrittr_2.0.5          farver_2.1.2            CoordinateCleaner_3.0.1
#>  [10] rmarkdown_2.31          fs_2.1.0                ragg_1.5.2             
#>  [13] vctrs_0.7.3             htmltools_0.5.9         polynom_1.4-1          
#>  [16] s2_1.1.9                pROC_1.19.0.1           parallelly_1.47.0      
#>  [19] sass_0.4.10             KernSmooth_2.23-26      bslib_0.11.0           
#>  [22] htmlwidgets_1.6.4       desc_1.4.3              plyr_1.8.9             
#>  [25] stars_0.7-2             lubridate_1.9.5         cachem_1.1.0           
#>  [28] whisker_0.4.1           lifecycle_1.0.5         iterators_1.0.14       
#>  [31] pkgconfig_2.0.3         R6_2.6.1                fastmap_1.2.0          
#>  [34] future_1.70.0           digest_0.6.39           reshape_0.8.10         
#>  [37] ps_1.9.3                textshaping_1.0.5       timechange_0.4.0       
#>  [40] httr_1.4.8              abind_1.4-8             compiler_4.6.0         
#>  [43] proxy_0.4-29            withr_3.0.2             bit64_4.8.2            
#>  [46] S7_0.2.2                backports_1.5.1         DBI_1.3.0              
#>  [49] R.utils_2.13.0          ecospat_4.1.3           MASS_7.3-65            
#>  [52] lava_1.9.1              classInt_0.4-11         ggpp_0.6.0             
#>  [55] gtools_3.9.5            ModelMetrics_1.2.2.2    oai_0.4.0              
#>  [58] tools_4.6.0             units_1.0-1             otel_0.2.0             
#>  [61] rgbif_3.8.5             future.apply_1.20.2     R.oo_1.27.1            
#>  [64] glue_1.8.1              callr_3.7.6             profmem_0.7.0          
#>  [67] stringdist_0.9.17       grid_4.6.0              checkmate_2.3.4        
#>  [70] reshape2_1.4.5          generics_0.1.4          recipes_1.3.3          
#>  [73] gtable_0.3.6            R.methodsS3_1.8.2       tidyr_1.3.2            
#>  [76] data.table_1.18.4       utf8_1.2.6              xml2_1.5.2             
#>  [79] pillar_1.11.1           stringr_1.6.0           ggspatial_1.1.10       
#>  [82] dplyr_1.2.1             survival_3.8-6          bit_4.6.0              
#>  [85] tidyselect_1.2.1        coro_1.1.0              lemon_0.5.2            
#>  [88] knitr_1.51              gridExtra_2.3           stats4_4.6.0           
#>  [91] xfun_0.57               hardhat_1.4.3           checkCLI_1.0           
#>  [94] timeDate_4052.112       stringi_1.8.7           lazyeval_0.2.3         
#>  [97] yaml_2.3.12             evaluate_1.0.5          codetools_0.2-20       
#> [100] tibble_3.3.1            cli_3.6.6               systemfonts_1.3.2      
#> [103] processx_3.9.0          jquerylib_0.1.4         globals_0.19.1         
#> [106] PresenceAbsence_1.1.11  rnaturalearth_1.2.0     parallel_4.6.0         
#> [109] pkgdown_2.2.0           gower_1.0.2             listenv_0.10.1         
#> [112] ipred_0.9-15            scales_1.4.0            prodlim_2026.03.11     
#> [115] e1071_1.7-17            purrr_1.2.2             geosphere_1.6-8        
#> [118] rlang_1.2.0
```
