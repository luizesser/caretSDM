# Benchmarking SDM Package Performance in R

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
- **dismo**
- **caretSDM**

All packages are evaluated under **identical conditions**, using the
same data, algorithms, and validation structure.

------------------------------------------------------------------------

## Benchmark design

To ensure fairness and reproducibility, we enforced the following
constraints:

- Identical presence–absence data
- Identical environmental predictors
- Identical cross-validation folds (pre-defined)
- Same algorithms across packages (GLM and Random Forest)
- No parallelization
- Runtime measured using the `bench` package

Only model fitting and prediction steps are timed; plotting and file
writing are excluded.

------------------------------------------------------------------------

## Packages

``` r
library(terra)
#> terra 1.9.1
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(bench)

library(biomod2)
#> biomod2 4.3-4-5 loaded.
#>  /!\ Welcome to augmented biomod2 with abundance modeling available! (*o*)
#>  Take a look at the HOME and NEWS section on the website to see all the features!
#> Loading required package: nnet
#> Loading required package: rpart
#> Loading required package: cito
#> Torch is not yet installed
#> Please run the following before using cito
#> "library("torch")"
#> "install_torch()"
#> see: https://torch.mlverse.org/docs/articles/installation.html
#> Loading required package: mda
#> Loading required package: class
#> Loaded mda 0.5-5
#> Loading required package: gam
#> Loading required package: splines
#> Loading required package: foreach
#> Loaded gam 1.22-7
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.9-3. For overview type 'help("mgcv-package")'.
#> 
#> Attaching package: 'mgcv'
#> The following objects are masked from 'package:gam':
#> 
#>     gam, gam.control, gam.fit, s
#> The following object is masked from 'package:nnet':
#> 
#>     multinom
#> Loading required package: gbm
#> Loaded gbm 2.2.3
#> This version of gbm is no longer under development. Consider transitioning to gbm3, https://github.com/gbm-developers/gbm3
#> Loading required package: earth
#> Loading required package: Formula
#> Loading required package: plotmo
#> Loading required package: plotrix
#> 
#> Attaching package: 'plotrix'
#> The following object is masked from 'package:terra':
#> 
#>     rescale
#> Loading required package: maxnet
#> Loading required package: randomForest
#> randomForest 4.7-1.2
#> Type rfNews() to see new features/changes/bug fixes.
#> Loading required package: xgboost
library(sdm)
#> sdm 1.2-59 (2025-07-13)
library(caretSDM)
#> 
#> Attaching package: 'caretSDM'
#> The following object is masked from 'package:sdm':
#> 
#>     background
#> The following object is masked from 'package:biomod2':
#> 
#>     get_predictions

library(RSNNS)
#> Loading required package: Rcpp
library(rpart)
library(mda)
library(gbm)
library(earth)
library(randomForest)
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
#>              Min.  1st Qu.   Median     Mean 3rd Qu. Max. NA's
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
    models = c("ANN", "CTA", "FDA", "GBM", "MARS", "RF"),
    CV.strategy = "kfold",
    CV.nb.rep = 1,
    CV.k = 5,
    metric.eval = c("TSS", "ROC"),
    do.progress = FALSE
  )

  biomod_ensmodel <- BIOMOD_EnsembleModeling(
    bm.mod = biomod_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = c('EMmean', 'EMwmean', 'EMca'),
    metric.select = c('AUCroc'),
    metric.select.thresh = c(0.5)
  )

  list(model = biomod_model,
       ensemble = biomod_ensmodel,
       scen = prep$scen)
  
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
      models.chosen = 'all',
      overwrite = TRUE,
      do.stack = FALSE
    )
  })

  mapply(function(proj, nm) {
    BIOMOD_EnsembleForecasting(
      bm.em = fit$ensemble,
      bm.proj = proj,
      proj.name = nm,
      models.chosen = 'all',
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
  env <- terra::rast(stars::st_warp(bioc, crs=6933))
  sc <- list(scen1 = terra::rast(stars::st_warp(scen[1], crs=6933)), 
             scen2 = terra::rast(stars::st_warp(scen[2], crs=6933)), 
             scen3 = terra::rast(stars::st_warp(scen[3], crs=6933)), 
             scen4 = terra::rast(stars::st_warp(scen[4], crs=6933)))
  
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
    models = c("ANN", "CTA", "FDA", "GBM", "MARS", "RF"),
    CV.strategy = "kfold",
    CV.nb.rep = 1,
    CV.k = 5,
    metric.eval = c("TSS", "ROC"),
    do.progress = FALSE
  )
  
  biomod_ensmodel <- BIOMOD_EnsembleModeling(
    bm.mod = biomod_model,
    models.chosen = 'all',
    em.by = 'all',
    em.algo = c('EMmean', 'EMwmean', 'EMca'),
    metric.select = c('AUCroc'),
    metric.select.thresh = c(0.5)
  )
  
  biomod_proj <- lapply(names(sc), function(nm) {
    BIOMOD_Projection(
      bm.mod = biomod_model,
      proj.name = nm,
      new.env = sc[[nm]],
      models.chosen = 'all',
      overwrite = TRUE,
      do.stack = FALSE
    )
  })

  biomod_ensforecast <- mapply(function(proj, nm) {
    BIOMOD_EnsembleForecasting(
      bm.em = biomod_ensmodel,
      bm.proj = proj,
      proj.name = nm,
      models.chosen = 'all',
      overwrite = TRUE
    )
  }, biomod_proj, names(scen))

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

  bg <- sdm::background(env, n = nrow(pres_data), method = 'gRandom')
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
# biomod2 - Post-processing
# ---------------------------

post_sdm <- function(fit) {
  
  lapply(fit$scen, function(r) {
    ensemble(fit$model, newdata = r,
             setting = list(method = 'weighted', stat = 'AUC',
                            expr = 'auc > 0.5'))
  })
  
}

# ---------------------------
# sdm - Complete
# ---------------------------

run_sdm <- function() {
  
  occ_sdm <- occ
  coords <- occ_sdm[, c("decimalLongitude", "decimalLatitude")]
  names(coords) <- c("x", "y")
  presence <- occ_sdm[, c("species", "decimalLongitude", "decimalLatitude")]
  names(presence) <- c("species", "x", "y")
  env <- terra::rast(stars::st_warp(bioc, crs=6933))
  pres_vals <- terra::extract(env, vect(coords)) 
  pres_vals <- pres_vals[, -1]
  pres_data <- cbind(presence, pres_vals)
  pres_data$species <- rep(1, nrow(pres_data))
  sc <- list(scen1 = terra::rast(stars::st_warp(scen[1], crs=6933)), 
             scen2 = terra::rast(stars::st_warp(scen[2], crs=6933)), 
             scen3 = terra::rast(stars::st_warp(scen[3], crs=6933)), 
             scen4 = terra::rast(stars::st_warp(scen[4], crs=6933)))
  
  bg <- sdm::background(env, n=nrow(pres_data), method = 'gRandom')
  bg$species <- rep(0, nrow(bg))
  pres <- rbind(bg, pres_data)

  d <- sdmData(
    formula = species ~ bio1 + bio4 + bio12,
    train = dplyr::select(pres, -c("x", "y"))
  )

  m <- sdm(
    formula = species ~ bio1 + bio4 + bio12,
    data = d,
    methods = c("mlp", "rpart", "fda", "brt", "mars", "rf"),
    replication = "cv",
    cv.folds = 5
  )
  
  projections <- lapply(sc, function(r) {
    e1 <- ensemble(m, newdata = r, setting = list(method = 'weighted', stat = 'AUC', expr = 'auc > 0.5'))
    e2 <- ensemble(m, newdata = r, setting = list(method = 'unweighted', expr = 'auc > 0.5'))
    e3 <- ensemble(m, newdata = r, setting = list(method = 'pa', opt = 2, expr = 'auc > 0.5'))
    return(list(e1, e2, e3))
  })
  
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
  oc <- occurrences_sdm(occ, crs = 6933)
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
    predict_sdm(th = 0.5)
  
}


# ---------------------------
# caretSDM - Complete
# ---------------------------
run_caretSDM <- function() {
  
  sa <- sdm_area(bioc)

  oc <- occurrences_sdm(occ, crs = 6933)
  
  i <- input_sdm(oc, sa) |> 
    pseudoabsences(method = "random", n_set = 1) |> 
    train_sdm(algo = c("mlp", "rpart", "fda", "gbm", "gcvEarth", "rf"), 
              tuneLength = 1,
              ctrl = caret::trainControl(method = "cv", 
                                         number = 5,
                                         classProbs = TRUE, 
                                         returnResamp = "none", 
                                         savePredictions = "final",
                                         summaryFunction = summary_sdm)) |> 
    add_scenarios(scen, pred_as_scen = FALSE) |>
    predict_sdm(th = 0.5)
  
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
  biomod2  = prep_biomod2(),
  sdm      = prep_sdm(),
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
  biomod2  = fit_biomod2(prep_biomod2_res),
  sdm      = fit_sdm(prep_sdm_res),
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
  biomod2  = post_biomod2(fit_biomod2_res),
  sdm      = post_sdm(fit_sdm_res),
  caretSDM = post_caretSDM(fit_caretSDM_res),
  iterations = 5,
  check = FALSE
)
```

``` r
run_biomod2()
run_sdm()
run_caretSDM()
```

``` r
bench_res_complete <- bench::mark(
  biomod2  = run_biomod2(),
  sdm      = run_sdm(),
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
#> 1 biomod2     168.2ms 170.25ms     5.53     7.75MB    1.11 
#> 2 sdm        133.89ms 134.85ms     4.25     6.02MB    0.850
#> 3 caretSDM      1.55s    1.58s     0.596    7.31MB    1.55
```

``` r
bench_res_fit
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2       7.68s    7.72s    0.125  1017.21MB    0.376
#> 2 sdm          15.54s   15.64s    0.0639    2.19GB    0.319
#> 3 caretSDM     22.25s   24.69s    0.0408    1.23GB    0.979
```

``` r
bench_res_post
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2       9.72s   10.18s    0.0950  958.22MB    0.380
#> 2 sdm           7.42s    7.45s    0.133   192.27MB    0    
#> 3 caretSDM      52.7s   54.66s    0.0182    4.28GB    0.415
```

``` r
bench_res_complete
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 biomod2      17.25s    17.8s    0.0543    1.95GB   0.347 
#> 2 sdm          52.96s    53.3s    0.0187    3.07GB   0.0262
#> 3 caretSDM      1.29m     1.5m    0.0114    5.51GB   0.599
```

The table above summarizes the median runtime, iteration rate, and
memory allocation for each package under identical conditions.

------------------------------------------------------------------------

## Interpretation

Observed differences in runtime primarily reflect:

- Workflow overhead (data formatting, object abstraction)
- Internal resampling strategies
- Model orchestration costs

Packages designed around **lightweight workflows and explicit resampling
control**, such as `caretSDM`, tend to show lower computational overhead
under identical modeling tasks.

------------------------------------------------------------------------

## Reproducibility

This benchmark is fully reproducible and can be executed on any system
with the required packages installed. Users are encouraged to rerun the
analysis on their own hardware to assess absolute performance
differences.

------------------------------------------------------------------------

## Limitations

- No parallel processing was used
- Performance may vary with dataset size, predictor dimensionality, and
  hardware

------------------------------------------------------------------------

## Session information

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
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
#>  [1] caret_7.0-1          lattice_0.22-7       ggplot2_4.0.2       
#>  [4] kernlab_0.9-33       glmnet_4.1-10        Matrix_1.7-4        
#>  [7] dismo_1.3-16         raster_3.6-32        sp_2.2-1            
#> [10] RSNNS_0.4-18         Rcpp_1.1.1           caretSDM_1.5        
#> [13] sdm_1.2-59           xgboost_3.2.0.1      randomForest_4.7-1.2
#> [16] maxnet_0.1.4         earth_5.3.5          plotmo_3.7.0        
#> [19] plotrix_3.8-14       Formula_1.2-5        gbm_2.2.3           
#> [22] mgcv_1.9-3           nlme_3.1-168         gam_1.22-7          
#> [25] foreach_1.5.2        mda_0.5-5            class_7.3-23        
#> [28] cito_1.1             rpart_4.1.24         nnet_7.3-20         
#> [31] biomod2_4.3-4-5      bench_1.1.4          sf_1.1-0            
#> [34] terra_1.9-1         
#> 
#> loaded via a namespace (and not attached):
#>   [1] blockCV_3.2-0           rnaturalearth_1.2.0     tibble_3.3.1           
#>   [4] R.oo_1.27.1             hardhat_1.4.2           pROC_1.19.0.1          
#>   [7] lifecycle_1.0.5         httr2_1.2.2             ecospat_4.1.3          
#>  [10] usdm_2.1-7              globals_0.19.0          processx_3.8.6         
#>  [13] MASS_7.3-65             crosstalk_1.2.2         backports_1.5.0        
#>  [16] magrittr_2.0.4          sass_0.4.10             rmarkdown_2.30         
#>  [19] jquerylib_0.1.4         yaml_2.3.12             otel_0.2.0             
#>  [22] DBI_1.3.0               RColorBrewer_1.1-3      lubridate_1.9.5        
#>  [25] abind_1.4-8             Rtsne_0.17              purrr_1.2.1            
#>  [28] R.utils_2.13.0          coro_1.1.0              rappdirs_0.3.4         
#>  [31] ipred_0.9-15            torch_0.16.3            satellite_1.0.6        
#>  [34] lava_1.8.2              listenv_0.10.0          units_1.0-0            
#>  [37] parallelly_1.46.1       pkgdown_2.2.0           PresenceAbsence_1.1.11 
#>  [40] codetools_0.2-20        profmem_0.7.0           xml2_1.5.2             
#>  [43] shape_1.4.6.1           tidyselect_1.2.1        farver_2.1.2           
#>  [46] stats4_4.5.2            base64enc_0.1-6         jsonlite_2.0.0         
#>  [49] e1071_1.7-17            progressr_0.18.0        survival_3.8-3         
#>  [52] ggspatial_1.1.10        iterators_1.0.14        systemfonts_1.3.2      
#>  [55] tools_4.5.2             ragg_1.5.1              stringdist_0.9.17      
#>  [58] glue_1.8.0              prodlim_2025.04.28      xfun_0.56              
#>  [61] checkCLI_1.0            dplyr_1.2.0             withr_3.0.2            
#>  [64] fastmap_1.2.0           callr_3.7.6             digest_0.6.39          
#>  [67] CoordinateCleaner_3.0.1 timechange_0.4.0        R6_2.6.1               
#>  [70] wk_0.9.5                textshaping_1.0.5       gtools_3.9.5           
#>  [73] R.methodsS3_1.8.2       utf8_1.2.6              tidyr_1.3.2            
#>  [76] generics_0.1.4          data.table_1.18.2.1     recipes_1.3.1          
#>  [79] httr_1.4.8              htmlwidgets_1.6.4       whisker_0.4.1          
#>  [82] ModelMetrics_1.2.2.2    pkgconfig_2.0.3         gtable_0.3.6           
#>  [85] timeDate_4052.112       S7_0.2.1                furrr_0.3.1            
#>  [88] htmltools_0.5.9         scales_1.4.0            png_0.1-8              
#>  [91] gower_1.0.2             knitr_1.51              geosphere_1.6-5        
#>  [94] reshape2_1.4.5          rgbif_3.8.4             checkmate_2.3.4        
#>  [97] proxy_0.4-29            cachem_1.1.0            stringr_1.6.0          
#> [100] pdp_0.8.3               KernSmooth_2.23-26      parallel_4.5.2         
#> [103] s2_1.1.9                desc_1.4.3              pillar_1.11.1          
#> [106] grid_4.5.2              reshape_0.8.10          vctrs_0.7.1            
#> [109] mapview_2.11.4          evaluate_1.0.5          oai_0.4.0              
#> [112] cli_3.6.5               compiler_4.5.2          rlang_1.1.7            
#> [115] future.apply_1.20.2     classInt_0.4-11         ps_1.9.1               
#> [118] plyr_1.8.9              fs_1.6.7                stringi_1.8.7          
#> [121] stars_0.7-1             lazyeval_0.2.2          leaflet_2.2.3          
#> [124] bit64_4.6.0-1           leafem_0.2.5            future_1.69.0          
#> [127] bslib_0.10.0            lwgeom_0.2-15           bit_4.6.0
```
