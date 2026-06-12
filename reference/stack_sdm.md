# Train a Stacked Ensemble for SDM

This function builds a meta-model (Layer 2) using the out-of-fold
predictions from models trained in Layer 1.

## Usage

``` r
stack_sdm(m, meta_algo = "glm", ctrl = NULL, ...)
```

## Arguments

- m:

  A `models` or `input_sdm` object.

- meta_algo:

  A character string specifying the algorithm for the meta-learner.

- ctrl:

  A `trainControl` object for the meta-learner. If NULL, a simple CV is
  used.

- ...:

  Additional arguments passed to
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

## Value

A `stacked_models` object.

## See also

[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`algorithms`](https://luizesser.github.io/caretSDM/reference/algorithms.md)` `[`train_sdm`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 100000, output_crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa)

# Create occurrences:
oc <- occurrences_sdm(occ, occ_crs = 6933)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Pseudoabsence generation:
i <- pseudoabsences(i, method = "random")

# Custom trainControl:
ctrl_sdm <- caret::trainControl(method = "repeatedcv",
                                number = 2,
                                repeats = 1,
                                classProbs = TRUE,
                                returnResamp = "all",
                                summaryFunction = summary_sdm,
                                savePredictions = "all")

# Train models:
i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl_sdm) |>
suppressWarnings()

# Train stacked ensemble:
i <- stack_sdm(i, meta_algo = "nnet", ctrl = ctrl_sdm)
#> Training meta-learner for: Araucaria angustifolia
#> # weights:  23
#> initial  value 108.642769 
#> iter  10 value 55.044376
#> iter  20 value 55.039419
#> iter  30 value 54.904800
#> iter  40 value 54.831573
#> iter  50 value 54.830748
#> iter  50 value 54.830748
#> final  value 54.830748 
#> converged
#> # weights:  67
#> initial  value 104.057657 
#> iter  10 value 54.159274
#> iter  20 value 42.428457
#> iter  30 value 32.451186
#> iter  40 value 30.569141
#> iter  50 value 29.327606
#> iter  60 value 28.721296
#> iter  70 value 27.735747
#> iter  80 value 24.412101
#> iter  90 value 23.631049
#> iter 100 value 23.582697
#> final  value 23.582697 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 136.302818 
#> final  value 55.039827 
#> converged
#> # weights:  23
#> initial  value 259.093772 
#> iter  10 value 57.464543
#> iter  20 value 54.677755
#> iter  30 value 48.329168
#> iter  40 value 48.219007
#> final  value 48.219006 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 95.862805 
#> iter  10 value 53.672253
#> iter  20 value 46.998737
#> iter  30 value 45.956958
#> iter  40 value 45.747951
#> iter  50 value 45.738336
#> final  value 45.738223 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 80.379815 
#> iter  10 value 48.273541
#> iter  20 value 45.740333
#> iter  30 value 45.686786
#> iter  40 value 45.658320
#> iter  50 value 45.623006
#> iter  60 value 45.604657
#> iter  70 value 45.591284
#> final  value 45.590435 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 140.760200 
#> iter  10 value 55.046307
#> iter  20 value 55.042355
#> iter  30 value 55.036693
#> iter  40 value 54.910747
#> iter  50 value 54.844936
#> iter  60 value 54.836545
#> iter  70 value 54.768774
#> iter  80 value 54.647594
#> iter  90 value 54.646010
#> iter 100 value 54.643834
#> final  value 54.643834 
#> stopped after 100 iterations
#> # weights:  67
#> initial  value 125.553887 
#> iter  10 value 55.136117
#> iter  20 value 55.054753
#> iter  30 value 54.883424
#> iter  40 value 54.864862
#> iter  50 value 53.369409
#> iter  60 value 35.249814
#> iter  70 value 33.586052
#> iter  80 value 32.499708
#> iter  90 value 31.347759
#> iter 100 value 30.393129
#> final  value 30.393129 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 235.925316 
#> iter  10 value 55.796976
#> iter  20 value 55.060308
#> iter  30 value 54.927287
#> iter  40 value 44.882946
#> iter  50 value 32.724838
#> iter  60 value 29.740715
#> iter  70 value 29.059700
#> iter  80 value 28.766945
#> iter  90 value 28.651619
#> iter 100 value 28.589905
#> final  value 28.589905 
#> stopped after 100 iterations
#> # weights:  23
#> initial  value 108.855286 
#> iter  10 value 57.788110
#> iter  20 value 57.784558
#> iter  30 value 57.673793
#> iter  40 value 35.347278
#> iter  50 value 21.441044
#> iter  60 value 19.890835
#> iter  70 value 19.881146
#> final  value 19.881108 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 127.166083 
#> iter  10 value 57.787072
#> iter  10 value 57.787072
#> iter  10 value 57.787072
#> final  value 57.787072 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 267.311242 
#> iter  10 value 57.819291
#> iter  20 value 57.787647
#> final  value 57.786883 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 143.798978 
#> iter  10 value 41.771414
#> iter  20 value 39.359303
#> final  value 39.334479 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 117.661565 
#> iter  10 value 43.630621
#> iter  20 value 37.518490
#> iter  30 value 36.850078
#> iter  40 value 36.766183
#> iter  50 value 36.719618
#> final  value 36.719575 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 115.631426 
#> iter  10 value 42.874248
#> iter  20 value 37.625991
#> iter  30 value 36.897484
#> iter  40 value 36.797548
#> iter  50 value 36.749645
#> iter  60 value 36.710664
#> iter  70 value 36.692043
#> iter  80 value 36.612113
#> iter  90 value 36.558735
#> iter 100 value 36.517680
#> final  value 36.517680 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 184.386788 
#> iter  10 value 57.794010
#> iter  20 value 57.791405
#> iter  30 value 50.499946
#> iter  40 value 34.219155
#> iter  50 value 33.966819
#> iter  60 value 33.821201
#> iter  70 value 33.791153
#> iter  80 value 33.783653
#> iter  90 value 33.780123
#> iter 100 value 33.777222
#> final  value 33.777222 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 205.627231 
#> iter  10 value 54.543604
#> iter  20 value 29.932008
#> iter  30 value 27.796186
#> iter  40 value 22.307523
#> iter  50 value 19.951554
#> iter  60 value 19.455105
#> iter  70 value 19.053832
#> iter  80 value 17.578168
#> iter  90 value 17.427921
#> iter 100 value 17.251758
#> final  value 17.251758 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 163.664029 
#> iter  10 value 44.144007
#> iter  20 value 23.952789
#> iter  30 value 23.553007
#> iter  40 value 23.447256
#> iter  50 value 23.345208
#> iter  60 value 23.262434
#> iter  70 value 22.627157
#> iter  80 value 19.944006
#> iter  90 value 17.106841
#> iter 100 value 15.104620
#> final  value 15.104620 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> Warning: There were missing values in resampled performance measures.
#> # weights:  23
#> initial  value 402.841770 
#> iter  10 value 110.395272
#> iter  20 value 84.986331
#> iter  30 value 84.492639
#> final  value 84.492218 
#> converged
```
