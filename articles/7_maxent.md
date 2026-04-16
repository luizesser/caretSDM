# 7. Applying MaxEnt in caretSDM

## Introduction

This vignette demonstrates how to fit a MaxEnt model for species
distribution modeling (SDM) using the `caretSDM` package. We use
*Araucaria angustifolia* occurrence data (`occ`) and bioclimatic
predictors over Paraná state (`bioc`) from the package.

MaxEnt estimates the probability distribution of species maximum entropy
constrained by environmental data at presence locations.

## Load Data

The package provides `occ` (*Araucaria* occurrences) and `bioc`
(bioclimatic rasters for Paraná).

``` r
# Occurrences
head(occ)
```

    ##                    species decimalLongitude decimalLatitude
    ## 327 Araucaria angustifolia         -4700678        -3065133
    ## 405 Araucaria angustifolia         -4711827        -3146727
    ## 404 Araucaria angustifolia         -4711885        -3147170
    ## 310 Araucaria angustifolia         -4717665        -3142767
    ## 49  Araucaria angustifolia         -4726011        -3148963
    ## 124 Araucaria angustifolia         -4727265        -3148517

``` r
# Bioclimatic data
bioc
```

    ## stars object with 3 dimensions and 1 attribute
    ## attribute(s):
    ##              Min.  1st Qu.   Median     Mean 3rd Qu. Max. NA's
    ## current  14.58698 21.19678 298.9147 622.9417  1353.5 2368 1845
    ## dimension(s):
    ##      from  to offset   delta refsys point              values x/y
    ## x     747 798   -180  0.1667 WGS 84 FALSE                NULL [x]
    ## y     670 706     90 -0.1667 WGS 84 FALSE                NULL [y]
    ## band    1   3     NA      NA     NA    NA bio1 , bio4 , bio12

## Run MaxEnt in `caretSDM`

To show how to run MaxEnt in `caretSDM`, we use an adaptation of the
article *1. Concatenate functions in caretSDM* example, but using an
terrestrial example. Before reading the code, it is fundamental to
understand a key difference between MaxEnt and other algorithms. MaxEnt
is a presence-background algorithm, while other methods applied in this
package can be either presence-only or presence-pseudoabsence
algorithms. In this sense, we implemented a separate function to
retrieve background data instead of pseudoabsences,
i.e. presence-background algorithms need to have background data, while
presence-pseudoabsence algorithms need to have pseudoabsence data.

``` r
sa <- sdm_area(bioc, 
               cell_size = 25000, 
               crs = 6933, 
               gdal = T) |>
  add_scenarios()

oc <- occurrences_sdm(occ, crs = 6933)

i <- input_sdm(oc, sa) |> 
  ### KEY DIFFERENCE: the background function. In other cases, here we should have a pseudoabsences function.
  background() |> 
  train_sdm(algo = c("maxent"), 
            ctrl = caret::trainControl(method = "repeatedcv", 
                                       number = 4, 
                                       repeats = 1, 
                                       classProbs = TRUE, 
                                       returnResamp = "all", 
                                       summaryFunction = summary_sdm, 
                                       savePredictions = "all")) |> 
  predict_sdm(th = 0.7)
i
```

    ##             caretSDM           
    ## ...............................
    ## Class                         : input_sdm
    ## --------  Occurrences  --------
    ## Species Names                 : Araucaria angustifolia 
    ## Number of presences           : 419 
    ## Background methods            :
    ##     Method to obtain BGs.     : random 
    ##     Number of Background sets : 1 
    ##     Number of Bg in each set  : 593 
    ##     Background proportion     : 1 
    ## --------  Predictors  ---------
    ## Number of Predictors          : 3 
    ## Predictors Names              : bio1, bio4, bio12 
    ## ---------  Scenarios  ---------
    ## Number of Scenarios           : 1 
    ## Scenarios Names               : current 
    ## -----------  Models  ----------
    ## Algorithms Names              : maxent 
    ## Variables Names               : bio1 bio4 bio12 
    ## Model Validation              :
    ##     Method                    : repeatedcv 
    ##     Number                    : 4 
    ##     Metrics                   :
    ## $`Araucaria angustifolia`
    ##     algo       ROC       TSS Sensitivity Specificity
    ## 1 maxent 0.8896861 0.5711794     0.76825     0.94275
    ## 
    ## --------  Predictions  --------
    ## Thresholds                    :
    ##     Method                    : threshold 
    ##     Criteria                  : 0.7

## The use of the `background` function

The `background` function is used in the SDM workflow to obtain
background data, a step necessary for MaxEnt algorithm to run. This
function helps avoid the use of pseudoabsence data in background
algorithms and the use of background data in pseudoabsence algorithms, a
very common mistake. The function works somewhat like the pseudoabsence
function, but with parameters slightly different and with different
standard values:

``` r
background(occ,
           pred = NULL,
           method = "random",
           n_set = 1,
           n_bg = 10000,
           proportion = NULL)
```

Parameter `occ` is usually the `input_sdm` object, where `pred` is also
retrieved from. Before that, main parameters are: `method`, which is the
method to obtain the background data (usually “random”); `n_set`, which
is the number of datasets of background data to create; `n_bg`, which is
the number of background records to be generated in each dataset created
(usually 10000); and `proportion`, which is a number between 0 and 1
representing a proportion of the area to be mapped as background,
e.g. if the whole area has 5000 cells and proportion is 0.1, then `n_bg`
is set to 500. This argument overwrites `n_bg`.

## Not using the correct approach

When the misconception of using pseudoabsences instead of background is
applied in the `caretSDM` workflow, the workflow stops in an error:

``` r
sa <- sdm_area(bioc, 
               cell_size = 25000, 
               crs = 6933, 
               gdal = T) |>
  add_scenarios()

oc <- occurrences_sdm(occ, crs = 6933)

i <- input_sdm(oc, sa) |> 
  ### Using the wrong function will return an error:
  pseudoabsences() |> 
  train_sdm(algo = c("maxent"), 
            ctrl = caret::trainControl(method = "repeatedcv", 
                                       number = 4, 
                                       repeats = 1, 
                                       classProbs = TRUE, 
                                       returnResamp = "all", 
                                       summaryFunction = summary_sdm, 
                                       savePredictions = "all"))
```

    ## Error in `train_sdm()`:
    ## ! `algo` contains maxent, but no background data is provided.
    ## ℹ Perhaps you have confused the concepts of pseudoabsence and background data.

## Implementation of MaxEnt in `caretSDM` workflow

As the `caretSDM` package is an extensible workflow, we applied the
broadly used MaxEnt through a different approach than other algorithms.
As it is not a standard method in `caret` package, we applied MaxEnt as
a new algorithm (see the article number 2: *Adding New Algorithms to
`caretSDM`*). To benefit from the `caret` package engine to perform
hiperparameters tuning, validation, and other tools, we implemented the
following object as a method within `caretSDM`’s `train_sdm` function.
Together with each piece of code, we included comments describing each
step of the process.

``` r
# Custom MaxEnt model to be implemented in caret:
#' @keywords internal
#' @importFrom maxnet maxnet
.maxent <- list(
  label = "Maximum Entropy Modeling",
  library = "maxnet",
  type = "Classification",

  parameters = data.frame(
    parameter = c("regmult", "linear", "quadratic", "product", "threshold", "hinge"),
    class = rep(c("numeric", "logical"), c(1, 5)),
    label = c("Regularization Multiplier", "Linear", "Quadratic", "Product", "Threshold", "Hinge")
  ),

  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      # Define sensible default feature combinations
      feature_combos <- expand.grid(
        linear = c(TRUE, FALSE),
        quadratic = c(TRUE, FALSE),
        product = c(TRUE, FALSE),
        threshold = c(TRUE, FALSE),
        hinge = c(TRUE, FALSE)
      )
      # Filter for common/useful combinations to avoid an excessively large grid
      # This keeps at least one feature type active, prioritizing linear and hinge
      sensible_combos <- subset(feature_combos, linear | hinge)

      # Create the full grid of all hyperparameters
      full_grid <- expand.grid(
        regmult = seq(0.5, 4, by = 0.5),
        combo_id = 1:nrow(sensible_combos)
      )

      # Merge to get the final hyperparameter grid
      final_grid <- cbind(regmult = full_grid$regmult, sensible_combos[full_grid$combo_id, ])

      # Sample `len` rows from the grid if `len` is specified
      if (!is.null(len) && len < nrow(final_grid)) {
        out <- final_grid[sample(nrow(final_grid), len), ]
      } else {
        out <- final_grid
      }

    } else { # Random search
      out <- data.frame(
        regmult = runif(len, 0.1, 5),
        linear = sample(c(TRUE, FALSE), len, replace = TRUE, prob = c(0.9, 0.1)),
        quadratic = sample(c(TRUE, FALSE), len, replace = TRUE, prob = c(0.6, 0.4)),
        product = sample(c(TRUE, FALSE), len, replace = TRUE, prob = c(0.4, 0.6)),
        threshold = sample(c(TRUE, FALSE), len, replace = TRUE, prob = c(0.3, 0.7)),
        hinge = sample(c(TRUE, FALSE), len, replace = TRUE, prob = c(0.7, 0.3))
      )
    }
    # Ensure no duplicate rows are returned
    return(unique(out))
  },

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    # Handle the response variable correctly
    # y is the response vector, x is the predictor data.frame
    # Convert factor response to binary (1 for presence, 0 for pseudoabsence/background)
    if (is.factor(y)) {
      p <- as.integer(y == lev[1])
    } else {
      p <- as.integer(y)
    }

    # Ensure x is a data.frame (caret sometimes passes matrices)
    if (is.matrix(x)) {
      data <- as.data.frame(x)
    } else if (is.data.frame(x)) {
      data <- x
    } else {
      # Convert other types to data.frame
      data <- as.data.frame(x)
    }

    # Efficiently determine feature classes from logical parameters
    feature_params <- names(param)[sapply(param, is.logical)]
    classes <- feature_params[unlist(param[feature_params])]

    # Create the classes string for maxnet
    if (length(classes) > 0) {
      classes_str <- paste(substr(classes, 1, 1), collapse = "")
    } else {
      classes_str <- "l"  # Default to linear if no classes selected
    }

    # Handle potential errors in maxnet fitting
    tryCatch({
      model <- maxnet::maxnet(
        p = p,
        data = data,
        f = maxnet::maxnet.formula(p, data = data, classes = classes_str),
        regmult = param$regmult,
        ...
      )

      # Store the response levels for later use
      model$obsLevels <- lev
      return(model)

    }, error = function(e) {
      # If maxnet fails, return a simple model structure that won't break caret
      warning(paste("MaxNet fitting failed:", e$message))
      dummy_model <- list(
        obsLevels = lev,
        failed = TRUE,
        error_message = e$message
      )
      class(dummy_model) <- "maxnet"
      return(dummy_model)
    })
  },

  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # Handle failed models
    if (!is.null(modelFit$failed) && modelFit$failed) {
      # Return predictions of the most common class
      return(factor(rep(modelFit$obsLevels[1], nrow(newdata)),
                    levels = modelFit$obsLevels))
    }

    # Ensure newdata is a data.frame
    if (is.matrix(newdata)) {
      newdata <- as.data.frame(newdata)
    }

    # For classification, predict the class with the highest probability
    probs <- .maxent$prob(modelFit, newdata)

    # Handle case where prob function might fail
    if (is.null(probs) || ncol(probs) < 2) {
      return(factor(rep(modelFit$obsLevels[1], nrow(newdata)),
                    levels = modelFit$obsLevels))
    }

    # Return the name of the column with the highest probability for each row
    factor(modelFit$obsLevels[apply(probs, 1, which.max)],
           levels = modelFit$obsLevels)
  },

  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # Handle failed models
    if (!is.null(modelFit$failed) && modelFit$failed) {
      # Return equal probabilities for all classes
      n_obs <- nrow(newdata)
      n_classes <- length(modelFit$obsLevels)
      prob_df <- as.data.frame(matrix(1/n_classes, nrow = n_obs, ncol = n_classes))
      colnames(prob_df) <- modelFit$obsLevels
      return(prob_df)
    }

    # Ensure newdata is a data.frame
    if (is.matrix(newdata)) {
      newdata <- as.data.frame(newdata)
    }

    # Add error handling for prediction
    tryCatch({
      # Use type = "logistic" to get probabilities in the [0, 1] range
      pred_probs <- as.numeric(predict(modelFit, newdata, type = "logistic"))

      # Structure the output as a data.frame with columns named after the outcome levels
      prob_df <- data.frame(
        class1 = pred_probs,
        class2 = 1 - pred_probs
      )
      # Name columns dynamically based on the levels seen during training
      colnames(prob_df) <- modelFit$obsLevels
      return(prob_df)

    }, error = function(e) {
      # If prediction fails, return equal probabilities
      warning(paste("MaxNet prediction failed:", e$message))
      n_obs <- nrow(newdata)
      n_classes <- length(modelFit$obsLevels)
      prob_df <- as.data.frame(matrix(0.5, nrow = n_obs, ncol = n_classes))
      colnames(prob_df) <- modelFit$obsLevels
      return(prob_df)
    })
  },

  varImp = function(object, lambda = NULL, normalize = TRUE, ...) {

    # Handle failed models
    if (!is.null(object$failed) && object$failed) {
      return(NULL)
    }

    # Check coefficients
    if (is.null(object$betas) || length(object$betas) == 0) {
      return(NULL)
    }

    # Extract coefficients
    coefs <- object$betas

    # Remove zero coefficients (important for LASSO sparsity)
    coefs <- coefs[coefs != 0]

    if (length(coefs) == 0) return(NULL)

    # --------------------------------------------
    # STEP 1 — Map features → original variables
    # --------------------------------------------

    # Feature names
    feat_names <- names(coefs)

    # Extract base variable names robustly
    # Handles:
    #   var
    #   var_l1
    #   var_hinge(...)
    #   var:interaction
    extract_var <- function(x) {
      # Case 1: hinge(), threshold(), etc.
      x <- sub(".*\\(([^)]+)\\).*", "\\1", x)

      # Case 2: interactions (bio1:bio2 → bio1)
      x <- sub(":.*", "", x)

      # Case 3: suffixes (_l1, _l4, etc.)
      x <- sub("_.*", "", x)

      return(x)
    }

    base_vars <- vapply(feat_names, extract_var, character(1))

    #base_vars <- sub("([:]._.*$)|(:.*$)", "", feat_names)

    # --------------------------------------------
    # STEP 2 — Aggregate importance
    # --------------------------------------------

    imp <- tapply(abs(coefs), base_vars, sum)

    # --------------------------------------------
    # STEP 3 — Optional penalty correction
    # --------------------------------------------

    if (!is.null(object$penalty.factor)) {
      pf <- object$penalty.factor

      # Align penalty factors with variables
      pf_vars <- pf[names(pf) %in% names(imp)]

      # Avoid division by zero
      pf_vars[pf_vars == 0] <- 1

      # Reweight importance (experimental but useful)
      imp[names(pf_vars)] <- imp[names(pf_vars)] / pf_vars
    }

    # --------------------------------------------
    # STEP 4 — Normalize
    # --------------------------------------------

    if (normalize) {
      imp <- imp / sum(imp)
    }

    # --------------------------------------------
    # STEP 5 — Format for caret
    # --------------------------------------------

    imp_df <- data.frame(Overall = imp)
    imp_df <- imp_df[order(-imp_df$Overall), , drop = FALSE]

    return(imp_df)
  },

  levels = function(x) x$obsLevels,
  tags = c("maxent", "Presence-Background")
)
```
