
set.seed(2)
sa <- sdm_area(parana, 100000, crs=6933)
sa <- add_predictors(sa, bioc)
sa <- select_predictors(sa, c("bio1", "bio12"))
sa <- add_scenarios(sa)
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)
i_pca <- pca_predictors(i)
i_pca <- pseudoabsences(i_pca, method = "random", n_p, variables_selected = "pca", n_set = 3)
suppressWarnings(i_vif <- vif_predictors(i))
i_vif <- pseudoabsences(i_vif, method = "random", variables_selected = "vif", n_set = 3)
i <- pseudoabsences(i, method = "random", variables_selected=c("bio1", "bio12"), n_set = 3)
ctrl <- caret::trainControl(
  method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
  summaryFunction = caret::twoClassSummary, savePredictions = "all"
)

test_that("train_sdm", {
  suppressWarnings(i2 <- train_sdm(i,
                                   algo=c("kknn", "naive_bayes"),
                                   variables_selected = c("bio1", "bio12"),
                                   ctrl=ctrl))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("kknn", "naive_bayes"), algorithms_used(i2))
  expect_equal(c("kknn", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio12") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})

test_that("train_sdm - pca", {
  suppressWarnings(i2 <- train_sdm(i_pca,
                                   algo=c("kknn", "naive_bayes"),
                                   variables_selected = "pca",
                                   ctrl=ctrl))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("kknn", "naive_bayes"), algorithms_used(i2))
  expect_equal(c("kknn", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(i2$predictors$variable_selection$pca$selected_variables %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})

test_that("train_sdm - vif", {
  suppressWarnings(i2 <- train_sdm(i_vif,
                                   algo=c("naive_bayes", "kknn"),
                                   variables_selected = "vif",
                                   ctrl = ctrl))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("naive_bayes", "kknn"), algorithms_used(i2))
  expect_equal(c("kknn", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(i2$predictors$variable_selection$vif$selected_variables %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})

test_that("train_sdm - change ctrl", {
  ctrl2 <- caret::trainControl(
    method = "boot", number = 10, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
  )
  suppressWarnings(i2 <- train_sdm(i_pca,
                                   algo=c("kknn", "naive_bayes"),
                                   variables_selected = "pca",
                                   ctrl=ctrl2))
  expect_equal(10, length(unique(i2$models$models$`Araucaria angustifolia`$m1.1$resample$Resample)))
  expect_equal("boot", i2$models$validation$method)
  #skip_on_cran()
  #expect_snapshot(i2)
})

test_that("train_sdm - selecting vars", {
  suppressWarnings(i2 <- train_sdm(i, algo=c("naive_bayes", "kknn"),
                                   variables_selected=c("bio1", "bio12"),
                                   ctrl=ctrl))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("naive_bayes", "kknn"), algorithms_used(i2))
  expect_equal(c("kknn", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio12") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})

test_that("train_sdm - ESM", {
  set.seed(2)
  sa <- sdm_area(parana, 100000, crs=6933)
  sa <- add_predictors(sa, bioc)
  sa <- select_predictors(sa, c("bio1", "bio4", "bio12"))
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pseudoabsences(i, method = "random", n_set = 3)
  i2 <- use_esm(i, spp = "Araucaria angustifolia")
  ctrl <- caret::trainControl(
    method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
  )
  suppressWarnings(i2 <- train_sdm(i2, algo=c("naive_bayes", "kknn"),
                                   ctrl=ctrl))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("naive_bayes", "kknn"), algorithms_used(i2))
  expect_equal(c("kknn", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio4") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})

mahal.custom <- list(
  label = "Mahalanobis Distance Classifier",
  library = NULL,
  type = "Classification",
  parameters = data.frame(
    parameter = c("abs"),
    class = c("logical"),
    label = c("Absolute Binarization")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    # We define a simple grid that will test both TRUE and FALSE
    # for the 'abs' parameter. Here, search can be anything that
    # the output is the same. But in other implementations the user
    # may want to change when search is not "grid".
    if (search == "grid") {
      out <- expand.grid(abs = c(TRUE, FALSE))
    } else {
      out <- expand.grid(abs = c(TRUE, FALSE))
    }
    return(out)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    # The 'fit' function is trained only on presence data.
    # It calculates and stores the mean vector and inverse covariance matrix.
    presence_data <- x[y == "presence", , drop = FALSE]

    if (nrow(presence_data) < 2) {
      stop("Not enough 'presence' data points to calculate covariance.")
    }

    # Calculate model parameters
    center_vec <- colMeans(presence_data, na.rm = TRUE)
    inv_cov_matrix <- solve(stats::cov(presence_data))

    # The model object here is just a list of parameters.
    result <- list(
      center = center_vec,
      inv_cov = inv_cov_matrix,
      df = ncol(x), # Correction demonstrated by Etherington 2019.
      abs = param$abs,
      levels = lev # Retain data information dor consistency.
    )
    return(result)
  },
  # Prediction function (must match caret's expected signature)
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # 'predict' generates class labels based on the probabilities.
    # 1. Get the probabilities by calling the 'prob' function.
    probs <- .mahal.dist$prob(modelFit, newdata)

    # 2. The 'abs' parameter determines the binarization type.
    if (modelFit$abs) {
      # For "Absolute Binarization", we threshold the p-value.
      # A common choice is alpha = 0.05. If p-value >= 0.05, the point is
      # considered within the "presence" environment.
      pred <- ifelse(probs[, modelFit$levels[1]] >= 0.05,
                     modelFit$levels[1], # presence
                     modelFit$levels[2]) # pseudoabsence
    } else {
      # Standard method: assign the class with the highest probability.
      pred <- colnames(probs)[apply(probs, 1, which.max)]
    }

    # 3. Return a factor with the correct levels.
    pred <- factor(pred, levels = modelFit$levels)
    return(pred)
  },

  predictors = function(x, ...) {
    # This correctly extracts predictor names from the fitted model.
    names(x$center)
  },

  # Optional: Specify if probabilities are supported
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # 'prob' calculates class probabilities using the fitted model.
    # 1. Calculate the squared Mahalanobis distance (D^2) for newdata.
    d2 <- stats::mahalanobis(x = newdata,
                             center = modelFit$center,
                             cov = modelFit$inv_cov,
                             inverted = TRUE) # Use inverted = TRUE for efficiency ######################

    # 2. Convert distance to a p-value using the chi-squared distribution.
    # This p-value can be interpreted as the probability of "presence".
    p_presence <- 1 - stats::pchisq(q = d2, df = modelFit$df)

    # 3. The output is a data frame of probabilities for both classes.
    prob_df <- data.frame(
      presence = p_presence,
      pseudoabsence = 1 - p_presence
    )
    colnames(prob_df) <- modelFit$levels # Ensure column names match levels
    return(prob_df)
  }
)


test_that("mahal.dist train", {
  skip_on_cran()
  sa <- sdm_area(parana,
                 cell_size = 50000, # Using a coarse resolution for speed
                 crs = 6933)
  sa <- add_predictors(sa, bioc)
  oc <- occurrences_sdm(occ, crs = 6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pseudoabsences(i,
                      method = "bioclim",
                      n_set = 3)
  ctrl_sdm <- caret::trainControl(method = "cv",
                                  number = 3,
                                  classProbs = TRUE,
                                  summaryFunction = summary_sdm,
                                  savePredictions = "final")
  i2 <- train_sdm(i,
                 algo = mahal.custom,
                 variables_selected = c("bio1", "bio4", "bio12"), # Using only two variables for simplicity
                 ctrl = ctrl_sdm) |>
    suppressWarnings()
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("mahal.custom"), algorithms_used(i2))
  expect_equal(c("mahal.custom"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio4", "bio12") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  #skip_on_cran()
  #expect_snapshot(i2)
})


test_that("train_sdm - one species ESM", {
  skip_on_cran()
  set.seed(2)
  sa <- sdm_area(parana,
                 cell_size = 25000,
                 crs = 6933,
                 gdal = T) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio4", "bio12"))
  oc <- occurrences_sdm(rbind(salm,occ), crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    data_clean() |>
    pseudoabsences(method = "bioclim") |>
    use_esm(n_records = 30)

  expect_no_error(i1 <- i |>
                    train_sdm(algo = mahal.custom,
                              variables_selected = c("bio1", "bio4", "bio12"), # Using only two variables for simplicity
                              ctrl = NULL) |>
                    suppressWarnings())
  expect_no_error(i2 <- i |>
                    train_sdm(algo = c("kknn"),
                              variables_selected = c("bio1", "bio4", "bio12"), # Using only two variables for simplicity
                              ctrl = NULL) |>
                    suppressWarnings())
  expect_true(algorithms_used(i1) == "mahal.custom")
  expect_true(algorithms_used(i2) == "kknn")
  #expect_snapshot(i1)
  #expect_snapshot(i2)
  #expect_snapshot(i1$models)
  #expect_snapshot(i2$models)
  expect_true(all(species_names(i1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(all(species_names(i2) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  m1 <- get_models(i1)
  m2 <- get_models(i2)
  expect_true(all(names(m1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(all(names(m2) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(length(m2$`Araucaria angustifolia`) == 10)
  expect_true(length(m2$`Salminus brasiliensis`) == 30)
  expect_true(length(m1$`Araucaria angustifolia`) == 10)
  expect_true(length(m1$`Salminus brasiliensis`) == 30)
  expect_true(m1$`Salminus brasiliensis`$m1.11$resampledCM$Resample[8] == "Fold4.Rep1")
  expect_true(m2$`Salminus brasiliensis`$m1.11$resampledCM$Resample[12] == "Fold4.Rep1")
})

test_that("train_sdm - independent data", {
  skip_on_cran()
  set.seed(2)
  sa <- sdm_area(parana,
                 cell_size = 25000,
                 crs = 6933,
                 gdal = T) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio4", "bio12"))
  oc <- occurrences_sdm(rbind(salm, occ), crs = 6933, independent_test = TRUE) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    data_clean() |>
    pseudoabsences(method = "bioclim")
  expect_no_error(i1 <- i |>
                    train_sdm(algo = c("kknn"),
                              variables_selected = c("bio1", "bio4", "bio12"), # Using only two variables for simplicity
                              ctrl = NULL) |>
                    suppressWarnings())
  expect_true(algorithms_used(i1) == "kknn")
  #expect_snapshot(i1)
  #expect_snapshot(i1$models)
  expect_true(all(species_names(i1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  m1 <- get_models(i1)
  expect_true(all(names(m1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(length(m1$`Araucaria angustifolia`) == 10)
  expect_true(length(m1$`Salminus brasiliensis`) == 10)
  expect_true("independent_validation" %in% names(i1$models))
  expect_true(all(species_names(i1) %in% names(i1$models$independent_validation)))
  expect_true(all(c("mean", "sd") %in% colnames(i1$models$independent_validation[[1]])))
})

# test maxent
test_that("train_sdm - maxent test", {
  skip_on_cran()
  set.seed(2)
  sa <- sdm_area(parana,
                 cell_size = 25000,
                 crs = 6933,
                 gdal = T) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio4", "bio12"))
  oc <- occurrences_sdm(salm, crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    pseudoabsences(method = "bioclim")
  expect_error(i1 <- i |>
                    train_sdm(algo = c("maxent"),
                              variables_selected = c("bio1", "bio4", "bio12"),
                              ctrl = NULL))
  i <- input_sdm(oc, sa) |>
    background() |>
    train_sdm(algo = c("maxent"),
              variables_selected = c("bio1", "bio4", "bio12"),
              ctrl = NULL) |>
    suppressWarnings()
  expect_true(algorithms_used(i) == "maxent")
  expect_true(all(species_names(i
                                ) == c("Salminus brasiliensis")))
  m1 <- get_models(i)
  expect_true(all(names(m1) == c("Salminus brasiliensis")))
  expect_true(length(m1$`Salminus brasiliensis`) == 1)
})

test_that("train_sdm - background and pseudoabsence algorithms", {
  skip_on_cran()
  set.seed(2)
  sa <- sdm_area(parana,
                 cell_size = 25000,
                 crs = 6933,
                 gdal = T) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio4", "bio12"))
  oc <- occurrences_sdm(rbind(salm, occ), crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    pseudoabsences(method = "bioclim") |>
    background()
  i1 <- train_sdm(i, algo = c("maxent", "kknn")) |>
    suppressWarnings()
  expect_true(all(algorithms_used(i1) == c("maxent", "kknn")))
  expect_true(all(species_names(i1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  m1 <- get_models(i1)
  expect_true(all(names(m1) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(length(m1$`Araucaria angustifolia`) == 11)
  expect_true(length(m1$`Salminus brasiliensis`) == 11)
})






