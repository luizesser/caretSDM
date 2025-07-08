
set.seed(1)
sa <- sdm_area(parana, 100000, crs=6933)
sa <- add_predictors(sa, bioc)
sa <- select(sa, c("bio1", "bio12"))
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
  skip_on_cran()
  expect_snapshot(i2)
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
  skip_on_cran()
  expect_snapshot(i2)
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
  skip_on_cran()
  expect_snapshot(i2)
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
  skip_on_cran()
  expect_snapshot(i2)
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
  skip_on_cran()
  expect_snapshot(i2)
})

#test_that("train_sdm - ESM", {
#  set.seed(1)
#  sa <- sdm_area(parana, 100000, crs=6933)
#  sa <- add_predictors(sa, bioc)
#  sa <- select(sa, c("bio1", "bio4", "bio12"))
#  sa <- add_scenarios(sa)
#  oc <- occurrences_sdm(occ, crs=6933)
#  suppressWarnings(oc <- join_area(oc, sa))
#  i <- input_sdm(oc, sa)
#  i <- pseudoabsences(i, method = "random", n_set = 3)
#  i2 <- use_esm(i, spp = "Araucaria angustifolia")
#  ctrl <- caret::trainControl(
#    method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
#    summaryFunction = caret::twoClassSummary, savePredictions = "all"
#  )
#  suppressWarnings(i2 <- train_sdm(i2, algo=c("naive_bayes", "kknn"),
#                                   ctrl=ctrl))
#  expect_true("models" %in% names(i2))
#  expect_equal(10, get_tune_length(i2))
#  expect_equal(c("naive_bayes", "kknn"), algorithms_used(i2))
#  expect_equal(c("kknn", "naive_bayes"),
#               unique(get_validation_metrics(i2)[[1]][,"algo"]))
#  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
#  expect_true(all(c("bio1", "bio12") %in%
#                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
#  skip_on_cran()
#  expect_snapshot(i2)
#})

