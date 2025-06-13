set.seed(1)
sa <- sdm_area(parana, 0.1)
sa <- add_predictors(sa, bioc)
sa <- select(sa, c("bio1", "bio12"))
sa <- sa |> mutate(div=bio1/bio12, prod=bio1*bio12, sub=bio12-bio1, soma=bio1+bio12)
sa <- add_scenarios(sa)
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)
i_pca <- pca_predictors(i)
i_pca <- pseudoabsences(i_pca, method = "bioclim", variables_selected = "pca")
suppressWarnings(i_vif <- vif_predictors(i))
i_vif <- pseudoabsences(i_vif, method = "bioclim", variables_selected = "vif")
i <- pseudoabsences(i, method = "bioclim", variables_selected=c("bio1", "bio12"))

test_that("train_sdm", {
  suppressWarnings(i2 <- train_sdm(i,
                                   algo=c("kknn", "mda", "naive_bayes"),
                                   variables_selected = c("bio1", "bio12")))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"), algorithms_used(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio12") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i2)
})

test_that("train_sdm - pca", {
  suppressWarnings(i2 <- train_sdm(i_pca,
                                 algo=c("kknn", "mda", "naive_bayes"),
                                 variables_selected = "pca"))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"), algorithms_used(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(i2$predictors$variable_selection$pca$selected_variables %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i2)
})

test_that("train_sdm - change ctrl", {
  ctrl <- caret::trainControl(
    method = "boot", number = 10, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
    )
  suppressWarnings(i2 <- train_sdm(i_pca,
                                 algo=c("kknn", "mda", "naive_bayes"),
                                 variables_selected = "pca",
                                 ctrl=ctrl))
  expect_equal(10, length(unique(i2$models$models$`Araucaria angustifolia`$m1.1$resample$Resample)))
  expect_equal("boot", i2$models$validation$method)
  expect_snapshot(i2)
})

test_that("train_sdm - vif", {
  suppressWarnings(i2 <- train_sdm(i_vif,
                                  algo=c("naive_bayes", "mda", "kknn"),
                                  variables_selected = "vif"))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("naive_bayes", "mda", "kknn"), algorithms_used(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(i2$predictors$variable_selection$vif$selected_variables %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i2)
})

test_that("train_sdm - selecting vars", {
  suppressWarnings(i2 <- train_sdm(i, algo=c("naive_bayes", "mda", "kknn"),
                                  variables_selected=c("bio1", "bio12")))
  expect_true("models" %in% names(i2))
  expect_equal(10, get_tune_length(i2))
  expect_equal(c("naive_bayes", "mda", "kknn"), algorithms_used(i2))
  expect_equal(c("kknn", "mda", "naive_bayes"),
               unique(get_validation_metrics(i2)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i2)[[1]])))
  expect_true(all(c("bio1", "bio12") %in%
                    colnames(i2$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i2)
})

#test_that("train_sdm - independent_test", {
#  sa <- sdm_area(parana, 0.1)
#  sa <- add_predictors(sa, bioc)
#  sa <- select(sa, c("bio01", "bio12"))
#  sa <- add_scenarios(sa)
#  oc <- occurrences_sdm(occ, crs=6933, independent_test = TRUE)
#  suppressWarnings(oc <- join_area(oc, sa))
#  i <- input_sdm(oc, sa)
#  suppressWarnings(i <- pseudoabsences(i, method = "bioclim", variables_selected=c("bio01", "bio12")))
#  suppressWarnings(i <- train_sdm(i, algo=c("svmLinear2", "mda", "nnet", "kknn"), variables_selected=c("bio01", "bio12")))
#  expect_true("models" %in% names(i))
#  expect_equal(10, get_tune_length(i))
#  expect_equal(c("svmLinear2", "mda", "nnet", "kknn"), algorithms_used(i))
#  expect_equal(c("kknn", "mda", "nnet", "svmLinear2"),
#               unique(get_validation_metrics(i)[[1]][,"algo"]))
#  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i)[[1]])))
#  expect_true(all(c("bio01", "bio12") %in% colnames(i$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
#})
