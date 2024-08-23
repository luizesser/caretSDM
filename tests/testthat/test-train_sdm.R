test_that("train_sdm", {
  sa <- sdm_area(parana, 0.1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio01", "bio12"))
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  suppressWarnings(i <- pseudoabsences(i, method = "bioclim"))
  suppressWarnings(i <- train_sdm(i, algo=c("svmLinear2", "mda", "nnet", "kknn")))
  expect_true("models" %in% names(i))
  expect_equal(10, get_tune_length(i))
  expect_equal(c("svmLinear2", "mda", "nnet", "kknn"), algorithms_used(i))
  expect_equal(c("kknn", "mda", "nnet", "svmLinear2"),
               unique(get_validation_metrics(i)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i)[[1]])))
  expect_true(all(c("bio01", "bio12") %in% colnames(i$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i)
})

test_that("train_sdm - pca", {
  sa <- sdm_area(parana, 0.1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio01", "bio12"))
  sa <- sa |> mutate(div=bio01/bio12, prod=bio01*bio12, sub=bio12-bio01, soma=bio01+bio12)
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pca_predictors(i)
  expect_warning(i <- pseudoabsences(i, method = "bioclim", variables_selected = "pca"))
  suppressWarnings(i <- train_sdm(i,
                                 algo=c("svmLinear2", "mda", "nnet", "kknn"),
                                 variables_selected = "pca"))
  expect_true("models" %in% names(i))
  expect_equal(10, get_tune_length(i))
  expect_equal(c("svmLinear2", "mda", "nnet", "kknn"), algorithms_used(i))
  expect_equal(c("kknn", "mda", "nnet", "svmLinear2"),
               unique(get_validation_metrics(i)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i)[[1]])))
  expect_true(all(i$predictors$variable_selection$pca$selected_variables %in% colnames(i$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i)
})

test_that("train_sdm - change ctrl", {
  sa <- sdm_area(parana, 0.1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio01", "bio12"))
  sa <- sa |> mutate(div=bio01/bio12, prod=bio01*bio12, sub=bio12-bio01, soma=bio01+bio12)
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pca_predictors(i)
  expect_warning(i <- pseudoabsences(i, method = "bioclim", variables_selected = "pca"))
    ctrl <- caret::trainControl(
    method = "boot", number = 10, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
    )
  suppressWarnings(i <- train_sdm(i,
                                 algo=c("svmLinear2", "mda", "nnet", "kknn"),
                                 variables_selected = "pca",
                                 ctrl=ctrl))
  expect_equal(10, length(unique(i$models$models$`Araucaria angustifolia`$m1.1$resample$Resample)))
  expect_equal("boot", i$models$validation$method)
  expect_snapshot(i)
})

test_that("train_sdm - vif", {
  sa <- sdm_area(parana, 0.1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio01", "bio12"))
  sa <- sa |> mutate(div=bio01/bio12, prod=bio01*bio12, sub=bio12-bio01, soma=bio01+bio12)
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  suppressWarnings(i <- vif_predictors(i))
  expect_warning(i <- pseudoabsences(i, method = "bioclim", variables_selected = "vif"))
  suppressWarnings(i <- train_sdm(i,
                                  algo=c("svmLinear2", "mda", "nnet", "kknn"),
                                  variables_selected = "vif"))
  expect_true("models" %in% names(i))
  expect_equal(10, get_tune_length(i))
  expect_equal(c("svmLinear2", "mda", "nnet", "kknn"), algorithms_used(i))
  expect_equal(c("kknn", "mda", "nnet", "svmLinear2"),
               unique(get_validation_metrics(i)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i)[[1]])))
  expect_true(all(i$predictors$variable_selection$vif$selected_variables %in% colnames(i$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i)
})

test_that("train_sdm - selecting vars", {
  sa <- sdm_area(parana, 0.1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio01", "bio12"))
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  suppressWarnings(i <- pseudoabsences(i, method = "bioclim", variables_selected=c("bio01", "bio12")))
  suppressWarnings(i <- train_sdm(i, algo=c("svmLinear2", "mda", "nnet", "kknn"), variables_selected=c("bio01", "bio12")))
  expect_true("models" %in% names(i))
  expect_equal(10, get_tune_length(i))
  expect_equal(c("svmLinear2", "mda", "nnet", "kknn"), algorithms_used(i))
  expect_equal(c("kknn", "mda", "nnet", "svmLinear2"),
               unique(get_validation_metrics(i)[[1]][,"algo"]))
  expect_true(all(c("algo", "ROC") %in% colnames(get_validation_metrics(i)[[1]])))
  expect_true(all(c("bio01", "bio12") %in% colnames(i$models$models$`Araucaria angustifolia`$m1.1$trainingData)))
  expect_snapshot(i)
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
