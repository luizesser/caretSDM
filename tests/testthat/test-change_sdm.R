test_that("change_sdm", {
  skip_on_cran()
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
  sa <- add_predictors(sa, bioc)
  sa <- add_scenarios(sa, scen) |>
    select_predictors(c("bio1", "bio12")) |>
    suppressWarnings()
  oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
  i <- input_sdm(oc, sa)
  i <- pseudoabsences(i, method="random", n_set = 2)
  ctrl_sdm <- caret::trainControl(method = "boot",
                                  number = 1,
                                  classProbs = TRUE,
                                  returnResamp = "all",
                                  summaryFunction = summary_sdm,
                                  savePredictions = "all")
  i <- train_sdm(i,
                 algo = c("naive_bayes"),
                 ctrl=ctrl_sdm,
                 variables_selected = c("bio1", "bio12")) |>
    suppressWarnings()
  i  <- predict_sdm(i, th=0.8)
  i <- gcms_ensembles(i, gcms = c("ca", "mi"))
  expect_no_error(prediction_change_sdm(i, scenario = "_ssp585_2090", ensemble_type = "mean_occ_prob"))
  expect_no_error(prediction_change_sdm(i, scenario = NULL, ensemble_type = NULL, species = NULL, th = 0.5))
  expect_error(prediction_change_sdm("i"))
  expect_error(prediction_change_sdm(i, scenario = "test"))
  expect_error(prediction_change_sdm(i, ensemble_type = "test"))
  expect_error(prediction_change_sdm(i, species = "test"))
  expect_error(prediction_change_sdm(i, th = "test"))
  expect_equal(class(prediction_change_sdm(i))[1], "ggplot2::ggplot")
})
