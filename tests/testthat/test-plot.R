set.seed(1)
sa <- sdm_area(parana, 1) |>
  add_predictors(bioc) |>
  select_predictors(c("bio1", "bio12")) |>
  add_scenarios()
suppressWarnings(oc <- occurrences_sdm(occ, crs=6933) |> join_area(sa))
test_that("plot works with i", {
  expect_no_error(plot(input_sdm(oc)))
})
i <- input_sdm(oc, sa)
test_that("plot works with i", {
  expect_no_error(plot(i))
})
suppressWarnings(i <- pseudoabsences(i, method = "random", n_set = 3))
ctrl_sdm <- caret::trainControl(method = "repeatedcv", number = 4, repeats = 2, classProbs = TRUE,
                                returnResamp = "all", summaryFunction = summary_sdm,
                                savePredictions = "all")
suppressWarnings(i <- train_sdm(i, algo=c("naive_bayes", "kknn"), crtl=ctrl_sdm))
i <- predict_sdm(i, th=0.5)

test_that("plot works with i", {
  expect_no_error(plot(i))
  expect_no_error(plot_grid(i))
  expect_no_error(mapview_grid(i))
  expect_no_error(plot_occurrences(i))
  expect_no_error(plot_occurrences(i, spp_name = "Araucaria angustifolia"))
  expect_error(plot_occurrences(i, spp_name = "a"))
  expect_no_error(mapview_occurrences(i))
  expect_no_error(mapview_occurrences(i, spp_name = "Araucaria angustifolia"))
  expect_error(mapview_occurrences(i, spp_name = "a"))
  expect_no_error(plot_predictors(i))
  expect_no_error(mapview_predictors(i))
  expect_no_error(plot_predictions(i))
  expect_no_error(plot_predictions(i, spp_name = "Araucaria angustifolia"))
  expect_error(plot_predictions(i, spp_name = "a"))
  expect_no_error(plot_predictions(i, scenario = "current"))
  #expect_error(plot_predictions(i, scenario = "a"))
  expect_no_error(plot_predictions(i, ensemble_type = "wmean_AUC"))
  expect_no_error(plot_predictions(i, ensemble_type = "committee_avg"))
  expect_error(plot_predictions(i, ensemble_type = "a"))
  expect_no_error(plot_predictions(i, ensemble = FALSE, id = "m1.1"))
  expect_no_error(plot_predictions(i, ensemble = FALSE, id = 1))
  expect_error(plot_predictions(i, ensemble = FALSE, id = "a"))
  expect_no_error(mapview_predictions(i))
  expect_no_error(mapview_predictions(i, spp_name = "Araucaria angustifolia"))
  expect_no_error(mapview_predictions(i, scenario = "current"))
  expect_no_error(mapview_predictions(i, ensemble_type = "wmean_AUC"))
  expect_no_error(mapview_predictions(i, ensemble_type = "committee_avg"))
  expect_error(mapview_predictions(i, ensemble_type = "a"))
  expect_no_error(mapview_predictions(i, ensemble = FALSE, id = "m1.1"))
  expect_no_error(mapview_predictions(i, ensemble = FALSE, id = 1))
  expect_error(mapview_predictions(i, ensemble = FALSE, id = "a"))
})

test_that("plot works with sa", {
  expect_no_error(plot_grid(sa))
  expect_no_error(mapview_grid(sa))
  expect_no_error(plot_predictors(sa))
  expect_no_error(mapview_predictors(sa))
})

test_that("plot works with oc", {
  expect_no_error(plot_occurrences(oc))
  expect_no_error(mapview_occurrences(oc))
})
