test_that("correlate_sdm", {
  skip_on_cran()
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
  sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
  i <- input_sdm(oc, sa)
  i <- pseudoabsences(i, method="random", n_set=2)
  ctrl_sdm <- caret::trainControl(method = "boot",
                                  number = 1,
                                  repeats = 1,
                                  classProbs = TRUE,
                                  returnResamp = "all",
                                  summaryFunction = summary_sdm,
                                  savePredictions = "all")
  i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
    suppressWarnings()
  expect_error(correlate_sdm(i))
  i  <- predict_sdm(i, th = 0.8)
  expect_no_error(correlate_sdm(i))
  expect_error(correlate_sdm("i"))
  expect_error(correlate_sdm(i, scenario = "test"))
  expect_equal(names(correlate_sdm(i)), species_names(i))
  expect_equal(colnames(correlate_sdm(i)[[1]]), rownames(correlate_sdm(i)[[1]]))
  expect_equal(class(correlate_sdm(i)), c("list"))
  expect_equal(class(correlate_sdm(i)[[1]]), c("matrix", "array"))
  expect_equal(class(correlate_sdm(i)[[1]][,1]), c("numeric"))
  expect_equal(class(correlate_sdm(i)[[1]][,2]), c("numeric"))
})
