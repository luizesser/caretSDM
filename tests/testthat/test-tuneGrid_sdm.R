test_that("tuneGrid_sdm", {
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
  expect_no_error(tuneGrid_sdm(i))
  expect_equal(class(tuneGrid_sdm(i)), "list")
  expect_equal(names(tuneGrid_sdm(i)), species_names(i))
  expect_equal(names(tuneGrid_sdm(i)[[1]]), c("m1.1", "m2.1"))
  expect_equal(names(tuneGrid_sdm(i)[[1]][[1]]), c("laplace", "usekernel", "adjust"))
  expect_equal(class(tuneGrid_sdm(i)[[1]][[1]][,1]), "numeric")
  expect_equal(class(tuneGrid_sdm(i)[[1]][[1]][,2]), "logical")
  expect_equal(class(tuneGrid_sdm(i)[[1]][[1]][,3]), "numeric")
  expect_error(tuneGrid_sdm("i"))
})
