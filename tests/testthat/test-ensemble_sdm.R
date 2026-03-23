test_that("ensembles_sdm", {
  skip_on_cran()
  set.seed(1)
  sa <- sdm_area(parana, 100000, crs=6933) |>
    add_predictors(bioc) |>
    select_predictors(c("bio1", "bio12")) |>
    add_scenarios()
  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  suppressWarnings(i <- pseudoabsences(i, method = "random", n_set = 3))
  ctrl <- caret::trainControl(
    method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
  )
  suppressWarnings(i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl))
  i <- predict_sdm(i, th = 0.8)

  i1 <- ensemble_sdm(i, method = "average")
  ens1 <- get_ensembles(i1)
  expect_equal(rownames(i1$ensembles$data), rownames(ens1))
  expect_equal(rownames(ens1), species_names(i1))
  expect_equal(colnames(ens1), scenarios_names(i1))
  expect_equal(c("cell_id", "average"), colnames(ens1[1,1][[1]]))
  expect_true(is.numeric(ens1[1,1][[1]][,"average"]))
  expect_true(is.numeric(ens1[1,1][[1]][,"cell_id"]))
  manual_calc <- lapply(get_predictions(i1)[[1]][[1]],
                        function(x) dplyr::select(as.data.frame(x), presence) )
  expect_equal(ens1[1,1][[1]][,2], rowMeans(do.call(cbind, manual_calc)))


  i2 <- ensemble_sdm(i, method = "weighted_average", metric = "ROC")
  ens2 <- get_ensembles(i2)
  expect_equal(rownames(i2$ensembles$data), rownames(ens2))
  expect_equal(rownames(ens2), species_names(i2))
  expect_equal(colnames(ens2), scenarios_names(i2))
  expect_equal(c("cell_id", "weighted_average"), colnames(ens2[1,1][[1]]))
  expect_true(is.numeric(ens2[1,1][[1]][,"weighted_average"]))
  expect_true(is.numeric(ens2[1,1][[1]][,"cell_id"]))

  i3 <- ensemble_sdm(i, method = "committee_average")
  ens3 <- get_ensembles(i3)
  expect_equal(rownames(i3$ensembles$data), rownames(ens3))
  expect_equal(rownames(ens3), species_names(i3))
  expect_equal(colnames(ens3), scenarios_names(i3))
  expect_equal(c("cell_id", "committee_average"), colnames(ens3[1,1][[1]]))
  expect_true(is.numeric(ens3[1,1][[1]][,"committee_average"]))
  expect_true(is.numeric(ens3[1,1][[1]][,"cell_id"]))


  i4 <- add_ensembles(i1,i2$ensembles)
  ens4 <- get_ensembles(i4)
  expect_equal(rownames(i4$ensembles$data), rownames(ens4))
  expect_equal(rownames(ens4), species_names(i4))
  expect_equal(colnames(ens4), scenarios_names(i4))
  expect_equal(c("cell_id", "average", "weighted_average"),
               colnames(ens4[1,1][[1]]))

  expect_false(any(ens1[,"current"][[1]][,2] > 1 ))
  expect_false(any(ens2[,"current"][[1]][,2] > 1 ))
  expect_false(any(ens3[,"current"][[1]][,2] > 1 ))
  expect_false(any(ens1[,"current"][[1]][,2] < 0 ))
  expect_false(any(ens2[,"current"][[1]][,2] < 0 ))
  expect_false(any(ens3[,"current"][[1]][,2] < 0 ))
  expect_false(any(is.na(ens1[,"current"][[1]][,2])))
  expect_false(any(is.na(ens2[,"current"][[1]][,2])))
  expect_false(any(is.na(ens3[,"current"][[1]][,2])))
  expect_false(any(is.nan(ens1[,"current"][[1]][,2])))
  expect_false(any(is.nan(ens2[,"current"][[1]][,2])))
  expect_false(any(is.nan(ens3[,"current"][[1]][,2])))

})
