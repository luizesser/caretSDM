test_that("sdm_as_stars", {
  sa <- sdm_area(parana, 100000, crs=6933) |>
    add_predictors(bioc) |>
    select(c("bio1", "bio12"))

  expect_null(sdm_as_stars(sa))
  expect_null(sdm_as_terra(sa))
  expect_null(sdm_as_raster(sa))

  oc <- occurrences_sdm(occ, crs=6933)
  suppressWarnings(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  #predictors
  expect_equal(class(sdm_as_stars(i)), "stars")

  i <- add_scenarios(i)
  #scenarios
  expect_equal(class(sdm_as_stars(i)), "list")
  expect_equal(names(sdm_as_stars(i)), "current")
  expect_equal(class(sdm_as_stars(i)$current), "stars")

  suppressWarnings(i <- pseudoabsences(i, method = "bioclim", n_set = 3))
  ctrl <- caret::trainControl(
    method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
    summaryFunction = caret::twoClassSummary, savePredictions = "all"
  )
  suppressWarnings(i <- train_sdm(i,
                                  algo = c("naive_bayes", "kknn"),
                                  ctrl = ctrl))
  p <- predict_sdm(i, ensembles = FALSE)
  # predictions
  expect_equal(class(sdm_as_stars(p)), "stars")
  expect_true(all(c("cell_id", "presence", "pseudoabsence") %in% names(sdm_as_stars(p))))

  p <- predict_sdm(i)
  # ensembles
  expect_equal(class(sdm_as_stars(p)), "stars")
  expect_true(all(c("cell_id", "mean_occ_prob") %in% names(sdm_as_stars(p))))

  # what
  expect_equal(class(sdm_as_stars(p, what="predictors")), "stars")
  expect_equal(class(sdm_as_stars(p, what="scenarios")), "list")
  expect_equal(names(sdm_as_stars(p, what="scenarios")), "current")
  expect_equal(class(sdm_as_stars(p, what="scenarios")$current), "stars")
  expect_equal(class(sdm_as_stars(p, what="predictions")), "stars")
  expect_true(all(c("cell_id", "presence", "pseudoabsence") %in% names(sdm_as_stars(p, what="predictions"))))
  expect_equal(class(sdm_as_stars(p, what="ensembles")), "stars")
  expect_true(all(c("cell_id", "mean_occ_prob") %in% names(sdm_as_stars(p, what="ensembles"))))
})

