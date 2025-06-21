set.seed(1)
sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
sa <- add_predictors(sa, bioc)
sa <- select_predictors(sa, c("bio1", "bio12"))
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)
suppressWarnings(i <- pseudoabsences(i, method = "bioclim"))
ctrl <- caret::trainControl(
  method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
  summaryFunction = caret::twoClassSummary, savePredictions = "all"
)
suppressWarnings(i <- train_sdm(i, algo=c("naive_bayes", "kknn"), ctrl = ctrl))
test_that("varImp_sdm works", {
  v <- varImp_sdm(i)
  expect_equal(names(v), species_names(i))
  expect_equal(rownames(v[[1]]),  get_predictor_names(i))

  v <- varImp_sdm(i, id=paste0("m",1:10,".2"))
  expect_equal(names(v), species_names(i))
  expect_equal(rownames(v[[1]]),  get_predictor_names(i))

  expect_error(varImp_sdm("i"))
})
