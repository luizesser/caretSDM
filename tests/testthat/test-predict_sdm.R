set.seed(1)
sa <- sdm_area(parana, 100000, crs=6933) |>
  add_predictors(bioc) |>
  select_predictors(c("bio1", "bio12")) |>
  add_scenarios()
test_that("scenarios_names - sa", {
  expect_equal(names(sa$scenarios$data), scenarios_names(sa))
  expect_equal(get_scenarios_data(sa), sa$scenarios$data)
})
oc <- occurrences_sdm(occ, crs=6933)
test_that("scenarios_names - NULL", {
  expect_null(scenarios_names(oc))
  expect_null(get_scenarios_data(oc))
})
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)
suppressWarnings(i <- pseudoabsences(i, method = "bioclim", n_set = 3))
test_that("predict_sdm - no model", {
  expect_error(predict_sdm(i))
})
ctrl <- caret::trainControl(
  method = "cv", number = 2, classProbs = TRUE, returnResamp = "all",
  summaryFunction = caret::twoClassSummary, savePredictions = "all"
)
suppressWarnings(i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl))

test_that("predict_sdm - errors", {
  expect_error(predict_sdm(i, th=1))
  expect_error(predict_sdm(i, th="a"))
  expect_error(predict_sdm(i, metric="auc"))
  expect_error(predict_sdm(i, tp="a"))
  expect_error(predict_sdm(i, th=function(x){NULL}))
  expect_null(get_predictions(i))
  expect_null(get_ensembles(i))
  expect_null(get_predictions("i"))
  expect_null(get_ensembles("i"))
})

test_that("predict_sdm", {
  p <- predict_sdm(i)

  expect_snapshot(p)
  expect_snapshot(p$predictions)
  expect_true("predictions" %in% names(p))
  expect_true("ensembles" %in% names(p$predictions))

  expect_equal(get_scenarios_data(p), p$scenarios$data)

  pred <- get_predictions(p)
  expect_true(all(names(pred$current$`Araucaria angustifolia`) %in%
                    names(p$models$models$`Araucaria angustifolia`)))
  expect_true(all(pred$current$`Araucaria angustifolia`$m1.1$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"),
               colnames(pred$current$`Araucaria angustifolia`$m1.1))
  expect_true(unique(st_geometry_type(pred$current$`Araucaria angustifolia`$m1.1)) == "POLYGON")
  expect_equal(st_crs(pred$current$`Araucaria angustifolia`$m1.1),
               st_crs(p$scenarios$data$current))
  expect_equal(st_bbox(pred$current$`Araucaria angustifolia`$m1.1),
               st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("predict_sdm - th 0", {
  p <- predict_sdm(i, th=0)

  expect_snapshot(p)
  expect_snapshot(p$predictions)
  expect_true("predictions" %in% names(p))
  expect_true("ensembles" %in% names(p$predictions))

  pred <- get_predictions(p)
  expect_equal(names(pred$current$`Araucaria angustifolia`),
               names(p$models$models$`Araucaria angustifolia`))
  expect_true(all(pred$current$`Araucaria angustifolia`$m1.1$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"),
               colnames(pred$current$`Araucaria angustifolia`$m1.1))
  expect_true(unique(st_geometry_type(pred$current$`Araucaria angustifolia`$m1.1)) == "POLYGON")
  expect_equal(st_crs(pred$current$`Araucaria angustifolia`$m1.1),
               st_crs(p$scenarios$data$current))
  expect_equal(st_bbox(pred$current$`Araucaria angustifolia`$m1.1),
               st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("predict_sdm - th function", {
  p <- predict_sdm(i, th=mean)

  expect_snapshot(p)
  expect_snapshot(p$predictions)
  expect_true("predictions" %in% names(p))
  expect_true("ensembles" %in% names(p$predictions))

  pred <- get_predictions(p)
  expect_true(all(names(pred$current$`Araucaria angustifolia`) %in%
                    names(p$models$models$`Araucaria angustifolia`)))
  expect_true(all(pred$current$`Araucaria angustifolia`$m2.1$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"),
               colnames(pred$current$`Araucaria angustifolia`$m2.1))
  expect_true(unique(sf::st_geometry_type(pred$current$`Araucaria angustifolia`$m2.1)) == "POLYGON")
  expect_equal(st_crs(pred$current$`Araucaria angustifolia`$m2.1),
               st_crs(p$scenarios$data$current))
  expect_equal(st_bbox(pred$current$`Araucaria angustifolia`$m2.1),
               st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("test ensembles", {
  p <- predict_sdm(i)
  p2 <- get_predictions(p)
  expect_equal(length(p2$current[[1]]), 3)
})

test_that("test ensembles", {
  expect_no_error(predict_sdm(i, th=mean, ensembles = FALSE))

  i2 <- i
  i2$scenarios$data$teste <- i2$scenarios$data$current
  i2$scenarios$data$teste$bio12 <- i2$scenarios$data$teste$bio12*0
  i2$scenarios$data$teste$bio1 <- i2$scenarios$data$teste$bio1*0

  p <- predict_sdm(i2, th=mean)
  e <- get_ensembles(p)

  expect_equal(length(unique(e[,"teste"][[1]]$committee_avg)), 1)
  expect_equal(length(unique(e[,"teste"][[1]]$wmean_AUC)), 1)
  expect_equal(length(unique(e[,"teste"][[1]]$mean_occ_prob)), 1)
  expect_false(any(e[,"current"][[1]][,2] > 1 ))
  expect_false(any(e[,"current"][[1]][,3] > 1 ))
  expect_false(any(e[,"current"][[1]][,4] > 1 ))
  expect_false(any(e[,"current"][[1]][,2] < 0 ))
  expect_false(any(e[,"current"][[1]][,3] < 0 ))
  expect_false(any(e[,"current"][[1]][,4] < 0 ))
  expect_false(any(is.na(e[,"current"][[1]][,2])))
  expect_false(any(is.na(e[,"current"][[1]][,3])))
  expect_false(any(is.na(e[,"current"][[1]][,4])))
  expect_false(any(is.nan(e[,"current"][[1]][,2])))
  expect_false(any(is.nan(e[,"current"][[1]][,3])))
  expect_false(any(is.nan(e[,"current"][[1]][,4])))

})
