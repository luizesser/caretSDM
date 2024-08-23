set.seed(1)
sa <- sdm_area(parana, 0.1)
sa <- add_predictors(sa, bioc)
sa <- select(sa, c("bio01", "bio12"))
sa <- add_scenarios(sa)
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)
suppressWarnings(i <- pseudoabsences(i, method = "bioclim"))
test_that("predict_sdm - no model", {
  expect_error(predict_sdm(i))
})
suppressWarnings(i <- train_sdm(i, algo=c("svmLinear2", "mda", "nnet", "kknn")))

test_that("predict_sdm - errors", {
  expect_warning(predict_sdm(i, th=1))
  expect_error(predict_sdm(i, th="a"))
  expect_error(predict_sdm(i, metric="auc"))
  expect_error(predict_sdm(i, tp="a"))
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

  pred <- get_predictions(p)
  expect_true(all(names(pred$current$`Araucaria angustifolia`) %in%
               names(p$models$models$`Araucaria angustifolia`)))
  expect_true(all(pred$current$`Araucaria angustifolia`$m1.1$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio01", "bio12", "presence", "pseudoabsence", "geometry"),
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
  expect_equal(c("cell_id", "bio01", "bio12", "presence", "pseudoabsence", "geometry"),
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
  expect_true(all(pred$current$`Araucaria angustifolia`$m1.2$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio01", "bio12", "presence", "pseudoabsence", "geometry"),
               colnames(pred$current$`Araucaria angustifolia`$m1.2))
  expect_true(unique(st_geometry_type(pred$current$`Araucaria angustifolia`$m1.2)) == "POLYGON")
  expect_equal(st_crs(pred$current$`Araucaria angustifolia`$m1.2),
               st_crs(p$scenarios$data$current))
  expect_equal(st_bbox(pred$current$`Araucaria angustifolia`$m1.2),
               st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})



