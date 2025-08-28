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
suppressWarnings(i <- pseudoabsences(i, method = "random", n_set = 3))
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
  skip_on_cran()
  p <- predict_sdm(i, th = 0.5)
  expect_snapshot(p)
  expect_snapshot(p$predictions)
})

test_that("predict_sdm2", {
  p <- predict_sdm(i, th = 0.5)

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
  expect_true(unique(sf::st_geometry_type(pred$current$`Araucaria angustifolia`$m1.1)) == "POLYGON")
  expect_equal(sf::st_crs(pred$current$`Araucaria angustifolia`$m1.1),
               sf::st_crs(p$scenarios$data$current))
  expect_equal(sf::st_bbox(pred$current$`Araucaria angustifolia`$m1.1),
               sf::st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("predict_sdm - th 0", {
  skip_on_cran()
  p <- predict_sdm(i, th=0)
  expect_snapshot(p)
  expect_snapshot(p$predictions)
})

test_that("predict_sdm2 - th 0", {
  p <- predict_sdm(i, th=0)

  expect_true("predictions" %in% names(p))
  expect_true("ensembles" %in% names(p$predictions))

  pred <- get_predictions(p)
  expect_equal(names(pred$current$`Araucaria angustifolia`),
               names(p$models$models$`Araucaria angustifolia`))
  expect_true(all(pred$current$`Araucaria angustifolia`$m1.1$cell_id %in%
                    p$predictors$grid$cell_id))
  expect_equal(c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"),
               colnames(pred$current$`Araucaria angustifolia`$m1.1))
  expect_true(unique(sf::st_geometry_type(pred$current$`Araucaria angustifolia`$m1.1)) == "POLYGON")
  expect_equal(sf::st_crs(pred$current$`Araucaria angustifolia`$m1.1),
               sf::st_crs(p$scenarios$data$current))
  expect_equal(sf::st_bbox(pred$current$`Araucaria angustifolia`$m1.1),
               sf::st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("predict_sdm - th function", {
  skip_on_cran()
  p <- predict_sdm(i, th=mean)
  expect_snapshot(p)
  expect_snapshot(p$predictions)
  expect_error(predict_sdm(i, th=function(x){mean(x) +1}))

})

test_that("predict_sdm - th function", {
  p <- predict_sdm(i, th=mean)
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
  expect_equal(sf::st_crs(pred$current$`Araucaria angustifolia`$m2.1),
               sf::st_crs(p$scenarios$data$current))
  expect_equal(sf::st_bbox(pred$current$`Araucaria angustifolia`$m2.1),
               sf::st_bbox(p$scenarios$data$current))

  ens <- get_ensembles(p)
  expect_equal(rownames(p$predictions$ensembles), rownames(ens))
  expect_equal(rownames(ens), species_names(p))
  expect_equal(colnames(ens), scenarios_names(p))
  expect_equal(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"),
               colnames(ens[1,1][[1]]))
})

test_that("test ensembles", {
  p <- predict_sdm(i, th = 0.5)
  p2 <- get_predictions(p)
  expect_equal(length(p2$current[[1]]), 6)
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

test_that("add_input_sdm", {
  set.seed(1234)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    select_predictors(c("bio1", "bio12")) |>
    add_scenarios()
  oc <- occurrences_sdm(occ, crs = 6933) |>
                   join_area(sa)
  i_aa <- input_sdm(oc, sa) |>
    pseudoabsences(method="random", n_set=2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       repeats = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all")) |>
    suppressWarnings() |>
    predict_sdm(th = 0.6)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    select_predictors(c("bio1", "bio12")) |>
    add_scenarios()
  oc <- occurrences_sdm(salm, crs = 6933) |>
    join_area(sa)
  i_sb <- input_sdm(oc, sa) |>
    pseudoabsences(method="random", n_set=2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       repeats = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all")) |>
    suppressWarnings() |>
    predict_sdm(th = 0.5)
  expect_no_error(i <- add_input_sdm(i_aa, i_sb))
  expect_true(all(c("Araucaria angustifolia", "Salminus brasiliensis" ) %in% species_names(i)))
  expect_true(all(c("Araucaria angustifolia", "Salminus brasiliensis" ) %in% names(i$occurrences$pseudoabsences$data)))
  expect_true(all(c("Araucaria angustifolia", "Salminus brasiliensis" ) %in% names(i$models$models)))

  skip_on_cran()
  expect_no_error(p1 <- add_occurrences(i_sb$occurrences, i_aa$occurrences))
  expect_no_error(p2 <- add_sdm_area(i_sb$predictors, i_aa$predictors))
  expect_no_error(p3 <- add_sdm_area(i_sb$scenarios, i_aa$scenarios))
  expect_no_error(p4 <- add_models(i_sb$models, i_aa$models))
  expect_no_error(p5 <- add_predictions(i_sb$predictions, i_aa$predictions))
  expect_no_error(p6 <- add_predictions(i_sb$predictions, p2 = NULL))
  expect_no_error(p7 <- add_predictions(p1 = NULL, i_aa$predictions))
  expect_snapshot(p1)
  expect_snapshot(p2)
  expect_snapshot(p3)
  expect_snapshot(p4)
  expect_snapshot(p5)
  expect_snapshot(p6)
  expect_snapshot(p7)
})
