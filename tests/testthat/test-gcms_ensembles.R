test_that("gcms_ensembles/names", {
  skip_on_cran()
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
  sa <- add_predictors(sa, bioc)
  # name changes
  pred_names <- c("a", "b", "c", "d", "bio1", "bio4", "bio12")
  sanames <- set_predictor_names(sa, pred_names)
  expect_true(all(pred_names == get_predictor_names(sanames)))
  expect_true(all(pred_names %in% colnames(sanames$grid)))
  expect_true(all(sanames$grid$a == sa$grid$GID0))
  #
  expect_warning(sa <- add_scenarios(sa, scen) |> select_predictors(c("bio1", "bio12")))
  # name changes
  pred_names <- c("a", "b")
  sanames <- set_predictor_names(sa, pred_names)
  expect_true(all(pred_names == get_predictor_names(sanames)))
  expect_true(all(pred_names %in% colnames(sanames$grid)))
  expect_true(all(pred_names %in% colnames(sanames$scenarios$grid)))
  expect_true(all(pred_names %in% colnames(sanames$scenarios$data$current)))
  expect_true(all(pred_names %in% colnames(sanames$scenarios$data$ca_ssp245_2090)))
  expect_true(all(sanames$grid$a == sa$grid$bio1))
  expect_true(all(sanames$scenarios$grid$a == sa$scenarios$grid$bio1))
  expect_true(all(sanames$scenarios$data$current$a == sa$scenarios$data$current$bio1))
  expect_true(all(sanames$scenarios$data$ca_ssp245_2090$a == sa$scenarios$data$ca_ssp245_2090$bio1))
  #
  oc <- occurrences_sdm(occ, crs = 6933) |>
    join_area(sa)
  i <- input_sdm(oc, sa)
  # name changes
  inames <- set_predictor_names(i, pred_names)
  expect_true(all(pred_names == get_predictor_names(inames)))
  expect_true(all(pred_names %in% colnames(inames$predictors$grid)))
  expect_true(all(pred_names %in% colnames(inames$scenarios$grid)))
  expect_true(all(pred_names %in% colnames(inames$scenarios$data$current)))
  expect_true(all(pred_names %in% colnames(inames$scenarios$data$ca_ssp245_2090)))
  expect_true(all(pred_names %in% colnames(inames$scenarios$data$ca_ssp245_2090)))
  expect_true(all(inames$predictors$grid$a == i$predictors$grid$bio1))
  expect_true(all(inames$scenarios$grid$a == i$scenarios$grid$bio1))
  expect_true(all(inames$scenarios$data$current$a == i$scenarios$data$current$bio1))
  expect_true(all(inames$scenarios$data$ca_ssp245_2090$a == i$scenarios$data$ca_ssp245_2090$bio1))
  #
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
  i <- ensemble_sdm(i)
  # name changes
  inames <- set_scenarios_names(i, as.character(c(1:5)))
  expect_true(all(as.character(c(1:5)) == scenarios_names(inames)))
  expect_true(all(as.character(c(1:5)) == names(inames$scenarios$data)))
  expect_true(all(as.character(c(1:5)) == names(inames$predictions$predictions)))
  expect_true(all(as.character(c(1:5)) == colnames(inames$ensembles$data)))
  #
  # ensemble
  i <- gcms_ensembles(i, gcms = c("ca", "mi")) ######
  expect_true(all(c("matrix", "array") == class(i$ensembles$data)))
  expect_true(ncol(i$ensembles$data) == length(scenarios_names(i))+2)
  expect_true(nrow(i$ensembles$data) == length(species_names(i)))
  expect_true(all(scenarios_names(i) %in% colnames(i$ensembles$data)))
  expect_true(all(species_names(i) %in% rownames(i$ensembles$data)))
  expect_true(all(c("cell_id", "average") %in%
                    colnames(i$ensembles$data[,"_ssp585_2090"][[1]])))
  expect_true(all(c("list") == class(i$ensembles$data[,"_ssp585_2090"])))
  expect_true(all(c("data.frame") == class(i$ensembles$data[,"_ssp585_2090"][[1]])))
  expect_snapshot(i)
  e <- i$ensembles$data[,"_ssp585_2090"][[1]]
  p1 <- i$ensembles$data[,"mi_ssp585_2090"][[1]]
  p2 <- i$ensembles$data[,"ca_ssp585_2090"][[1]]
  expect_true(all(e$cell_id == p1$cell_id))
  expect_true(all(e$mean_occ_prob == (p1$mean_occ_prob + p2$mean_occ_prob) / 2))
})
