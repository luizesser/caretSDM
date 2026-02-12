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
  # name changes
  inames <- set_scenarios_names(i, as.character(c(1:5)))
  expect_true(all(as.character(c(1:5)) == scenarios_names(inames)))
  expect_true(all(as.character(c(1:5)) == names(inames$scenarios$data)))
  expect_true(all(as.character(c(1:5)) == names(inames$predictions$predictions)))
  expect_true(all(as.character(c(1:5)) == colnames(inames$predictions$ensembles)))
  #
  # ensemble
  i <- gcms_ensembles(i, gcms = c("ca", "mi")) ######
  expect_true("ensembles" %in% names(i$predictions))
  expect_true(all(c("matrix", "array") == class(i$predictions$ensembles)))
  expect_true(ncol(i$predictions$ensembles) == length(scenarios_names(i))+2)
  expect_true(nrow(i$predictions$ensembles) == length(species_names(i)))
  expect_true(all(scenarios_names(i) %in% colnames(i$predictions$ensembles)))
  expect_true(all(species_names(i) %in% rownames(i$predictions$ensembles)))
  expect_true(all(c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg") %in%
                    colnames(i$predictions$ensembles[,"_ssp585_2090"][[1]])))
  expect_true(all(c("list") == class(i$predictions$ensembles[,"_ssp585_2090"])))
  expect_true(all(c("data.frame") == class(i$predictions$ensembles[,"_ssp585_2090"][[1]])))
  expect_snapshot(i)
  e <- i$predictions$ensembles[,"_ssp585_2090"][[1]]
  p1 <- i$predictions$ensembles[,"mi_ssp585_2090"][[1]]
  p2 <- i$predictions$ensembles[,"ca_ssp585_2090"][[1]]
  expect_true(all(e$cell_id == p1$cell_id))
  expect_true(all(e$mean_occ_prob == (p1$mean_occ_prob + p2$mean_occ_prob) / 2))
})

test_that("full structure check", {
  set.seed(1)
  # prepare data
  sa_m <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
       add_predictors(bioc) |>
       add_scenarios(scen) |>
       select_predictors(c("bio1", "bio12")) |>
       suppressWarnings()
  sa_s <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio12")) |>
    suppressWarnings()
  oc_s <- occurrences_sdm(occ, crs = 6933)
  oc_m <- occurrences_sdm(rbind(salm, occ), crs = 6933)

  # multispecies multiscenario
  i_mm <- input_sdm(oc_m, sa_m) |>
       pseudoabsences(method="random", n_set = 2) |>
       train_sdm(algo = c("naive_bayes"),
                 ctrl=caret::trainControl(method = "boot",
                                          number = 1,
                                          classProbs = TRUE,
                                          returnResamp = "all",
                                          summaryFunction = summary_sdm,
                                          savePredictions = "all"),
                 variables_selected = c("bio1", "bio12")) |>
    suppressWarnings() |>
    predict_sdm(th=0.5) |>
    gcms_ensembles(gcms = c("ca", "mi"))
  # multispecies single scenario
  i_ms <- input_sdm(oc_m, sa_s) |>
    pseudoabsences(method="random", n_set = 2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all"),
              variables_selected = c("bio1", "bio12")) |>
    suppressWarnings() |>
    predict_sdm(th=0.5)
  # single species multiscenario
  i_sm <- input_sdm(oc_s, sa_m) |>
    pseudoabsences(method="random", n_set = 2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all"),
              variables_selected = c("bio1", "bio12")) |>
    suppressWarnings() |>
    predict_sdm(th=0.5) |>
    gcms_ensembles(gcms = c("ca", "mi"))
  # single species single scenario
  i_ss <- input_sdm(oc_s, sa_s) |>
    pseudoabsences(method="random", n_set = 2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all"),
              variables_selected = c("bio1", "bio12")) |>
    suppressWarnings() |>
    predict_sdm(th=0.5)

  ## Testing structure
  expect_equal(names(i_ss), c("occurrences", "predictors", "scenarios", "models", "predictions"))
  expect_equal(names(i_sm), c("occurrences", "predictors", "scenarios", "models", "predictions"))
  expect_equal(names(i_ms), c("occurrences", "predictors", "scenarios", "models", "predictions"))
  expect_equal(names(i_mm), c("occurrences", "predictors", "scenarios", "models", "predictions"))

  # occurrence
  expect_equal(names(i_ss$occurrences), c("occurrences", "spp_names", "n_presences", "crs", "pseudoabsences"))
  expect_equal(names(i_sm$occurrences), c("occurrences", "spp_names", "n_presences", "crs", "pseudoabsences"))
  expect_equal(names(i_ms$occurrences), c("occurrences", "spp_names", "n_presences", "crs", "pseudoabsences"))
  expect_equal(names(i_mm$occurrences), c("occurrences", "spp_names", "n_presences", "crs", "pseudoabsences"))

  expect_equal(class(i_ss$occurrences), c("occurrences"))
  expect_equal(class(i_sm$occurrences), c("occurrences"))
  expect_equal(class(i_ms$occurrences), c("occurrences"))
  expect_equal(class(i_mm$occurrences), c("occurrences"))

  expect_equal(names(i_ss$occurrences$occurrences), c("cell_id", "species", "geometry"))
  expect_equal(names(i_sm$occurrences$occurrences), c("cell_id", "species", "geometry"))
  expect_equal(names(i_ms$occurrences$occurrences), c("cell_id", "species", "geometry"))
  expect_equal(names(i_mm$occurrences$occurrences), c("cell_id", "species", "geometry"))

  expect_equal(class(i_ss$occurrences$occurrences), c("sf", "data.frame"))
  expect_equal(class(i_sm$occurrences$occurrences), c("sf", "data.frame"))
  expect_equal(class(i_ms$occurrences$occurrences), c("sf", "data.frame"))
  expect_equal(class(i_mm$occurrences$occurrences), c("sf", "data.frame"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_ss$occurrences$occurrences))), c("POINT"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_sm$occurrences$occurrences))), c("POINT"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_ms$occurrences$occurrences))), c("POINT"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$occurrences$occurrences))), c("POINT"))

  expect_equal(i_ss$occurrences$spp_names, species_names(i_ss))
  expect_equal(i_sm$occurrences$spp_names, species_names(i_sm))
  expect_equal(i_ms$occurrences$spp_names, species_names(i_ms))
  expect_equal(i_mm$occurrences$spp_names, species_names(i_mm))

  expect_equal(class(i_ss$occurrences$spp_names), c("character"))
  expect_equal(class(i_sm$occurrences$spp_names), c("character"))
  expect_equal(class(i_ms$occurrences$spp_names), c("character"))
  expect_equal(class(i_mm$occurrences$spp_names), c("character"))

  expect_equal(class(i_ss$occurrences$n_presences), c("table"))
  expect_equal(class(i_sm$occurrences$n_presences), c("table"))
  expect_equal(class(i_ms$occurrences$n_presences), c("table"))
  expect_equal(class(i_mm$occurrences$n_presences), c("table"))

  expect_true(all(names(i_ss$occurrences$n_presences) %in% species_names(i_ss)))
  expect_true(all(names(i_sm$occurrences$n_presences) %in% species_names(i_sm)))
  expect_true(all(names(i_ms$occurrences$n_presences) %in% species_names(i_ms)))
  expect_true(all(names(i_mm$occurrences$n_presences) %in% species_names(i_mm)))

  expect_equal(class(i_ss$occurrences$crs), c("integer"))
  expect_equal(class(i_sm$occurrences$crs), c("integer"))
  expect_equal(class(i_ms$occurrences$crs), c("integer"))
  expect_equal(class(i_mm$occurrences$crs), c("integer"))

  expect_equal(names(i_ss$occurrences$pseudoabsences), c("data", "method", "n_set", "n_pa"))
  expect_equal(names(i_sm$occurrences$pseudoabsences), c("data", "method", "n_set", "n_pa"))
  expect_equal(names(i_ms$occurrences$pseudoabsences), c("data", "method", "n_set", "n_pa"))
  expect_equal(names(i_mm$occurrences$pseudoabsences), c("data", "method", "n_set", "n_pa"))

  expect_equal(class(i_ss$occurrences$pseudoabsences$n_pa), c("table"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$n_pa), c("table"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$n_pa), c("table"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$n_pa), c("table"))

  expect_equal(class(i_ss$occurrences$pseudoabsences$n_set), c("numeric"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$n_set), c("numeric"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$n_set), c("numeric"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$n_set), c("numeric"))

  expect_equal(class(i_ss$occurrences$pseudoabsences$method), c("character"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$method), c("character"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$method), c("character"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$method), c("character"))

  expect_equal(class(i_ss$occurrences$pseudoabsences$data), c("list"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$data), c("list"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$data), c("list"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$data), c("list"))

  expect_equal(names(i_ss$occurrences$pseudoabsences$data), species_names(i_ss))
  expect_equal(names(i_sm$occurrences$pseudoabsences$data), species_names(i_sm))
  expect_equal(names(i_ms$occurrences$pseudoabsences$data), species_names(i_ms))
  expect_equal(names(i_mm$occurrences$pseudoabsences$data), species_names(i_mm))

  expect_equal(class(i_ss$occurrences$pseudoabsences$data[[1]]), c("list"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$data[[1]]), c("list"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$data[[1]]), c("list"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$data[[1]]), c("list"))

  expect_equal(names(i_ss$occurrences$pseudoabsences$data[[1]]), NULL)
  expect_equal(names(i_sm$occurrences$pseudoabsences$data[[1]]), NULL)
  expect_equal(names(i_ms$occurrences$pseudoabsences$data[[1]]), NULL)
  expect_equal(names(i_mm$occurrences$pseudoabsences$data[[1]]), NULL)

  expect_equal(length(i_ss$occurrences$pseudoabsences$data[[1]]), 2)
  expect_equal(length(i_sm$occurrences$pseudoabsences$data[[1]]), 2)
  expect_equal(length(i_ms$occurrences$pseudoabsences$data[[1]]), 2)
  expect_equal(length(i_mm$occurrences$pseudoabsences$data[[1]]), 2)

  expect_equal(names(i_ss$occurrences$pseudoabsences$data[[1]][[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_sm$occurrences$pseudoabsences$data[[1]][[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_ms$occurrences$pseudoabsences$data[[1]][[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_mm$occurrences$pseudoabsences$data[[1]][[1]]), c("cell_id", "bio1", "bio12", "geometry"))

  expect_equal(class(i_ss$occurrences$pseudoabsences$data[[1]][[1]]), c("sf", "data.frame"))
  expect_equal(class(i_sm$occurrences$pseudoabsences$data[[1]][[1]]), c("sf", "data.frame"))
  expect_equal(class(i_ms$occurrences$pseudoabsences$data[[1]][[1]]), c("sf", "data.frame"))
  expect_equal(class(i_mm$occurrences$pseudoabsences$data[[1]][[1]]), c("sf", "data.frame"))

  # predictors
  expect_equal(names(i_ss$predictors), c("grid", "cell_size", "parameters"))
  expect_equal(names(i_sm$predictors), c("grid", "cell_size", "parameters"))
  expect_equal(names(i_ms$predictors), c("grid", "cell_size", "parameters"))
  expect_equal(names(i_mm$predictors), c("grid", "cell_size", "parameters"))

  expect_equal(class(i_ss$predictors), c("sdm_area"))
  expect_equal(class(i_sm$predictors), c("sdm_area"))
  expect_equal(class(i_ms$predictors), c("sdm_area"))
  expect_equal(class(i_mm$predictors), c("sdm_area"))

  expect_equal(class(i_ss$predictors$grid), c("sf", "data.frame"))
  expect_equal(class(i_sm$predictors$grid), c("sf", "data.frame"))
  expect_equal(class(i_ms$predictors$grid), c("sf", "data.frame"))
  expect_equal(class(i_mm$predictors$grid), c("sf", "data.frame"))

  expect_equal(names(i_ss$predictors$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_sm$predictors$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_ms$predictors$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_mm$predictors$grid), c("cell_id", "bio1", "bio12", "geometry"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_ss$predictors$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_sm$predictors$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_ms$predictors$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictors$grid))), c("POLYGON"))

  expect_equal(class(i_ss$predictors$cell_size), c("numeric"))
  expect_equal(class(i_sm$predictors$cell_size), c("numeric"))
  expect_equal(class(i_ms$predictors$cell_size), c("numeric"))
  expect_equal(class(i_mm$predictors$cell_size), c("numeric"))

  expect_equal(class(i_ss$predictors$parameters), c("list"))
  expect_equal(class(i_sm$predictors$parameters), c("list"))
  expect_equal(class(i_ms$predictors$parameters), c("list"))
  expect_equal(class(i_mm$predictors$parameters), c("list"))

  expect_equal(names(i_ss$predictors$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_sm$predictors$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_ms$predictors$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_mm$predictors$parameters), c("gdal", "lines_as_sdm_area"))

  expect_equal(class(i_ss$predictors$parameters$gdal), c("logical"))
  expect_equal(class(i_sm$predictors$parameters$gdal), c("logical"))
  expect_equal(class(i_ms$predictors$parameters$gdal), c("logical"))
  expect_equal(class(i_mm$predictors$parameters$gdal), c("logical"))

  expect_equal(class(i_ss$predictors$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_sm$predictors$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_ms$predictors$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_mm$predictors$parameters$lines_as_sdm_area), c("logical"))

  # scenarios
  expect_equal(names(i_ss$scenarios), c("grid", "cell_size", "parameters", "data"))
  expect_equal(names(i_sm$scenarios), c("grid", "cell_size", "parameters", "data"))
  expect_equal(names(i_ms$scenarios), c("grid", "cell_size", "parameters", "data"))
  expect_equal(names(i_mm$scenarios), c("grid", "cell_size", "parameters", "data"))

  expect_equal(class(i_ss$scenarios), c("sdm_area"))
  expect_equal(class(i_sm$scenarios), c("sdm_area"))
  expect_equal(class(i_ms$scenarios), c("sdm_area"))
  expect_equal(class(i_mm$scenarios), c("sdm_area"))

  expect_equal(class(i_ss$scenarios$grid), c("sf", "data.frame"))
  expect_equal(class(i_sm$scenarios$grid), c("sf", "data.frame"))
  expect_equal(class(i_ms$scenarios$grid), c("sf", "data.frame"))
  expect_equal(class(i_mm$scenarios$grid), c("sf", "data.frame"))

  expect_equal(names(i_ss$scenarios$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_sm$scenarios$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_ms$scenarios$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_mm$scenarios$grid), c("cell_id", "bio1", "bio12", "geometry"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_ss$scenarios$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_sm$scenarios$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_ms$scenarios$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$scenarios$grid))), c("POLYGON"))

  expect_equal(class(i_ss$scenarios$cell_size), c("numeric"))
  expect_equal(class(i_sm$scenarios$cell_size), c("numeric"))
  expect_equal(class(i_ms$scenarios$cell_size), c("numeric"))
  expect_equal(class(i_mm$scenarios$cell_size), c("numeric"))

  expect_equal(class(i_ss$scenarios$parameters), c("list"))
  expect_equal(class(i_sm$scenarios$parameters), c("list"))
  expect_equal(class(i_ms$scenarios$parameters), c("list"))
  expect_equal(class(i_mm$scenarios$parameters), c("list"))

  expect_equal(names(i_ss$scenarios$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_sm$scenarios$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_ms$scenarios$parameters), c("gdal", "lines_as_sdm_area"))
  expect_equal(names(i_mm$scenarios$parameters), c("gdal", "lines_as_sdm_area"))

  expect_equal(class(i_ss$scenarios$parameters$gdal), c("logical"))
  expect_equal(class(i_sm$scenarios$parameters$gdal), c("logical"))
  expect_equal(class(i_ms$scenarios$parameters$gdal), c("logical"))
  expect_equal(class(i_mm$scenarios$parameters$gdal), c("logical"))

  expect_equal(class(i_ss$scenarios$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_sm$scenarios$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_ms$scenarios$parameters$lines_as_sdm_area), c("logical"))
  expect_equal(class(i_mm$scenarios$parameters$lines_as_sdm_area), c("logical"))

  expect_equal(class(i_ss$scenarios$data), c("list"))
  expect_equal(class(i_sm$scenarios$data), c("list"))
  expect_equal(class(i_ms$scenarios$data), c("list"))
  expect_equal(class(i_mm$scenarios$data), c("list"))

  expect_equal(names(i_ss$scenarios$data), scenarios_names(i_ss))
  expect_equal(names(i_sm$scenarios$data), scenarios_names(i_sm))
  expect_equal(names(i_ms$scenarios$data), scenarios_names(i_ms))
  expect_equal(names(i_mm$scenarios$data), scenarios_names(i_mm))

  expect_equal(class(i_ss$scenarios$data[[1]]), c("sf", "data.frame"))
  expect_equal(class(i_sm$scenarios$data[[1]]), c("sf", "data.frame"))
  expect_equal(class(i_ms$scenarios$data[[1]]), c("sf", "data.frame"))
  expect_equal(class(i_mm$scenarios$data[[1]]), c("sf", "data.frame"))

  expect_equal(names(i_ss$scenarios$data[[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_sm$scenarios$data[[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_ms$scenarios$data[[1]]), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_mm$scenarios$data[[1]]), c("cell_id", "bio1", "bio12", "geometry"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_ss$scenarios$data[[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_sm$scenarios$data[[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_ms$scenarios$data[[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$scenarios$data[[1]]))), c("POLYGON"))

  # models
  expect_equal(names(i_ss$models), c("validation", "predictors", "algorithms", "models", "tuning"))
  expect_equal(names(i_sm$models), c("validation", "predictors", "algorithms", "models", "tuning"))
  expect_equal(names(i_ms$models), c("validation", "predictors", "algorithms", "models", "tuning"))
  expect_equal(names(i_mm$models), c("validation", "predictors", "algorithms", "models", "tuning"))

  expect_equal(class(i_ss$models), c("models"))
  expect_equal(class(i_sm$models), c("models"))
  expect_equal(class(i_ms$models), c("models"))
  expect_equal(class(i_mm$models), c("models"))

  expect_equal(class(i_ss$models$tuning), c("numeric"))
  expect_equal(class(i_sm$models$tuning), c("numeric"))
  expect_equal(class(i_ms$models$tuning), c("numeric"))
  expect_equal(class(i_mm$models$tuning), c("numeric"))

  expect_equal(class(i_ss$models$algorithms), c("character"))
  expect_equal(class(i_sm$models$algorithms), c("character"))
  expect_equal(class(i_ms$models$algorithms), c("character"))
  expect_equal(class(i_mm$models$algorithms), c("character"))

  expect_equal(class(i_ss$models$predictors), c("character"))
  expect_equal(class(i_sm$models$predictors), c("character"))
  expect_equal(class(i_ms$models$predictors), c("character"))
  expect_equal(class(i_mm$models$predictors), c("character"))

  expect_equal(i_ss$models$predictors, get_predictor_names(i_ss))
  expect_equal(i_sm$models$predictors, get_predictor_names(i_sm))
  expect_equal(i_ms$models$predictors, get_predictor_names(i_ms))
  expect_equal(i_mm$models$predictors, get_predictor_names(i_mm))

  expect_equal(names(i_ss$models$validation), c("method", "number", "metrics"))
  expect_equal(names(i_sm$models$validation), c("method", "number", "metrics"))
  expect_equal(names(i_ms$models$validation), c("method", "number", "metrics"))
  expect_equal(names(i_mm$models$validation), c("method", "number", "metrics"))

  expect_equal(class(i_ss$models$validation$number), c("numeric"))
  expect_equal(class(i_sm$models$validation$number), c("numeric"))
  expect_equal(class(i_ms$models$validation$number), c("numeric"))
  expect_equal(class(i_mm$models$validation$number), c("numeric"))

  expect_equal(class(i_ss$models$validation$method), c("character"))
  expect_equal(class(i_sm$models$validation$method), c("character"))
  expect_equal(class(i_ms$models$validation$method), c("character"))
  expect_equal(class(i_mm$models$validation$method), c("character"))

  expect_equal(class(i_ss$models$validation$metrics), c("list"))
  expect_equal(class(i_sm$models$validation$metrics), c("list"))
  expect_equal(class(i_ms$models$validation$metrics), c("list"))
  expect_equal(class(i_mm$models$validation$metrics), c("list"))

  expect_equal(names(i_ss$models$validation$metrics), species_names(i_ss))
  expect_equal(names(i_sm$models$validation$metrics), species_names(i_sm))
  expect_equal(names(i_ms$models$validation$metrics), species_names(i_ms))
  expect_equal(names(i_mm$models$validation$metrics), species_names(i_mm))

  expect_equal(class(i_ss$models$validation$metrics[[1]]), c("data.frame"))
  expect_equal(class(i_sm$models$validation$metrics[[1]]), c("data.frame"))
  expect_equal(class(i_ms$models$validation$metrics[[1]]), c("data.frame"))
  expect_equal(class(i_mm$models$validation$metrics[[1]]), c("data.frame"))

  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_ss$models$validation$metrics[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_sm$models$validation$metrics[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_ms$models$validation$metrics[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_mm$models$validation$metrics[[1]])))

  expect_equal(class(i_ss$models$models), c("list"))
  expect_equal(class(i_sm$models$models), c("list"))
  expect_equal(class(i_ms$models$models), c("list"))
  expect_equal(class(i_mm$models$models), c("list"))

  expect_equal(names(i_ss$models$models), species_names(i_ss))
  expect_equal(names(i_sm$models$models), species_names(i_sm))
  expect_equal(names(i_ms$models$models), species_names(i_ms))
  expect_equal(names(i_mm$models$models), species_names(i_mm))

  expect_equal(names(i_ss$models$models[[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_sm$models$models[[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_ms$models$models[[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_mm$models$models[[1]]), c("m1.1", "m2.1"))

  expect_equal(class(i_ss$models$models[[1]][[1]]), c("train", "train.formula"))
  expect_equal(class(i_sm$models$models[[1]][[1]]), c("train", "train.formula"))
  expect_equal(class(i_ms$models$models[[1]][[1]]), c("train", "train.formula"))
  expect_equal(class(i_mm$models$models[[1]][[1]]), c("train", "train.formula"))

  # predictions
  expect_equal(names(i_ss$predictions), c("thresholds", "predictions", "file", "ensembles", "grid"))
  expect_equal(names(i_sm$predictions), c("thresholds", "predictions", "file", "ensembles", "grid"))
  expect_equal(names(i_ms$predictions), c("thresholds", "predictions", "file", "ensembles", "grid"))
  expect_equal(names(i_mm$predictions), c("thresholds", "predictions", "file", "ensembles", "grid"))

  expect_equal(class(i_ss$predictions), c("predictions"))
  expect_equal(class(i_sm$predictions), c("predictions"))
  expect_equal(class(i_ms$predictions), c("predictions"))
  expect_equal(class(i_mm$predictions), c("predictions"))

  expect_equal(class(i_ss$predictions$thresholds), "list")
  expect_equal(class(i_sm$predictions$thresholds), "list")
  expect_equal(class(i_ms$predictions$thresholds), "list")
  expect_equal(class(i_mm$predictions$thresholds), "list")

  expect_equal(names(i_ss$predictions$thresholds), c("values", "method", "criteria"))
  expect_equal(names(i_sm$predictions$thresholds), c("values", "method", "criteria"))
  expect_equal(names(i_ms$predictions$thresholds), c("values", "method", "criteria"))
  expect_equal(names(i_mm$predictions$thresholds), c("values", "method", "criteria"))

  expect_equal(class(i_ss$predictions$thresholds$criteria), c("character"))
  expect_equal(class(i_sm$predictions$thresholds$criteria), c("character"))
  expect_equal(class(i_ms$predictions$thresholds$criteria), c("character"))
  expect_equal(class(i_mm$predictions$thresholds$criteria), c("character"))

  expect_equal(class(i_ss$predictions$thresholds$method), c("character"))
  expect_equal(class(i_sm$predictions$thresholds$method), c("character"))
  expect_equal(class(i_ms$predictions$thresholds$method), c("character"))
  expect_equal(class(i_mm$predictions$thresholds$method), c("character"))

  expect_equal(class(i_ss$predictions$thresholds$values), c("list"))
  expect_equal(class(i_sm$predictions$thresholds$values), c("list"))
  expect_equal(class(i_ms$predictions$thresholds$values), c("list"))
  expect_equal(class(i_mm$predictions$thresholds$values), c("list"))

  expect_equal(names(i_ss$predictions$thresholds$values), species_names(i_ss))
  expect_equal(names(i_sm$predictions$thresholds$values), species_names(i_sm))
  expect_equal(names(i_ms$predictions$thresholds$values), species_names(i_ms))
  expect_equal(names(i_mm$predictions$thresholds$values), species_names(i_mm))

  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_ss$predictions$thresholds$values[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_sm$predictions$thresholds$values[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_ms$predictions$thresholds$values[[1]])))
  expect_true(all(c("algo", "ROC", "TSS", "Sensitivity", "Specificity") %in% names(i_mm$predictions$thresholds$values[[1]])))

  expect_equal(class(i_ss$predictions$grid), c("sf", "data.frame"))
  expect_equal(class(i_sm$predictions$grid), c("sf", "data.frame"))
  expect_equal(class(i_ms$predictions$grid), c("sf", "data.frame"))
  expect_equal(class(i_mm$predictions$grid), c("sf", "data.frame"))

  expect_equal(names(i_ss$predictions$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_sm$predictions$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_ms$predictions$grid), c("cell_id", "bio1", "bio12", "geometry"))
  expect_equal(names(i_mm$predictions$grid), c("cell_id", "bio1", "bio12", "geometry"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_ss$predictions$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_sm$predictions$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_ms$predictions$grid))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictions$grid))), c("POLYGON"))

  expect_equal(class(i_ss$predictions$predictions), c("list"))
  expect_equal(class(i_sm$predictions$predictions), c("list"))
  expect_equal(class(i_ms$predictions$predictions), c("list"))
  expect_equal(class(i_mm$predictions$predictions), c("list"))

  expect_true(all(names(i_ss$predictions$predictions) %in% scenarios_names(i_ss)))
  expect_true(all(names(i_sm$predictions$predictions) %in% scenarios_names(i_sm)))
  expect_true(all(names(i_ms$predictions$predictions) %in% scenarios_names(i_ms)))
  expect_true(all(names(i_mm$predictions$predictions) %in% scenarios_names(i_mm)))

  expect_true(all(names(i_ss$predictions$predictions[[1]]) %in% species_names(i_ss)))
  expect_true(all(names(i_sm$predictions$predictions[[1]]) %in% species_names(i_sm)))
  expect_true(all(names(i_ms$predictions$predictions[[1]]) %in% species_names(i_ms)))
  expect_true(all(names(i_mm$predictions$predictions[[1]]) %in% species_names(i_mm)))

  expect_equal(names(i_ss$predictions$predictions[[1]][[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_sm$predictions$predictions[[1]][[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_ms$predictions$predictions[[1]][[1]]), c("m1.1", "m2.1"))
  expect_equal(names(i_mm$predictions$predictions[[1]][[1]]), c("m1.1", "m2.1"))

  expect_equal(names(i_ss$predictions$predictions[[1]][[1]][[1]]), c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"))
  expect_equal(names(i_sm$predictions$predictions[[1]][[1]][[1]]), c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"))
  expect_equal(names(i_ms$predictions$predictions[[1]][[1]][[1]]), c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"))
  expect_equal(names(i_mm$predictions$predictions[[1]][[1]][[1]]), c("cell_id", "bio1", "bio12", "presence", "pseudoabsence", "geometry"))

  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictions$predictions[[1]][[1]][[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictions$predictions[[1]][[1]][[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictions$predictions[[1]][[1]][[1]]))), c("POLYGON"))
  expect_equal(as.character(unique(sf::st_geometry_type(i_mm$predictions$predictions[[1]][[1]][[1]]))), c("POLYGON"))

  expect_equal(class(i_ss$predictions$ensembles), c("matrix", "array"))
  expect_equal(class(i_sm$predictions$ensembles), c("matrix", "array"))
  expect_equal(class(i_ms$predictions$ensembles), c("matrix", "array"))
  expect_equal(class(i_mm$predictions$ensembles), c("matrix", "array"))

  expect_true(all(scenarios_names(i_ss) %in% colnames(i_ss$predictions$ensembles)))
  expect_true(all(scenarios_names(i_sm) %in% colnames(i_sm$predictions$ensembles)))
  expect_true(all(scenarios_names(i_ms) %in% colnames(i_ms$predictions$ensembles)))
  expect_true(all(scenarios_names(i_mm) %in% colnames(i_mm$predictions$ensembles)))

  expect_equal(rownames(i_ss$predictions$ensembles), species_names(i_ss))
  expect_equal(rownames(i_sm$predictions$ensembles), species_names(i_sm))
  expect_equal(rownames(i_ms$predictions$ensembles), species_names(i_ms))
  expect_equal(rownames(i_mm$predictions$ensembles), species_names(i_mm))

  expect_equal(class(i_ss$predictions$ensembles[1,1]), "list")
  expect_equal(class(i_sm$predictions$ensembles[1,1]), "list")
  expect_equal(class(i_ms$predictions$ensembles[1,1]), "list")
  expect_equal(class(i_mm$predictions$ensembles[1,1]), "list")

  expect_equal(class(i_ss$predictions$ensembles[1,1][[1]]), "data.frame")
  expect_equal(class(i_sm$predictions$ensembles[1,1][[1]]), "data.frame")
  expect_equal(class(i_ms$predictions$ensembles[1,1][[1]]), "data.frame")
  expect_equal(class(i_mm$predictions$ensembles[1,1][[1]]), "data.frame")

  expect_equal(names(i_ss$predictions$ensembles[1,1][[1]]), c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"))
  expect_equal(names(i_sm$predictions$ensembles[1,1][[1]]), c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"))
  expect_equal(names(i_ms$predictions$ensembles[1,1][[1]]), c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"))
  expect_equal(names(i_mm$predictions$ensembles[1,1][[1]]), c("cell_id", "mean_occ_prob", "wmean_AUC", "committee_avg"))
})

