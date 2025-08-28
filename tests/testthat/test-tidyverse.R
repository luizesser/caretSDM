if (fs::dir_exists(here::here("tests", "testthat", "testdata"))) {
  pr_gpkg <- here::here("tests", "testthat", "testdata", "parana.gpkg") |>
    sf::st_read(quiet = TRUE)
} else {
  pr_gpkg <- test_path("testdata","parana.gpkg") |>
    sf::st_read(quiet = TRUE)
}

test_that("select - tidyverse sa", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  sa <- dplyr::select(sa, c("CODIGOIB1", "NOMEUF2"))
  sa_col_names <- sa$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_equal(c("cell_id", "CODIGOIB1", "NOMEUF2", "geometry"), sa_col_names)
})

test_that("select - tidyverse i", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  i <- input_sdm(sa)
  i <- dplyr::select(i, c("CODIGOIB1", "NOMEUF2"))
  i_col_names <- i$predictors$grid |> colnames()
  expect_true("cell_id" %in% i_col_names)
  expect_equal(c("cell_id", "CODIGOIB1", "NOMEUF2", "geometry"), i_col_names)
})

test_that("mutate - tidyverse sa", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  sa <- sa |> dplyr::mutate(teste=GID0/CODIGOIB1)
  sa_col_names <- sa$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_true("teste" %in% sa_col_names)
  expect_true(round(as.numeric(sa$grid[1,"teste"])[1], 6)==0.463415)
})

test_that("mutate - tidyverse i", {
  sa <- sdm_area(pr_gpkg, cell_size = 10000, crs = 6933)
  i <- input_sdm(sa)
  i <- i |> dplyr::mutate(teste=GID0/CODIGOIB1)
  sa_col_names <- i$predictors$grid |> colnames()
  expect_true("cell_id" %in% sa_col_names)
  expect_true("teste" %in% sa_col_names)
  expect_true(round(as.numeric(i$predictors$grid[1,"teste"])[1], 6) == 0.463415)
})

test_that("select/filter scenarios", {
  expect_warning(sa <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    add_scenarios(scen) )
  sa1 <- dplyr::select(sa, c("bio1"))
  expect_equal(c("cell_id", "bio1", "geometry"), colnames(sa1$grid))
  expect_equal(c("cell_id", "bio1", "geometry"), colnames(sa1$scenarios$data$current))
  expect_equal(c("cell_id", "bio1", "geometry"), colnames(sa1$scenarios$data$ca_ssp245_2090))
  sa1 <- dplyr::filter(sa1, cell_id < 30)
  expect_true(all( sa1$grid$cell_id < 30 ))
  oc1 <- occurrences_sdm(occ, crs=6933)
  oc2 <- occurrences_sdm(salm, crs=6933)
  oc <- add_occurrences(oc1, oc2)
  expect_equal(species_names(oc), unique(oc$occurrences$species))

  i <- input_sdm(oc, sa)
  i1 <- dplyr::select(i, c("bio1"))
  expect_equal(c("cell_id", "bio1", "geometry"), colnames(i1$predictors$grid))
  expect_true(
    all(c("cell_id", "bio1", "geometry") %in% colnames(i1$scenarios$data$current))
  )
  expect_true(
    all(c("cell_id", "bio1", "geometry") %in% colnames(i1$scenarios$data$ca_ssp245_2090))
  )
  expect_equal(species_names(i1), unique(i1$occurrences$occurrences$species))
  i2 <- dplyr::filter(i1, species == "Araucaria angustifolia")
  expect_equal(species_names(i2), unique(i2$occurrences$occurrences$species))

  i1 <- input_sdm(join_area(oc, sa), sa)
  expect_true("cell_id" %in% colnames(i1$occurrences$occurrences))
  i2 <- dplyr::filter(i1, species == "Araucaria angustifolia")
  expect_true("cell_id" %in% colnames(i2$occurrences$occurrences))

  i2 <- dplyr::filter(i1, species == "Araucaria angustifolia")
  expect_error(filter_species(i1, spp = NULL))
  expect_true("cell_id" %in% colnames(i2$occurrences$occurrences))
  expect_true("Araucaria angustifolia" %in% unique(i2$occurrences$occurrences$species))

  expect_true(all(species_names(oc) %in% unique(oc$occurrences$species)))
  oc2 <- filter_species(oc, spp = "Araucaria angustifolia")
  expect_true("Araucaria angustifolia" %in% unique(oc2$occurrences$species))
})

test_that("filter_species", {
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    dplyr::select(c("bio1", "bio4", "bio12")) |>
    add_scenarios()
  oc <- occurrences_sdm(occ, crs = 6933) |>
    add_occurrences(occurrences_sdm(salm, crs = 6933)) |>
    join_area(sa)
  i <- input_sdm(oc, sa)
  i <- dplyr::select(i, c("bio1", "bio12"))
  expect_true(!"bio4" %in% colnames(i$predictors$grid))
  expect_true(!"bio4" %in% colnames(i$scenarios$data$current))
  i <- i |>
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
  expect_true(all(species_names(i) %in% unique(i$occurrences$occurrences$species)))
  expect_true(all(species_names(i) %in% names(i$occurrences$pseudoabsences$data)))
  expect_true(all(species_names(i) %in% names(i$models$models)))
  expect_true(all(species_names(i) %in% names(i$predictions$predictions[[1]])))
  expect_true(all(species_names(i) %in% rownames(i$predictions$ensembles)))
  i2 <- filter_species(i, spp = "Araucaria angustifolia")
  expect_true(all(species_names(i2) %in% unique(i2$occurrences$occurrences$species)))
  expect_true(all(species_names(i2) %in% names(i2$occurrences$pseudoabsences$data)))
  expect_true(all(species_names(i2) %in% names(i2$models$models)))
  expect_true(all(species_names(i2) %in% names(i2$predictions$predictions[[1]])))
  expect_true(all(species_names(i2) %in% rownames(i2$predictions$ensembles)))
  expect_error(filter_species(i, spp = "x"))
  expect_error(filter_species(i, spp = NULL))


})
