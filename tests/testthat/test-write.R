test_that("write", {
  skip_on_cran()
  # Prepare Data
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933) |>
    add_predictors(bioc) |>
    add_scenarios(scen) |>
    select_predictors(c("bio1", "bio12")) |>
    suppressWarnings()
  oc <- occurrences_sdm(occ, crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    pseudoabsences(method="random", n_set = 2) |>
    train_sdm(algo = c("naive_bayes"),
              ctrl=caret::trainControl(method = "boot",
                                       number = 1,
                                       classProbs = TRUE,
                                       returnResamp = "all",
                                       summaryFunction = summary_sdm,
                                       savePredictions = "all"),
              variables_selected = c("bio1", "bio12")) |>
    predict_sdm(th=0.8) |>
    gcms_ensembles(gcms = c("ca", "mi")) |>
    suppressWarnings()

  # write_ensembles
  expect_no_error(write_ensembles(i, path = "tmp_test", ext = ".tif", centroid = FALSE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/_ssp245_2090.tif"))
  expect_no_error(write_ensembles(i$predictions, path = "tmp_test", ext = ".gpkg", centroid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/_ssp245_2090.gpkg"))
  expect_no_error(write_ensembles(i, path = "tmp_test", ext = ".csv", centroid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/_ssp245_2090.csv"))

  # write_predictions
  expect_no_error(write_predictions(i, path = "tmp_test", ext = ".tif", centroid = FALSE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/mi_ssp245_2090/predictions/m1.1.tif"))
  expect_no_error(suppressWarnings(write_predictions(i$predictions, path = "tmp_test", ext = ".gpkg", centroid = TRUE)))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/mi_ssp245_2090/predictions/m1.1.gpkg"))
  expect_no_error(suppressWarnings(write_predictions(i, path = "tmp_test", ext = ".csv", centroid = TRUE)))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/mi_ssp245_2090/predictions/m1.1.csv"))

  # write_predictors
  expect_no_error(write_predictors(i, path = "tmp_test", ext = ".tif", centroid = FALSE))
  expect_true(file.exists("tmp_test/predictors.tif"))
  expect_warning(write_predictors(i$predictors, path = "tmp_test", ext = ".gpkg", centroid = TRUE))
  expect_true(file.exists("tmp_test/predictors.gpkg"))
  expect_warning(write_predictors(i, path = "tmp_test", ext = ".csv", centroid = TRUE))
  expect_true(file.exists("tmp_test/predictors.csv"))

  # write_models
  expect_no_error(write_models(i, path = "tmp_test"))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/models.rds"))

  # write_occurrences
  expect_no_error(write_occurrences(i, path = "tmp_test/Araucaria angustifolia/presences.csv", grid = FALSE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/presences.csv"))
  expect_warning(write_occurrences(i, path = "tmp_test/Araucaria angustifolia/presences_grid.csv", grid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/presences_grid.csv"))

  # write_pseudoabsences
  expect_no_error(write_pseudoabsences(i, path = "tmp_test", centroid = FALSE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/pseudoabsences_1.csv"))
  expect_no_error(write_pseudoabsences(i, path = "tmp_test", ext = ".gpkg",centroid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/pseudoabsences_1.gpkg"))
  expect_no_error(write_pseudoabsences(i, path = "tmp_test", ext = ".tif",centroid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/pseudoabsences_1.tif"))

  # write_grid
  expect_warning(write_grid(i, path = "tmp_test/Araucaria angustifolia/grid.gpkg", centroid = TRUE))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/grid.gpkg"))

  # write_validation_metrics
  expect_no_error(write_validation_metrics(i, path = "tmp_test"))
  expect_true(file.exists("tmp_test/Araucaria angustifolia/validation_metrics.csv"))
  unlink("tmp_test", recursive = TRUE)
  # write_gpkg
  tmp_dir <- "/tmp"
  withr::with_tempdir(
    pattern = "tmp_sdm_area",
    {
      sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
      sa |> write_gpkg(file_path = ".", file_name = "parana")
      expect_file_exists(fs::path("parana.gpkg"))
    },
    tmpdir = tmp_dir
  )
  sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
  expect_error(
    sa |> write_gpkg(file_path = "parana.gpkg", file_name = "parana"),
    "x Assertion on file_path failed."
  )
  expect_error(
    sa |> write_gpkg(file_path = "/parana", file_name = "parana"),
    "x Assertion on file_path failed."
  )
  expect_error(
    sa |> write_gpkg(file_path = "/tmp", file_name = 1),
    "x Assertion on file_name failed."
  )
})
