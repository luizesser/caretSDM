test_that("tsne_sdm", {
  skip_on_cran()
  sa <- sdm_area(parana, cell_size = 10000, crs = 6933) |>
    add_predictors(bioc) |>
    select_predictors(c("bio1", "bio4", "bio12")) |>
    add_scenarios()
  expect_warning(oc <- occurrences_sdm(occ, crs = 6933) |>
    join_area(sa) |>
    input_sdm(sa) |>
    data_clean())
  i <- pseudoabsences(oc, method = "bioclim", th = 0)
  expect_no_error(p <- tsne_sdm(i))
  expect_true(class(p) == "list")
  expect_true(names(p) == species_names(i))
  expect_true(ggplot2::is_ggplot(p[[1]][[1]]))
  expect_error(expect_warning(tsne_sdm(i$occurrences)))
  i <- oc |>
    vif_predictors() |>
    pseudoabsences(oc, method = "bioclim", th = 0, variables_selected = "vif")
  expect_no_error(p <- tsne_sdm(i, variables_selected = "vif"))
  expect_true(class(p) == "list")
  expect_true(names(p) == species_names(i))
  expect_true(ggplot2::is_ggplot(p[[1]][[1]]))
  expect_error(expect_warning(tsne_sdm(i$occurrences)))
  i <- oc |>
    pca_predictors() |>
    pseudoabsences(oc, method = "bioclim", th = 0, variables_selected = "pca")
  expect_error(p <- tsne_sdm(i, variables_selected = "pca")) # Returns error since "pca" selected is only 1 var.
  expect_true(class(p) == "list")
})
