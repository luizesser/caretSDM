sa <- sdm_area(parana, 100000, crs=6933) |>
  add_predictors(bioc) |>
  select(c("bio1", "bio12")) |>
  add_scenarios()
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)

test_that("pseudoabsences - normal", {
  suppressWarnings(i2 <- pseudoabsences(i, method = "bioclim", n_set = 1))
  expect_equal(n_pseudoabsences(i2), n_records(i2))
  expect_equal("bioclim", pseudoabsence_method(i2))
  expect_warning(i2 <- pseudoabsences(i2, method = "random", n_set = 1, variables_selected = c("bio1", "bio12")))
  expect_equal("random", pseudoabsence_method(i2))
  expect_equal(n_pseudoabsences(i2), n_records(i2))
  suppressWarnings(i3 <- vif_predictors(i, th = 0.9))
  i3 <- pseudoabsences(i3, method = "bioclim", variables_selected = "vif", n_set = 1)
  expect_equal("bioclim", pseudoabsence_method(i3))
  i4 <- pca_predictors(i)
  i4 <- pseudoabsences(i4, method = "bioclim", variables_selected = "pca", n_set = 1)
  expect_equal("bioclim", pseudoabsence_method(i4))
  n_set=3
  i5 <- pseudoabsences(i, method = "random", n_set=n_set)
  expect_equal(n_set, pseudoabsence_data(i5)[[1]] |> length())
  n_pa <- 100
  expect_warning(i6 <- pseudoabsences(i, method = "random", n_pa=n_pa, n_set=1))
  expect_true(n_pa == n_pseudoabsences(i6))
  suppressWarnings(oc <- occurrences_sdm(rbind(occ, salm), crs=6933) |> join_area(sa))
  i6 <- input_sdm(oc, sa)
  test <- capture_warnings(i6 <- pseudoabsences(i6, method = "random", n_pa=n_pa))
  expect_equal(length(test), 2)
  test <-  n_pseudoabsences(i6)
  expect_equal(names(test), species_names(i6))
  expect_equal(length(as.numeric(test)), length(species_names(i6)))
  expect_equal(as.numeric(test), rep(n_pa, length(species_names(i6))))
  expect_error(pseudoabsences(i, method = "random", n_pa=c(1,2,3)))
})
