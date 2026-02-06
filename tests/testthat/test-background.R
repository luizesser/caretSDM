
test_that("background_tests", {
  skip_on_cran()
  set.seed(2)
  sa <- sdm_area(parana,
                 cell_size = 25000,
                 crs = 6933,
                 gdal = T) |>
    add_predictors(bioc) |>
    add_scenarios() |>
    select_predictors(c("bio1", "bio4", "bio12"))
  # single species
  oc <- occurrences_sdm(salm, crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    background()

  ## background_tests - n_background
  expect_equal(n_background(i), nrow(i$occurrences$background$data[[1]][[1]]))

  ## background_tests - background_data
  expect_equal(background_data(i), i$occurrences$background$data)


  expect_equal(as.numeric(i$occurrences$background$proportion), 1)
  expect_equal(as.numeric(i$occurrences$background$n_set), 1)

  ## modeling
  i1 <- i |>
    train_sdm(algo = c("maxent")) |>
    suppressWarnings()

  expect_true(all(names(i1$models$models) == c("Salminus brasiliensis")))
  expect_true(length(i1$models$models$`Salminus brasiliensis`) == 1)
  expect_true("maxent" %in% algorithms_used(i1))

  # multi species

  oc <- occurrences_sdm(rbind(salm, occ), crs = 6933) |>
    join_area(sa) |>
    suppressWarnings()
  i <- input_sdm(oc, sa) |>
    background()

  ## background_tests - n_background
  expect_equal(n_background(i), rep(nrow(i$occurrences$background$data[[1]][[1]]),
                                    length(species_names(i))))

  ## background_tests - background_data
  expect_equal(background_data(i), i$occurrences$background$data)


  expect_equal(as.numeric(i$occurrences$background$proportion), rep(1,
                                                                    length(species_names(i))))
  expect_equal(as.numeric(i$occurrences$background$n_set), 1)

  ## modeling
  i1 <- i |>
    train_sdm(algo = c("maxent")) |>
    suppressWarnings()

  expect_true(all(names(i1$models$models) == c("Salminus brasiliensis", "Araucaria angustifolia")))
  expect_true(length(i1$models$models$`Salminus brasiliensis`) == 1)
  expect_true("maxent" %in% algorithms_used(i1))

  # erros
  i <- input_sdm(oc, sa) |>
    pseudoabsences()
  expect_error(train_sdm(i, algo = c("maxent")))

})
