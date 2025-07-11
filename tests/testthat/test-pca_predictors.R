test_that("pca_predictors - normal path", {
  sa <- sdm_area(parana, 1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio1", "bio12"))
  sa <- sa |> mutate(div=bio1/bio12, prod=bio1*bio12, sub=bio12-bio1, soma=bio1+bio12)
  sa <- add_scenarios(sa)
  oc <- occurrences_sdm(occ, crs=6933)
  expect_warning(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pca_predictors(i)
  expect_true("variable_selection" %in% names(i$predictors))
  expect_true("pca" %in% names(i$predictors$variable_selection))
  expect_true(any(get_predictor_names(i)[-c(1:6)] %in% i$predictors$variable_selection$pca$selected_variables))
  expect_true(all(get_predictor_names(i)[-c(1:6)] %in% colnames(i$scenarios$data$current)))
  skip_on_cran()
  expect_snapshot(pca_summary(i))
})

test_that("pca_predictors - without scenarios", {
  sa <- sdm_area(parana, 1)
  sa <- add_predictors(sa, bioc)
  sa <- select(sa, c("bio1", "bio12"))
  sa <- sa |> mutate(div=bio1/bio12, prod=bio1*bio12, sub=bio12-bio1, soma=bio1+bio12)
  oc <- occurrences_sdm(occ, crs=6933)
  expect_warning(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  i <- pca_predictors(i)
  expect_true("variable_selection" %in% names(i$predictors))
  expect_true("pca" %in% names(i$predictors$variable_selection))
  expect_true(any(get_predictor_names(i)[-c(1:6)] %in% i$predictors$variable_selection$pca$selected_variables))
  expect_true(all(get_predictor_names(i)[-c(1:6)] %in% colnames(i$scenarios$data$current)))
  skip_on_cran()
  expect_snapshot(pca_summary(i))
})
