test_that("vif_predictors - normal path", {
  sa <- sdm_area(parana, 1)
  sa <- add_predictors(sa, bioc)
  sa <- dplyr::select(sa, c("bio1", "bio12"))
  sa <- sa |> dplyr::mutate(div=bio1/bio12, prod=bio1*bio12, sub=bio12-bio1, soma=bio1+bio12)
  oc <- occurrences_sdm(occ, crs=6933)
  expect_warning(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  th=0.8
  suppressWarnings(i <- vif_predictors(i, th=th))
  expect_true("variable_selection" %in% names(i$predictors))
  expect_true("vif" %in% names(i$predictors$variable_selection))
  expect_equal(i$predictors$variable_selection$vif$threshold, th)
  expect_true(any(get_predictor_names(i) %in% i$predictors$variable_selection$vif$selected_variables))
})

test_that("vif_predictors - normal path", {
  sa <- sdm_area(parana, 1)
  sa <- add_predictors(sa, bioc)
  sa <- dplyr::select(sa, c("bio1", "bio12"))
  sa <- sa |> dplyr::mutate(div=bio1/bio12, prod=bio1*bio12, sub=bio12-bio1, soma=bio1+bio12)
  oc <- occurrences_sdm(occ, crs=6933)
  expect_warning(oc <- join_area(oc, sa))
  i <- input_sdm(oc, sa)
  expect_error(vif_predictors(i, th=NA))
  expect_error(vif_predictors(i, th="a"))
  expect_error(vif_predictors(i, variables_selected = 'bio99'))
  expect_error(vif_predictors(i$occurrences))
  expect_error(vif_predictors(i$predictors))
})
