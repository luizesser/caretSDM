test_that("predictors misc - predictors_names", {
  sa <- sdm_area(parana, cell_size = 1)
  pred <- bioc
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(
    predictors(sa_pred),
    c("GID0", "CODIGOIB1", "NOMEUF2", "SIGLAUF3", "bio1", "bio4", "bio12")
  )
})

test_that("predictors misc - set_predictors_names", {
  sa <- sdm_area(parana, cell_size = 1)
  pred <- bioc
  suppressWarnings(sa_pred <- add_predictors(sa, pred))
  expect_equal(get_predictors(sa_pred), sa_pred$grid)
})
