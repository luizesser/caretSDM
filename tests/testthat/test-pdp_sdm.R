test_that("pdp_sdm", {
  skip_on_cran()
  sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
  sa <- add_predictors(sa, bioc)
  sa <- dplyr::select(sa, c("bio1", "bio12"))
  i <- input_sdm(occurrences_sdm(occ, crs=6933), sa)
  i <- pseudoabsences(i, method="random", n_set = 3)
  suppressWarnings(i <- train_sdm(i, algo = c("naive_bayes", "kknn")))
  expect_error(pdp_sdm("i"))
  x <- pdp_sdm(i)
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("bio1", "bio12"), unique(x$data$variable))

  expect_error(pdp_sdm(i, variables_selected = "bio01"))
  x <- pdp_sdm(i, variables_selected = "bio1")
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("bio1"), unique(x$data$variable))

  expect_error(pdp_sdm(i, algo = "knn"))
  x <- pdp_sdm(i, algo = "kknn")
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("m1.2", "m2.2", "m3.2"),
               unique(x$data$id))

  x <- get_pdp_sdm(i)
  expect_equal(class(x), "list")
  expect_equal(names(x), algorithms_used(i))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$naive_bayes)))
  expect_error(get_pdp_sdm("i"))
  expect_equal(c("bio1", "bio12"), unique(x$naive_bayes$variable))

  expect_error(get_pdp_sdm(i, variables_selected = "bio01"))
  x <- get_pdp_sdm(i, variables_selected = "bio1")
  expect_equal(class(x), c("list"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$naive_bayes)))
  expect_equal(c("bio1"), unique(x$naive_bayes$variable))

  expect_error(get_pdp_sdm(i, algo = "knn"))
  x <- get_pdp_sdm(i, algo = "kknn")
  expect_equal(class(x), c("list"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$kknn)))
  expect_equal(c("m1.2", "m2.2", "m3.2"),
               unique(x$kknn$id))
})

