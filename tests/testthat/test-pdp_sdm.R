sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
sa <- add_predictors(sa, bioc)
sa <- dplyr::select(sa, c("bio01", "bio12"))
i <- input_sdm(occurrences_sdm(occ, crs=6933), sa)
i <- data_clean(i)
i <- pseudoabsences(i, method="random")
suppressWarnings(i <- train_sdm(i, algo = c("nnet", "kknn")))

test_that("pdp_sdm", {
  expect_error(pdp_sdm("i"))
  x <- pdp_sdm(i)
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("bio01", "bio12"), unique(x$data$variable))

  expect_error(pdp_sdm(i, variables_selected = "bio1"))
  x <- pdp_sdm(i, variables_selected = "bio01")
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("bio01"), unique(x$data$variable))

  expect_error(pdp_sdm(i, algo = "knn"))
  x <- pdp_sdm(i, algo = "kknn")
  expect_equal(class(x), c("gg", "ggplot"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$data)))
  expect_equal(c("m1.2", "m2.2", "m3.2", "m4.2", "m5.2", "m6.2", "m7.2", "m8.2", "m9.2", "m10.2"),
               unique(x$data$id))

  x <- get_pdp_sdm(i)
  expect_equal(class(x), "list")
  expect_equal(names(x), algorithms_used(i))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$nnet)))

  expect_error(get_pdp_sdm("i"))
  x <- get_pdp_sdm(i)
  expect_equal(class(x), c("list"))
  expect_equal(names(x), algorithms_used(i))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$nnet)))
  expect_equal(c("bio01", "bio12"), unique(x$nnet$variable))

  expect_error(get_pdp_sdm(i, variables_selected = "bio1"))
  x <- get_pdp_sdm(i, variables_selected = "bio01")
  expect_equal(class(x), c("list"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$nnet)))
  expect_equal(c("bio01"), unique(x$nnet$variable))

  expect_error(get_pdp_sdm(i, algo = "knn"))
  x <- get_pdp_sdm(i, algo = "kknn")
  expect_equal(class(x), c("list"))
  expect_true(all(c("id", "yhat", "variable", "value") %in% colnames(x$kknn)))
  expect_equal(c("m1.2", "m2.2", "m3.2", "m4.2", "m5.2", "m6.2", "m7.2", "m8.2", "m9.2", "m10.2"),
               unique(x$kknn$id))
})

