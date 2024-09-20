sa <- sdm_area(parana, cell_size = 5000, crs = 6933)
sa <- add_predictors(sa, bioc)
sa <- dplyr::select(sa, c("bio01", "bio12"))
suppressWarnings(oc <- occurrences_sdm(occ, crs=6933) |> join_area(sa))
i <- input_sdm(oc, sa)
i <- data_clean(i)
i <- vif_predictors(i)
i <- pca_predictors(i)
i <- pseudoabsences(i, method="bioclim")

test_that("tsne_sdm works", {
  expect_error(tsne_sdm(i))
})
