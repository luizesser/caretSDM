test_that("join_area", {
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
  oc <- occurrences_sdm(occ, crs = 6933)
  oc1 <- join_area(oc, sa)
  expect_warning(oc2 <- join_area(oc1, sa))
  expect_true("cell_id" %in% colnames(oc2$occurrences))
  expect_true(all(oc1$occurrences$cell_id == oc2$occurrences$cell_id))

  sa <- sdm_area(rivs, cell_size = 100000, crs = 6933, lines_as_sdm_area = TRUE)
  oc <- occurrences_sdm(salm, crs = 6933)
  oc1 <- join_area(oc, sa)
  expect_true("cell_id" %in% colnames(oc1$occurrences))
  expect_equal(as.character(unique(sf::st_geometry_type(sa$grid))), "LINESTRING")

  sa <- sdm_area(scen_rs, cell_size = 100000, crs = 6933)
  oc <- occurrences_sdm(salm, crs = 6933)
  expect_error(expect_warning(join_area(oc, sa)))
})
