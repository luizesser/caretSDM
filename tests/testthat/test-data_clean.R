test_that("data_clean - normal path with sdm_area", {
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
  sa <- add_predictors(sa, bioc)
  expect_warning(oc <- occurrences_sdm(occ, independent_test = TRUE, crs= 6933) |> join_area(sa))
  i <- input_sdm(oc, sa)
  i <- data_clean(i, terrestrial=FALSE)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("cell_id" %in% colnames(i$occurrences$occurrences))
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))

})

test_that("data_clean - normal path with pred", {
  set.seed(1)
  pred <- sdm_area(bioc, cell_size = 1)
  expect_warning(oc <- occurrences_sdm(occ, independent_test = TRUE, crs= 6933) |> join_area(pred))
  i <- input_sdm(oc, pred)
  i <- data_clean(i, terrestrial=FALSE)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("cell_id" %in% colnames(i$occurrences$occurrences))
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})

test_that("data_clean - normal path without pred", {
  oc <- occurrences_sdm(occ, crs= 6933)
  i <- input_sdm(oc)
  i <- data_clean(i, terrestrial=FALSE)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})

test_that("data_clean - normal path with occurences", {
  oc <- occurrences_sdm(occ, crs= 6933)
  i <- data_clean(oc, terrestrial=FALSE)
  expect_true(st_crs(i$occurrences) == st_crs(oc$occurrences))
  expect_true(sf::st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                sf::st_geometry_type(i$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences)[1], "sf")
  expect_true("species" %in% colnames(i$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences))
  expect_true(ncol(i$occurrences) == ncol(oc$occurrences))
  expect_true(is_occurrences(i))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences))
})

test_that("data_clean - normal path with pred at wgs84", {
  pred <- sdm_area(bioc, cell_size = 1)
  occ2 <- sf::st_as_sf(occ,
                     coords = c("decimalLongitude", "decimalLatitude"),
                     crs = 6933)
  occ2 <- sf::st_transform(occ2, crs = 4326)
  suppressWarnings(oc <- occurrences_sdm(occ2, independent_test = TRUE, crs= 4326) |> join_area(pred))
  i <- input_sdm(oc, pred)
  i <- data_clean(i, terrestrial=FALSE)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("cell_id" %in% colnames(i$occurrences$occurrences))
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})


