test_that("data_clean - normal path with sdm_area", {
  sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
  sa <- add_predictors(sa, bioc)
  oc <- occurrences_sdm(occ, independent_test = TRUE, epsg= 6933)
  i <- input_sdm(oc, sa)
  i <- data_clean(i)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})

test_that("data_clean - normal path with pred", {
  pred <- predictors_sdm(bioc)
  oc <- occurrences_sdm(occ, independent_test = TRUE, epsg= 6933)
  i <- input_sdm(oc, pred)
  i <- data_clean(i)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})

test_that("data_clean - normal path without pred", {
  oc <- occurrences_sdm(occ, epsg= 6933)
  i <- input_sdm(oc)
  i <- data_clean(i)
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
  oc <- occurrences_sdm(occ, epsg= 6933)
  i <- data_clean(oc)
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
  pred <- predictors_sdm(bioc)
  occ2 <- sf::st_as_sf(occ,
                     coords = c("decimalLongitude", "decimalLatitude"),
                     crs = 6933)
  occ2 <- sf::st_transform(occ2, crs = 4326)
  oc <- occurrences_sdm(occ2, independent_test = TRUE, epsg= 4326)
  i <- input_sdm(oc, pred)
  i <- data_clean(i)
  expect_true(st_crs(i$occurrences$occurrences) == st_crs(oc$occurrences))
  expect_true(st_geometry_type(oc$occurrences, by_geometry = FALSE) ==
                st_geometry_type(i$occurrences$occurrences, by_geometry = FALSE))
  expect_equal(class(i$occurrences$occurrences)[1], "sf")
  expect_true("species" %in% colnames(i$occurrences$occurrences))
  expect_true("geometry" %in% colnames(i$occurrences$occurrences))
  expect_true(ncol(i$occurrences$occurrences) == ncol(oc$occurrences))
  expect_true(nrow(oc$occurrences) >= nrow(i$occurrences$occurrences))
})
