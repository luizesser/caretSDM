test_that("occurrences - data.frame - single species", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(df), "occurrences")
})

test_that("occurrences - data.frame - multiple species", {
  df <- data.frame(spp_names = c(rep("Aa", 50), rep("Bb", 50)), longitude = runif(100),
                   decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(df), "occurrences")
})

test_that("occurrences - tibble - single species", {
  tb <- tibble(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(tb), "occurrences")
})

test_that("occurrences - tibble - multiple species", {
  tb <- tibble(spp_names = c(rep("Aa", 50), rep("Bb", 50)), longitude = runif(100),
               decimalLatitude = runif(100))
  expect_s3_class(occurrences_sdm(tb), "occurrences")
})

test_that("occurrences - sf", {
  sf_occ <- st_as_sf(occ, coords = c(2:3))
  sf::st_crs(sf_occ) <- 6933
  expect_s3_class(occurrences_sdm(sf_occ), "occurrences")
})

test_that("occurrences - missing lat/long column", {
  tb <- tibble(spp_names = c(rep("Aa", 50), rep("Bb", 50)), decimalLatitude = runif(100))
  expect_error(occurrences_sdm(tb))
})

test_that("occurrences - missing sp column", {
  tb <- tibble(longitude = runif(100), decimalLatitude = runif(100))
  expect_message(occurrences_sdm(tb))
})

test_that("occurrences - check data columns names", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb)
  expect_true("species" %in% colnames(oc$occurrences))
  expect_true("geometry" %in% colnames(oc$occurrences))
})

test_that("occurrences - check data columns classes", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb)
  expect_true(class(oc$occurrences$species)=='character')
  expect_true(class(oc$occurrences$geometry)[1]=='sfc_POINT')
})

#### testar nomes das colunas no output
test_that("occurrences - column names correction", {
  df <- data.frame(spp_names = rep("Aa", 100), longitude = runif(100), decimalLatitude = runif(100))
  oc <- occurrences_sdm(df)
  a <- colnames(oc$occurrences)[1]
  b <- colnames(df)[1]
  expect_false(all(a==b))
})

test_that("occurrences - reordering of columns", {
  df <- data.frame(decimalLatitude = runif(100), longitude = runif(100), spp_names = c(rep("Aa", 50),
                                                                                   rep("Bb", 50)))
  oc <- occurrences_sdm(df)
  a <- get_coords(oc)
  colnames(df) <- c("Y", "X", "species")
  b <- df[,c(2,1)]
  expect_true(all(a==b))
})

test_that("occurrences - species column without proper name", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), araucaria = c(rep("Aa", 50),
                                                                                   rep("Bb", 50)))
  expect_message(occurrences_sdm(tb))
})

#### testar epsgs validos
test_that("occurrences - epsg out of range", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  expect_error(occurrences_sdm(tb, epsg = 1))
})

test_that("occurrences - epsg not numeric", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  expect_error(occurrences_sdm(tb, epsg = "a"))
})

test_that("occurrences - valid epsg", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb, epsg = 4326)
  expect_true(oc$epsg==4326)
})

test_that("occurrences - independent epsg", {
  set.seed(1)
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_tb <- tibble(y = runif(100), x = runif(100), spp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb, independent_test = indep_tb, epsg = 6933, independent_test_epsg = 4326)
  s <- oc$occurrences
  expect_equal(as.character(st_crs(oc$occurrences)[1]), "EPSG:6933")
  expect_equal(as.character(st_crs(oc$independent_test)[1]), "EPSG:6933")
})

test_that("occurrences - independent epsg not given", {
  set.seed(1)
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_tb <- tibble(y = runif(100), x = runif(100), spp = c(rep("Aa", 100)))
  expect_error(occurrences_sdm(tb, independent_test = indep_tb, epsg = 6933))
})

#### testar dados independentes
test_that("occurrences - independent data has same colnames as data", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  indep_tb <- tibble(y = runif(100), x = runif(100), spp = c(rep("Aa", 50), rep("Bb", 50)))
  oc <- occurrences_sdm(tb, independent_test = indep_tb, independent_test_epsg = 4326)
  expect_true(all(colnames(oc$independent_test) == colnames(oc$occurrences)))
})

test_that("occurrences - independent data has correct amount of data", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb, independent_test = TRUE, p=0.1)
  expect_equal(nrow(tb)*0.1, nrow(oc$independent_test))
  expect_equal(nrow(tb)*0.9, nrow(oc$occurrences))
})

test_that("occurrences - p out of range", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  expect_error(occurrences_sdm(tb, independent_test = TRUE, p=10))
})

test_that("occurrences - p wrong class", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  expect_error(occurrences_sdm(tb, independent_test = TRUE, p="a"))
})

test_that("occurrences - check indep_test and occurrences class", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb, independent_test = TRUE, p=0.3)
  expect_true(class(oc$independent_test)[1] == 'sf')
  expect_true(class(oc$occurrences)[1] == 'sf')
  expect_true(class(oc$independent_test$geometry)[1] == 'sfc_POINT')
  expect_true(class(oc$occurrences$geometry)[1] == 'sfc_POINT')
})

test_that("occurrences - check indep_test columns names", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb, independent_test = TRUE, p=0.3)
  expect_true("species" %in% colnames(oc$independent_test))
  expect_true("geometry" %in% colnames(oc$independent_test))
})

test_that("occurrences - check indep_test columns classes", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  oc <- occurrences_sdm(tb, independent_test = TRUE, p=0.3)
  expect_true(class(oc$independent_test$species)=='character')
  expect_true(class(oc$independent_test$geometry)[1]=='sfc_POINT')
})

test_that("occurrences - both data have no sp info", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100))
  indep_tb <- tibble(y = runif(100), x = runif(100))
  expect_message(oc <- occurrences_sdm(tb, independent_test = indep_tb, independent_test_epsg = 4326))
  a <- unique(oc$occurrences$species)
  b <- unique(oc$independent_test$species)
  expect_equal(a,b)
})

test_that("occurrences - indep_data has no sp info and data has two species", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  indep_tb <- tibble(y = runif(100), x = runif(100))
  expect_error(occurrences_sdm(tb, independent_test = indep_tb, independent_test_epsg = 4326))
})

test_that("occurrences - indep_data has no sp info and data has one species", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_tb <- tibble(y = runif(100), x = runif(100))
  oc <- occurrences_sdm(tb, independent_test = indep_tb, independent_test_epsg = 4326)
  expect_equal(unique(oc$occurrences$species),unique(oc$independent_test$species))
})

test_that("occurrences - indep_data is from wrong class", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  expect_error(occurrences_sdm(tb, independent_test = "a"))
})

test_that("occurrences - indep_data is sf with no epsg", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  expect_error(occurrences_sdm(tb, independent_test = indep_sf))
})

test_that("occurrences - indep_data is sf with given epsg", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  oc <- occurrences_sdm(tb, independent_test = indep_sf, epsg=6933, independent_test_epsg = 4326)
  expect_true(st_crs(oc$independent_test) == st_crs(oc$occurrences))
})

test_that("occurrences - indep_data is sf with epsg in it.", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  sf::st_crs(indep_sf) <- 4326
  oc <- occurrences_sdm(tb, independent_test = indep_sf, epsg=6933)
  expect_true(st_crs(oc$independent_test) == st_crs(oc$occurrences))
})

test_that("occurrences - indep_data is sf with no species column.", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  sf::st_crs(indep_sf) <- 4326
  oc <- occurrences_sdm(tb, independent_test = indep_sf, epsg=6933)
  expect_true("species" %in% colnames(oc$independent_test))
})

test_that("occurrences - indep_data is sf with no species column, but there are 2 species in x.", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  sf::st_crs(indep_sf) <- 4326
  expect_error(occurrences_sdm(tb, independent_test = indep_sf, epsg=6933))
})

test_that("occurrences - indep_data is sf has species column, but there are missing species.", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 50),
                                                                            rep("Bb", 50)))
  indep_sf <- tibble(decimalLatitude = runif(100), longitude = runif(100),
                     species = c(rep("Aa", 100)))
  indep_sf <- st_as_sf(indep_sf, coords=colnames(indep_sf)[c(2,1)])
  sf::st_crs(indep_sf) <- 4326
  expect_error(occurrences_sdm(tb, independent_test = indep_sf, epsg=6933))
})

# Test species_names
test_that("species_names - from occurrences", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb)
  expect_equal(species_names(oc), unique(tb$sp))
  expect_equal(species_names(oc), oc$spp_names)
})

test_that("species_names - from input_sdm", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  i <- input_sdm(occurrences_sdm(tb))
  expect_equal(species_names(i), unique(tb$sp))
  expect_equal(species_names(i), i$occurrences$spp_names)
})

test_that("species_names - from wrong class", {
  expect_error(species_names("i"))
})

test_that("species_names - from right class, but no data", {
  i <- input_sdm()
  expect_null(species_names(i))
})

# Test n_records
test_that("species_names - from occurrences", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb)
  expect_equal(n_records(oc), table(tb$sp))
  expect_equal(n_records(oc), table(oc$occurrences$species))
})

test_that("species_names - from input_sdm", {
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  i <- input_sdm(occurrences_sdm(tb))
  expect_equal(n_records(i), table(tb$sp))
  expect_equal(n_records(i), table(i$occurrences$occurrences$species))
})

test_that("species_names - from wrong class", {
  expect_error(n_records("i"))
})

test_that("species_names - from right class, but no data", {
  i <- input_sdm()
  expect_null(n_records(i))
})

# Test get_coords
test_that("get_coords - from occurrences", {
  df <- data.frame(Y = runif(100), X = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(df)
  expect_equal(get_coords(oc), df[,c(2,1)])
})

test_that("get_coords - from input_sdm", {
  df <- data.frame(Y = runif(100), X = runif(100), sp = c(rep("Aa", 100)))
  i <- input_sdm(occurrences_sdm(df))
  expect_equal(get_coords(i), df[,c(2,1)])
})

test_that("get_coords - from wrong class", {
  expect_error(get_coords("i"))
})

test_that("get_coords - from right class, but no data", {
  i <- input_sdm()
  expect_error(get_coords(i))
})

# Print
test_that("occurrences - print", {
  set.seed(1)
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb, independent_test = TRUE)
  expect_snapshot(print(oc), error = FALSE)
})

# Test sf output
test_that("occurrences - sf_output bbox", {
  set.seed(1)
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb, independent_test = TRUE)
  s <- oc$occurrences
  expect_equal(as.numeric(st_bbox(oc$occurrences)),
               c(0.01307758, 0.01339033, 0.99268406, 0.99190609))
  expect_equal(as.numeric(st_bbox(oc$independent_test)),
               c(0.38049389, 0.14330438, 0.92861520, 0.86433947))
})

test_that("occurrences - sf_output epsg", {
  set.seed(1)
  tb <- tibble(decimalLatitude = runif(100), longitude = runif(100), sp = c(rep("Aa", 100)))
  oc <- occurrences_sdm(tb, independent_test = TRUE, epsg=6933)
  s <- oc$occurrences
  expect_equal(as.character(st_crs(oc$occurrences)[1]), "EPSG:6933")
  expect_equal(as.character(st_crs(oc$independent_test)[1]), "EPSG:6933")
})

