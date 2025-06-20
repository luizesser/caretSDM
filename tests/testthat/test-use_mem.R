set.seed(1)
sa <- sdm_area(parana, 0.1)
sa <- add_predictors(sa, bioc)
sa <- select(sa, c("bio1", "bio12"))
sa <- add_scenarios(sa)
oc <- occurrences_sdm(occ, crs=6933)
suppressWarnings(oc <- join_area(oc, sa))
i <- input_sdm(oc, sa)

test_that("use_mem works", {
  i1 <- use_mem(i, add=FALSE)
  expect_true(all(get_coords(i)==get_coords(i1)))
})

set.seed(1)
sa <- sdm_area(parana, 0.1)
sa <- add_predictors(sa, bioc)
sa <- select(sa, c("bio1", "bio12"))
sa <- add_scenarios(sa)
occ <- rbind(occ, salm)
oc <- occurrences_sdm(occ, crs=4326)
i <- input_sdm(oc, sa)

test_that("use_mem works", {
  expect_error(use_mem("i"))
  i1 <- use_mem(i)
  expect_equal(2*sum(n_records(i)), sum(n_records(i1)))
  expect_equal(species_names(i1), c(species_names(i), "MEM"))

  i2 <- use_mem(i, add=FALSE)
  expect_equal(sum(n_records(i)), as.numeric(n_records(i2)))
  expect_equal(species_names(i2), c("MEM"))

  i3 <- use_mem(i, add=FALSE, name="tudojunto")
  expect_equal(sum(n_records(i)), as.numeric(n_records(i3)))
  expect_equal(species_names(i3), c("tudojunto"))

  i1 <- use_mem(oc)
  expect_equal(2*sum(n_records(i)), sum(n_records(i1)))
  expect_equal(species_names(i1), c(species_names(i), "MEM"))

  i2 <- use_mem(oc, add=FALSE)
  expect_equal(sum(n_records(i)), as.numeric(n_records(i2)))
  expect_equal(species_names(i2), c("MEM"))

  i3 <- use_mem(oc, add=FALSE, name="tudojunto")
  expect_equal(sum(n_records(i)), as.numeric(n_records(i3)))
  expect_equal(species_names(i3), c("tudojunto"))
})
