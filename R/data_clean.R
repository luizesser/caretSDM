#' Presence data cleaning routine
#'
#' Data cleaning wrapper using CoordinateCleaner package.
#'
#' @usage
#' data_clean(occ, pred = NULL,
#'            species = NA, lon = NA, lat = NA,
#'            capitals = TRUE,
#'            centroids = TRUE,
#'            duplicated = TRUE,
#'            identical = TRUE,
#'            institutions = TRUE,
#'            invalid = TRUE,
#'            terrestrial = TRUE,
#'            independent_test = TRUE)
#'
#' @param occ A \code{occurrences_sdm} object or \code{input_sdm}.
#' @param pred A \code{sdm_area} object. If \code{occ} is a \code{input_sdm} object with
#' predictors data, than \code{pred} is obtained from it.
#' @param species A \code{character} stating the name of the column with species names in \code{occ} (see details).
#' @param lon A \code{character} stating the name of the column with longitude in \code{occ} (see details).
#' @param lat A \code{character} stating the name of the column with latitude in \code{occ} (see details).
#' @param capitals Boolean to turn on/off the exclusion from countries capitals coordinates (see \code{?cc_cap})
#' @param centroids Boolean to turn on/off the exclusion from countries centroids coordinates (see \code{?cc_cen})
#' @param duplicated Boolean to turn on/off the exclusion from duplicated records (see \code{?cc_dupl})
#' @param identical Boolean to turn on/off the exclusion from records with identical lat/long values (see \code{?cc_equ})
#' @param institutions Boolean to turn on/off the exclusion from biodiversity institutions coordinates (see \code{?cc_inst})
#' @param invalid Boolean to turn on/off the exclusion from invalid coordinates (see \code{?cc_val})
#' @param terrestrial Boolean to turn on/off the exclusion from coordinates falling on sea (see \code{?cc_sea})
#'
#' @param independent_test Boolean. If \code{occ} has independent test data, the data cleaning routine
#' is also applied on it.
#'
#' @returns A \code{occurrences_sdm} object or \code{input_sdm} with cleaned presence data.
#'
#' @details
#' If the user does not used \code{GBIF_data} function to obtain species records, the function may
#' have problems to find which column from the presences table has species, longitude and latitude
#' information. In this regard, we implemented the parameters \code{species}, \code{lon} and
#' \code{lat} so the use can explicitly inform which columns should be used. If they remain as NA
#' (standard) the function will try to guess which columns are the correct one.
#'
#' @seealso \code{\link{GBIF_data} \link{occurrences_sdm} \link{sdm_area} \link{input_sdm}
#' \link{predictors}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Clean coordinates (terrestrial is set to false to make the run quicker):
#' i <- data_clean(i, terrestrial = FALSE)
#'
#' @importFrom CoordinateCleaner cc_cap cc_cen cc_dupl cc_equ cc_inst cc_val cc_sea
#' @importFrom stars st_extract st_rasterize
#' @importFrom stringdist stringdist
#' @importFrom sf st_as_sf st_crs st_transform st_join st_geometry_type
#' @importFrom dplyr mutate select
#'
#' @export
data_clean <- function(occ, pred = NULL,
                       species = NA, lon = NA, lat = NA,
                       capitals = TRUE,
                       centroids = TRUE,
                       duplicated = TRUE,
                       identical = TRUE,
                       institutions = TRUE,
                       invalid = TRUE,
                       terrestrial = TRUE,
                       independent_test = TRUE) {

  assert_logical_cli(capitals, any.missing = FALSE,   all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(centroids, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(duplicated, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(identical, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(institutions, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(invalid, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(terrestrial, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  assert_logical_cli(independent_test, any.missing = FALSE, all.missing = FALSE, len = 1, null.ok = FALSE)
  if(!is.null(pred)){assert_class_cli(pred, c("sdm_area"), null.ok = TRUE)}
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    if (is.null(pred)) {
      pred <- occ$predictors
    }
  } else {
    y <- occ
  }
  if("cell_id" %in% names(y$occurrences)) {
    message("Cell_ids identified, removing duplicated cell_id.")
    y$occurrences <- y$occurrences[!duplicated(y$occurrences$cell_id),]
  }
  if(sf::st_crs(y) !=4326){
    sf_t <- sf::st_transform(y$occurrences, 4326)
    x <- .sf_to_df_sdm(sf_t)
  } else {
    x <- .sf_to_df_sdm(y$occurrences)
  }

  if (is.na(species)) {
    cn <- colnames(x)
    species <- cn[which.min(stringdist::stringdist(cn, "species"))]
  }
  if (is.na(lon)) {
    colnames(x)
    lon <- cn[which.min(stringdist::stringdist(cn, "longitude"))]
  }
  if (is.na(lat)) {
    colnames(x)
    lat <- cn[which.min(stringdist::stringdist(cn, "latitude"))]
  }
  x <- subset(x, !is.na(lon) | !is.na(lat))
  if (capitals) { x <- CoordinateCleaner::cc_cap(x, lon = lon, lat = lat, species = species) }
  if (centroids) { x <- CoordinateCleaner::cc_cen(x, lon = lon, lat = lat, species = species) }
  if (duplicated) { x <- CoordinateCleaner::cc_dupl(x, lon = lon, lat = lat, species = species) }
  if (identical) { x <- CoordinateCleaner::cc_equ(x, lon = lon, lat = lat) }
  if (institutions) { x <- CoordinateCleaner::cc_inst(x, lon = lon, lat = lat, species = species) }
  if (invalid) { x <- CoordinateCleaner::cc_val(x, lon = lon, lat = lat) }
  if (terrestrial) { x <- CoordinateCleaner::cc_sea(x, lon = lon, lat = lat) }

  if(!is.null(pred)){
    not_line <- unique(sf::st_geometry_type(pred$grid)) != "LINESTRING"
  } else {
    not_line <- FALSE
  }

  if (not_line) {
    message("Predictors identified, procceding with grid filter (removing NA and duplicated data).")
    x2 <- sf::st_as_sf(x,
                       coords = c(lon, lat),
                       crs = 4326)
    if (sf::st_crs(x2) != sf::st_crs(pred$grid)) {
      x2 <- sf::st_transform(x2, crs = sf::st_crs(pred$grid))
    }
    if (is_sdm_area(pred)) {
      teste <- pred$grid |>
        stars::st_rasterize() |>
        stars::st_extract(x2) |>
        dplyr::mutate(species = x2$species) # |> na.omit()
      dup_rows <- teste |>
        as.data.frame() |>
        dplyr::select(-"geometry") |>
        duplicated()
      x <- teste[!dup_rows, c("cell_id","species", "geometry")]
    }

  }
  if(is.data.frame(x)){
    x <- sf::st_as_sf(x,
                      coords = c(lon, lat),
                      crs = 4326)
  }
  if(sf::st_crs(x) != sf::st_crs(y$crs)){
    x <- sf::st_transform(x, y$crs)
  }
  if(!"cell_id" %in% names(x) & !is.null(pred)) {
    nearest <- st_nearest_feature(x, select(pred$grid, "cell_id"))
    cell_id <- pred$grid[nearest, "cell_id"]
    x <- cbind(x, cell_id)|> # or cbind(cell_id, x)?
      dplyr::relocate("cell_id") |>
      dplyr::select(-"geometry.1")
  }

  y$occurrences <- x
  clean_methods <- c("NAs")
  if (capitals) { clean_methods <- c(clean_methods, "Capitals") }
  if (centroids) { clean_methods <- c(clean_methods, "Centroids") }
  if (duplicated) { clean_methods <- c(clean_methods, "Geographically Duplicated") }
  if (identical) { clean_methods <- c(clean_methods, "Identical Lat/Long") }
  if (institutions) { clean_methods <- c(clean_methods, "Institutions") }
  if (invalid) { clean_methods <- c(clean_methods, "Invalid") }
  if (terrestrial) { clean_methods <- c(clean_methods, "Non-terrestrial") }
  if (!is.null(pred)) { clean_methods <- c(clean_methods, "Duplicated Cell (grid)") }
  y$n_presences <- table(y$occurrences$species)

  if ("independent_test" %in% names(y) & independent_test) {
    x <- y$independent_test
    if(as.character(st_crs(x))[1] != "EPSG:4326"){
      sf_t <- sf::st_transform(x, 4326)
      x <- .sf_to_df_sdm(sf_t)
    } else {
      x <- .sf_to_df_sdm(x)
    }
    cn <- colnames(x)
    species <- cn[which.min(stringdist::stringdist(cn, "species"))]
    lon <- cn[which.min(stringdist::stringdist(cn, "longitude"))]
    lat <- cn[which.min(stringdist::stringdist(cn, "latitude"))]
    x <- subset(x, !is.na(lon) | !is.na(lat))
    if (capitals) { x <- CoordinateCleaner::cc_cap(x, lon = lon, lat = lat, species = species) }
    if (centroids) { x <- CoordinateCleaner::cc_cen(x, lon = lon, lat = lat, species = species) }
    if (duplicated) { x <- CoordinateCleaner::cc_dupl(x, lon = lon, lat = lat, species = species) }
    if (identical) { x <- CoordinateCleaner::cc_equ(x, lon = lon, lat = lat) }
    if (institutions) { x <- CoordinateCleaner::cc_inst(x, lon = lon, lat = lat, species = species) }
    if (invalid) { x <- CoordinateCleaner::cc_val(x, lon = lon, lat = lat) }
    if (terrestrial) { x <- CoordinateCleaner::cc_sea(x, lon = lon, lat = lat) }
    if (!is.null(pred)) {
      message("Predictors identified, procceding with grid filter (removing NA and duplicated data).")
      x2 <- sf::st_as_sf(x,
                         coords = c(lon, lat),
                         crs = 4326)
      if (sf::st_crs(x2) != sf::st_crs(pred$grid)) {
        x2 <- sf::st_transform(x2, crs = sf::st_crs(pred$grid))
      }
      if (is_sdm_area(pred)) {
        teste <- pred$grid |>
          stars::st_rasterize() |>
          stars::st_extract(x2) |>
          dplyr::mutate(species = x2$species) # |> na.omit()
        dup_rows <- teste |>
          as.data.frame() |>
          dplyr::select(-"geometry") |>
          duplicated()
        x <- teste[!dup_rows, c("cell_id","species", "geometry")]
      }
    }
    y$independent_test <- x
    clean_methods <- c(clean_methods, "Methods also applied in independent_test")
  }
  y$data_cleaning <- clean_methods

  if (is_input_sdm(occ)) {
    occ$occurrences <- y
    y <- occ
  }
  return(y)
}
