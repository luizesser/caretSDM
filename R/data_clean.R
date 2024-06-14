#' Presence data cleaning routine
#'
#' Data cleaning wrapper using CoordinateCleaner.
#'
#' @usage data_clean(x, pred = NULL, species = NA, long = NA, lat = NA, terrestrial = TRUE, independent_test = TRUE)
#'
#' @param occ A \code{occurrences} object or \code{input_sdm}.
#' @param pred A \code{predictors} object. If \code{occ} is a \code{input_sdm} object with
#' predictors data, than \code{pred} is obtained from it.
#' @param species A character stating the name of the column with species names in \code{occ} (see details).
#' @param long A character stating the name of the column with longitude in \code{occ} (see details).
#' @param lat A character stating the name of the column with latitude in \code{occ} (see details).
#' @param terrestrial If the species is terrestrial (TRUE), than the function deletes
#' non-terrestrial coordinates.
#' @param independent_test TRUE. If \code{occ} has independent test data, the data cleaning routine
#' is also applied on it.
#'
#' @details
#' If the user does not used \code{GBIF_data} function to obtain species records, the function may
#' have problems to find which column from the presences table has species, longitude and latitude
#' information. In this regard, we implemented the parameters \code{species}, \code{long} and
#' \code{lat} so the use can explicitly inform which columns should be used. If they remain as NA
#' (standard) the function will try to guess which columns are the correct one.
#'
#' @seealso \code{\link{GBIF_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Create input_sdm:
#' i <- input_sdm(occurrences(occ), sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#' i
#'
#' @import CoordinateCleaner
#' @import raster
#' @import stringdist
#'
#' @export
data_clean <- function(occ, pred = NULL, species = NA, long = NA, lat = NA, terrestrial = TRUE, independent_test = TRUE) {
  if (class(occ) == "input_sdm") {
    y <- occ$occurrences
    if (is.null(pred)) {
      pred <- occ$predictors
    }
  } else {
    y <- occ
  }
  if (!is.na(species)) {
    species <- species
  } else {
    cn <- colnames(y$occurrences)
    species <- cn[which.min(stringdist(cn, "species"))]
  }
  if (!is.na(long)) {
    lon <- long
  } else {
    colnames(y$occurrences)
    lon <- cn[which.min(stringdist(cn, "longitude"))]
  }
  if (!is.na(lat)) {
    lat <- lat
  } else {
    colnames(y$occurrences)
    lat <- cn[which.min(stringdist(cn, "latitude"))]
  }
  x <- y$occurrences
  x <- subset(x, !is.na(lon) | !is.na(lat))
  x <- cc_cap(x, lon = lon, lat = lat, species = species)
  x <- cc_cen(x, lon = lon, lat = lat, species = species)
  x <- cc_dupl(x, lon = lon, lat = lat, species = species)
  x <- cc_equ(x, lon = lon, lat = lat)
  x <- cc_inst(x, lon = lon, lat = lat, species = species)
  x <- cc_val(x, lon = lon, lat = lat)
  if (terrestrial) {
    x <- cc_sea(x, lon = lon, lat = lat)
  }
  if (!is.null(pred)) {
    print("Predictors identified, procceding with grid filter (removing NA and duplicated data).")
    x2 <- st_as_sf(x,
      coords = c(lon, lat),
      crs = st_crs(occ$occurrences$epsg)
    )
    if (!st_crs(x2) == st_crs(pred$grid)) {
      print("CRS from predictors is different from occurrences' CRS. Ocurrences' CRS will be transformed to predictors' CRS.")
      x2 <- st_transform(x2, crs = st_crs(pred$grid))
      y$epsg <- pred$epsg
    }

    if (class(pred) == "predictors") {
      preds <- st_rasterize(st_as_sf(pred$grid))
      x2 <- st_transform(x2, crs = st_crs(preds))
      teste <- cbind(st_extract(preds, x2), x2$species)
      x <- na.omit(teste[!duplicated(select(as.data.frame(teste), -"geometry")), ])
      colnames(x) <- c("cell_id", "species", "geometry")
    } else if (class(pred) == "sdm_area") {
      teste <- pred$grid |>
        st_rasterize() |>
        st_extract(x2) |>
        mutate(species = x2$species) |>
        na.omit()
      dup_rows <- teste |>
        as.data.frame() |>
        select(-"geometry") |>
        duplicated()
      x <- teste[!dup_rows, c("species", "geometry")]
      x <- st_join(pred$grid, x)
      x <- x[, c("cell_id", "species", predictors_names(pred), "geometry")]
    }
  }
  y$occurrences <- x
  clean_methods <- c("NAs", "Capitals", "Centroids", "Geographically Duplicated", "Identical Lat/Long", "Institutions", "Invalid")
  if (terrestrial) {
    clean_methods <- c(clean_methods, "Non-terrestrial")
  }
  if (!is.null(pred)) {
    clean_methods <- c(clean_methods, "Duplicated Cell (grid)")
  }
  y$n_presences <- table(y$occurrences$species)

  if ("independent_test" %in% names(y) & independent_test) {
    x <- y$independent_test
    x <- subset(x, !is.na(lon) | !is.na(lat))
    x <- cc_cap(x, lon = lon, lat = lat, species = species)
    x <- cc_cen(x, lon = lon, lat = lat, species = species)
    x <- cc_dupl(x, lon = lon, lat = lat, species = species)
    x <- cc_equ(x, lon = lon, lat = lat)
    x <- cc_inst(x, lon = lon, lat = lat, species = species)
    x <- cc_val(x, lon = lon, lat = lat)
    if (terrestrial) {
      x <- cc_sea(x, lon = lon, lat = lat)
    }
    if (!is.null(pred)) {
      print("Predictors identified, procceding with grid filter.")
      x2 <- x
      coordinates(x2) <- c(lon, lat)
      x2 <- st_as_sf(x2)
      st_crs(x2) <- as.character(st_crs(y$epsg))[1]
      preds <- st_rasterize(st_as_sf(pred$grid))
      x2 <- st_transform(x2, crs = st_crs(preds))
      teste <- cbind(st_extract(preds, x2), x2$species)
      x <- na.omit(teste[!duplicated(teste), ])
      colnames(x) <- c("cell_id", "species", "geometry")
    }
    y$independent_test <- x
    clean_methods <- c(clean_methods, "Methods also applied in independent_test")
  }
  y$data_cleaning <- clean_methods

  if (class(occ) == "input_sdm") {
    occ$occurrences <- y
    y <- occ
  }
  return(y)
}
