#' Presence data cleaning wrapper
#'
#' @description Data cleaning procedure using CoordinateCleaner and a raster.
#' @usage data.clean(x, r = NULL)
#' @param occ A occurrences object
#' @param pred A predictors object
#' @param terrestrial If the species is terrestrial (TRUE), than the function deletes non-terrestrial coordinates.
#' @details Function to clean presence data avoiding autocorrelation.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' # Select species names:
#' s <- c("Araucaria angustifolia", "Paubrasilia echinata", "Eugenia uniflora")
#'
#' # Run function:
#' data <- GBIF_data(s)
#'
#' # Clean coordinates:
#' data2 <- data.clean(data)
#'
#' @import CoordinateCleaner
#' @import raster
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
    species <- colnames(y$occurrences)[1]
  }
  if (!is.na(long)) {
    lon <- long
  } else {
    lon <- colnames(y$occurrences)[2]
  }
  if (!is.na(lat)) {
    lat <- lat
  } else {
    lat <- colnames(y$occurrences)[3]
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
             crs = st_crs(occ$occurrences$epsg))
    if(!st_crs(x2) == st_crs(pred$grid)){
      print("CRS from predictors is different from occurrences' CRS. Ocurrences' CRS will be transformed to predictors' CRS.")
      x2 <- st_transform(x2, crs = st_crs(pred$grid))
      y$epsg <- pred$epsg
    }

    if(class(pred)=="predictors"){
      preds <- st_rasterize(st_as_sf(pred$grid))
      x2 <- st_transform(x2, crs = st_crs(preds))
      teste <- cbind(st_extract(preds, x2), x2$species)
      x <- na.omit(teste[!duplicated(select(as.data.frame(teste), -"geometry")), ])
      colnames(x) <- c("cell_id", "species", "geometry")
    } else if(class(pred)=="sdm_area") {
      teste <- pred$grid |>
        st_rasterize() |>
        st_extract(x2) |>
        mutate(species=x2$species) |>
        na.omit()
      dup_rows <- teste |>
        as.data.frame() |>
        select(-"geometry") |>
        duplicated()
      x <- teste[!dup_rows,c("species", "geometry")]
      x <- st_join(pred$grid, x)
      x <- x[,c('cell_id', 'species', predictors_names(pred), 'geometry')]
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
