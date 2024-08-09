#' Presence data cleaning routine
#'
#' Data cleaning wrapper using CoordinateCleaner.
#'
#' @usage
#' data_clean(x,
#'            pred = NULL,
#'            species = NA,
#'            lon = NA,
#'            lat = NA,
#'            terrestrial = TRUE,
#'            independent_test = TRUE)
#'
#' @param occ A \code{occurrences} object or \code{input_sdm}.
#' @param pred A \code{predictors} object. If \code{occ} is a \code{input_sdm} object with
#' predictors data, than \code{pred} is obtained from it.
#' @param species A \code{character} stating the name of the column with species names in \code{occ} (see details).
#' @param lon A \code{character} stating the name of the column with longitude in \code{occ} (see details).
#' @param lat A \code{character} stating the name of the column with latitude in \code{occ} (see details).
#' @param terrestrial If the species is terrestrial (TRUE), than the function deletes
#' non-terrestrial coordinates.
#' @param independent_test TRUE. If \code{occ} has independent test data, the data cleaning routine
#' is also applied on it.
#'
#' @returns A \code{occurrences} object or \code{input_sdm} with new presence data.
#'
#' @details
#' If the user does not used \code{GBIF_data} function to obtain species records, the function may
#' have problems to find which column from the presences table has species, longitude and latitude
#' information. In this regard, we implemented the parameters \code{species}, \code{lon} and
#' \code{lat} so the use can explicitly inform which columns should be used. If they remain as NA
#' (standard) the function will try to guess which columns are the correct one.
#'
#' @seealso \code{\link{GBIF_data} \link{occurrences} \link{sdm_area} \link{input_sdm}
#' \link{predictors}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Create input_sdm:
#' i <- input_sdm(occurrences_sdm(occ, crs= 6933), sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#' i
#'
#' @importFrom CoordinateCleaner cc_cap cc_cen cc_dupl cc_equ cc_inst cc_val cc_sea
#' @importFrom stars st_extract st_rasterize
#' @importFrom stringdist stringdist
#' @importFrom sf st_as_sf st_crs st_transform st_join
#' @importFrom dplyr mutate select
#'
#' @export
data_clean <- function(occ, pred = NULL, species = NA, lon = NA, lat = NA, terrestrial = TRUE,
                       independent_test = TRUE) {
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    if (is.null(pred)) {
      pred <- occ$predictors
    }
  } else {
    y <- occ
  }
  if(sf::st_crs(y) !=4326){
    sf_t <- sf::st_transform(y$occurrences, 4326)
    x <- sf_to_df_sdm(sf_t)
  } else {
    x <- sf_to_df_sdm(y$occurrences)
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
  x <- CoordinateCleaner::cc_cap(x, lon = lon, lat = lat, species = species)
  x <- CoordinateCleaner::cc_cen(x, lon = lon, lat = lat, species = species)
  x <- CoordinateCleaner::cc_dupl(x, lon = lon, lat = lat, species = species)
  x <- CoordinateCleaner::cc_equ(x, lon = lon, lat = lat)
  x <- CoordinateCleaner::cc_inst(x, lon = lon, lat = lat, species = species)
  x <- CoordinateCleaner::cc_val(x, lon = lon, lat = lat)
  if (terrestrial) {
    x <- CoordinateCleaner::cc_sea(x, lon = lon, lat = lat)
  }
  if (!is.null(pred)) {
    print("Predictors identified, procceding with grid filter (removing NA and duplicated data).")
    x2 <- sf::st_as_sf(x,
      coords = c(lon, lat),
      crs = 4326)
    if (sf::st_crs(x2) != sf::st_crs(pred$grid)) {
      x2 <- sf::st_transform(x2, crs = sf::st_crs(pred$grid))
    }
    #if (is_predictors(pred)) {
    #  preds <- stars::st_rasterize(sf::st_as_sf(pred$grid))
    #  x2 <- sf::st_transform(x2, crs = sf::st_crs(preds))
    #  teste <- cbind(stars::st_extract(preds, x2), x2$species)
    #  x <- na.omit(teste[!duplicated(select(as.data.frame(teste), -"geometry")), ])
    #  colnames(x) <- c("cell_id", "species", "geometry")
    #  x <- select(x, c("species"))
    #} else
    if (is_sdm_area(pred)) {
      teste <- pred$grid |>
        stars::st_rasterize() |>
        stars::st_extract(x2) |>
        dplyr::mutate(species = x2$species) # |> na.omit()
      dup_rows <- teste |>
        as.data.frame() |>
        dplyr::select(-"geometry") |>
        duplicated()
      x <- teste[!dup_rows, c("species", "geometry")]
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
    if(as.character(st_crs(x))[1] != "EPSG:4326"){
      sf_t <- sf::st_transform(x, 4326)
      x <- sf_to_df_sdm(sf_t)
    } else {
      x <- sf_to_df_sdm(x)
    }
    cn <- colnames(x)
    species <- cn[which.min(stringdist::stringdist(cn, "species"))]
    lon <- cn[which.min(stringdist::stringdist(cn, "longitude"))]
    lat <- cn[which.min(stringdist::stringdist(cn, "latitude"))]
    x <- subset(x, !is.na(lon) | !is.na(lat))
    x <- CoordinateCleaner::cc_cap(x, lon = lon, lat = lat, species = species)
    x <- CoordinateCleaner::cc_cen(x, lon = lon, lat = lat, species = species)
    x <- CoordinateCleaner::cc_dupl(x, lon = lon, lat = lat, species = species)
    x <- CoordinateCleaner::cc_equ(x, lon = lon, lat = lat)
    x <- CoordinateCleaner::cc_inst(x, lon = lon, lat = lat, species = species)
    x <- CoordinateCleaner::cc_val(x, lon = lon, lat = lat)
    if (terrestrial) {
      x <- CoordinateCleaner::cc_sea(x, lon = lon, lat = lat)
    }
    if (!is.null(pred)) {
      print("Predictors identified, procceding with grid filter (removing NA and duplicated data).")
      x2 <- sf::st_as_sf(x,
                         coords = c(lon, lat),
                         crs = 4326)
      if (sf::st_crs(x2) != sf::st_crs(pred$grid)) {
        x2 <- sf::st_transform(x2, crs = sf::st_crs(pred$grid))
      }
      #if (is_predictors(pred)) {
      #  preds <- stars::st_rasterize(sf::st_as_sf(pred$grid))
      #  x2 <- sf::st_transform(x2, crs = sf::st_crs(preds))
      #  teste <- cbind(stars::st_extract(preds, x2), x2$species)
      #  x <- na.omit(teste[!duplicated(select(as.data.frame(teste), -"geometry")), ])
      #  colnames(x) <- c("cell_id", "species", "geometry")
      #  x <- select(x, c("species"))
      #} else
      if (is_sdm_area(pred)) {
        teste <- pred$grid |>
          stars::st_rasterize() |>
          stars::st_extract(x2) |>
          dplyr::mutate(species = x2$species) # |> na.omit()
        dup_rows <- teste |>
          as.data.frame() |>
          dplyr::select(-"geometry") |>
          duplicated()
        x <- teste[!dup_rows, c("species", "geometry")]
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
