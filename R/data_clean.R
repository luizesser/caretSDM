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
data_clean <- function(occ, pred=NULL, species=NA, long=NA, lat=NA, terrestrial=TRUE){
  if(class(occ)=='input_sdm'){
    y <- occ$occurrences
    if(is.null(pred)){
      pred <- occ$predictors
    }
  } else {
    y <- occ
  }
  if(!is.na(species)){species=species} else {species='species'}
  if(!is.na(long)){lon=long} else {lon='decimalLongitude'}
  if(!is.na(lat)){lat=lat} else {lat='decimalLatitude'}
  x <- y$occurrences
  x <- subset( x, !is.na(lon) | !is.na(lat))
  x <- cc_cap( x, lon = lon, lat = lat, species = species)
  x <- cc_cen( x, lon = lon, lat = lat, species = species)
  x <- cc_dupl(x, lon = lon, lat = lat, species = species)
  x <- cc_equ( x, lon = lon, lat = lat)
  #x <- cc_inst(x, lon = lon, lat = lat, species = species)
  x <- cc_val( x, lon = lon, lat = lat)
  if(terrestrial){x <- cc_sea( x, lon = lon, lat = lat)}
  if(!is.null(pred)){
    print('Predictors identified, procceding with grid filter.')
    r <- raster(i$predictors$grid)
    values(r) <- 1:ncell(r)
    x2 <- x
    coordinates(x2) <- 2:3
    cell_id <- extract(r, x2)
    x <- cbind(x, cell_id)
    x <- x[!duplicated(x[,c(1,4)]),-4]
  }
  y$occurrences <- x
  clean_methods <- c('NAs', 'Capitals', 'Centroids', 'Geographically Duplicated', 'Identical Lat/Long', 'Institutions', 'Invalid')
  if(terrestrial){clean_methods <- c(clean_methods,'Non-terrestrial')}
  if(!is.null(pred)){clean_methods <- c(clean_methods,'Duplicated Cell')}
  y$data_cleaning <- clean_methods
  y$n_presences <- nrow(y$occurrences)

  if(class(occ)=='input_sdm'){
    i$occurrences <- y
    y <- i
  }
  return(y)
}
