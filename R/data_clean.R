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
data_clean <- function(occ, pred=NULL, terrestrial=TRUE){
  x <- occ$occurrences
  x <- subset( x, !is.na("decimalLongitude") | !is.na("decimalLatitude"))
  x <- cc_cap( x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_cen( x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_dupl(x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_equ( x, lon = "decimalLongitude", lat = "decimalLatitude")
  x <- cc_inst(x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_val( x, lon = "decimalLongitude", lat = "decimalLatitude")
  if(terrestrial){x <- cc_sea( x, lon = "decimalLongitude", lat = "decimalLatitude")}
  if(!is.null(pred)){
    print('Predictors identified, procceding with grid filter.')
    # falar com reginaldo/ incluir grid do próprio raster usado no predictors ou grid tirado por geoprocessamento.
  }
  occ$occurrences <- x
  clean_methods <- c('NAs', 'Capitals', 'Centroids', 'Geographically Duplicated', 'Identical Lat/Long', 'Institutions', 'Invalid')
  if(terrestrial){clean_methods <- c(clean_methods,'Non-terrestrial')}
  if(!is.null(pred)){clean_methods <- c(clean_methods,'Environmentally Duplicated')}
  occ$data_cleaning <- clean_methods
  occ$n_presences <- nrow(occ$occurrences)
  return(occ)
}
