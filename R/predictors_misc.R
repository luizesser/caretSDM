#' Add predictors to sdm_area
#'
#' This function adds a new predictor to the sdm_area object.
#'
#' @param x A \code{sf}, \code{RasterStack}, \code{SpatRaster} or \code{stars}.
#' @param cell_size Numeric. The cell size to be used in models.
#' @param epsg Numeric. If epsg from \code{x} is missing,
#'
#' @return A \code{sdm_area} object.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import checkmate
#' @import cli
#' @import stars
#' @import tibble
#' @import dplyr
#'
#' @export
predictors_names <- function(sdm_area) {
  return(sdm_area$predictors)
}

#' @export
set_predictors_names <- function(sdm_area, new_names) {
  colnames(sdm_area$grid) <- c("id", new_names, "geometry")
  return(sdm_area)
}

#' @export
get_predictors <- function(sdm_area) {
  return(sdm_area$grid)
}
