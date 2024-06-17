#' Add predictors to \code{sdm_area}
#'
#' This function includes predictors to the \code{sdm_area} object.
#'
#' @usage add_predictors(sdm_area, pred, variables_selected = NULL)
#'
#' @param sdm_area A \code{sdm_area} object.
#' @param pred \code{RasterStack}, \code{SpatRaster} or \code{stars} object with predictors data.
#' @param variables_selected \code{character} vector with variables names in \code{pred} to be used
#' as predictors. If \code{NULL} adds all variables.
#'
#' @returns The input \code{sdm_area} object with a new grid including the \code{pred} data as a
#' \code{sf} object.
#'
#' @seealso \code{\link{sdm_area} \link{predictors} \link{bioc}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' \link{https://luizfesser.wordpress.com}
#'
#' @examples
#' # Create sdm_area object
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' # Include predictors
#' sa <- add_predictors(sa, bioc)
#'
#' @import checkmate
#' @import cli
#' @import stars
#' @import tibble
#' @import dplyr
#'
#' @export
add_predictors <- function(sdm_area, pred, variables_selected = NULL) {
  if (!is_sdm_area(sdm_area)) {
    cli_abort("first argument is not of class sdm_area")
  }
  UseMethod("add_predictors", pred)
}

#' @export
add_predictors.RasterStack <- function(sdm_area, pred, variables_selected = NULL) {
  pred <- st_as_stars(pred)
  xa <- add_predictors(sdm_area, pred)
  return(xa)
}

#' @export
add_predictors.SpatRaster <- function(sdm_area, pred, variables_selected = NULL) {
  xs <- st_as_stars(pred)
  names(st_dimensions(xs)) <- c("x", "y", "band")
  xa <- add_predictors(sdm_area, xs)
  return(xa)
}

#' @export
add_predictors.stars <- function(sdm_area, pred, variables_selected = NULL) {
  if (length(names(pred)) > 1) {
    cli_abort(c(
      "x has more than 1 attribute:",
      "i" = "There {?is/are} {len} element{?s}."
    ))
  }
  if (!is.null(variables_selected)) {
    pred <- pred[, , , variables_selected]
  }
  grd <- sdm_area$grid
  grd <- st_transform(grd, crs = st_crs(pred))
  grd2 <- pred |>
    st_crop(grd) |>
    st_as_sf() |>
    st_centroid() |>
    aggregate(grd, mean) |>
    cbind(grd) |>
    select(-c("geometry.1"))
  grd2 <- st_transform(grd2, crs = st_crs(sdm_area$grid))
  bbox2 <- st_bbox(grd2)
  var_names <- grd2 %>%
    as_tibble() %>%
    select(-c("geometry", "cell_id")) %>%
    colnames()
  l <- list(
    grid = grd2,
    bbox = bbox2,
    cell_size = sdm_area$cell_size,
    epsg = sdm_area$epsg,
    predictors = var_names
  )
  sa <- .sdm_area(l)
  return(sa)
}
