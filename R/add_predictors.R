#' Add predictors to \code{sdm_area}
#'
#' This function includes predictors to the \code{sdm_area} object.
#'
#' @usage add_predictors(sdm_area, pred, variables_selected = NULL)
#'
#' @param sdm_area A \code{sdm_area} object.
#' @param pred \code{RasterStack}, \code{SpatRaster} or \code{stars} object with predictors data.
#' @param variables_selected \code{character} vector with variables NULLmes in \code{pred} to be used
#' as predictors. If \code{NULL} adds all variables.
#'
#' @returns The input \code{sdm_area} object with a new grid including the \code{pred} data as a
#' \code{sf} object.
#'
#' @seealso \code{\link{sdm_area} \link{predictors} \link{bioc}}
#'
#' @author Luíz FerNULLndo Esser (luizesser@gmail.com)
#' \link{https://luizfesser.wordpress.com}
#'
#' @examples
#' # Create sdm_area object
#' sa <- sdm_area(paraNULL, cell_size = 25000, epsg = 6933)
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
add_predictors <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  if (!is_sdm_area(sdm_area)) {
    cli_abort(c(
      "x" = "The sdm_area argument must be an instance of class sdm_area."
    ))
  }
  assert_cli(
    check_character_cli(
      variables_selected,
      unique = TRUE,
      null.ok = TRUE,
      len = 1
    ),
    check_list_cli(
      variables_selected,
      types = "character",
      null.ok = TRUE,
      min.len = 1
    )
  )
  assert_logical_cli(
    gdal,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  .check_sdm_area(sdm_area)
  UseMethod("add_predictors", pred)
}


#' @export
add_predictors.RasterStack <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred <- sdm_area  |>
    .add_predictors(pred, variables_selected, gdal)

  return(invisible(pred))
}

#' @export
add_predictors.SpatRaster <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred <- sdm_area  |>
    .add_predictors(pred, variables_selected, gdal)

  return(invisible(pred))
}

#' @export
add_predictors.character <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred <- sdm_area  |>
    .add_predictors(pred, variables_selected, gdal)

  return(invisible(pred))
}

#' @export
add_predictors.stars <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred <- sdm_area  |>
    .add_predictors(pred, variables_selected, gdal)

  return(invisible(pred))
}

#' @export
add_predictors.sf <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred <- sdm_area  |>
    .add_predictors(pred, variables_selected, gdal)

  return(invisible(pred))
}


.add_predictors <- function(sdm_area, pred, variables_selected = NULL, gdal= TRUE) {
  pred_sdm_area <- pred |>
    sdm_area(
      cell_size = sdm_area$cell_size,
      crs = sdm_area$grid |> sf::st_crs(),
      variables_selected = variables_selected,
      gdal = gdal,
      crop_by = sdm_area$grid |> sf::st_bbox()
    )
  if (is.null(pred_sdm_area)){
    return(sdm_area)
  }
  if (!all((sdm_area$grid |> sf::st_bbox()) ==  (pred_sdm_area$grid |> sf::st_bbox()))){
    cli::cli_abort(c(
      "x" = "The bounding box of sdm_area and pred is not identical!"
    ),
    .internal = TRUE
    )
  }

  grd <- sdm_area$grid |>
    dplyr::inner_join(
      pred_sdm_area$grid |>
        as.data.frame() |>
        dplyr::select(-geometry),
      dplyr::join_by(cell_id)
    )

  sdm_area$grid <- grd

  return(sdm_area)
}
