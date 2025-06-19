#' Add predictors to \code{sdm_area}
#'
#' This function includes new predictors to the \code{sdm_area} object.
#'
#' @usage add_predictors(sa, pred, variables_selected = NULL, gdal = TRUE)
#'
#' @param sa A \code{sdm_area} object.
#' @param pred \code{RasterStack}, \code{SpatRaster}, \code{stars} or \code{sf} object with predictors
#' data.
#' @param variables_selected \code{character} vector with variables names in \code{pred} to be used
#' as predictors. If \code{NULL} adds all variables.
#' @param gdal Boolean. Force the use or not of GDAL when available. See details.
#' @param i \code{input_sdm} or \code{sdm_area} object to retrieve data from.
#'
#' @details
#' \code{add_predictors} returns a \code{sdm_area} object with a grid built upon the \code{x} parameter.
#' There are two ways to make the grid and resample the variables in \code{sdm_area}: with and
#' without gdal. As standard, if gdal is available in you machine it will be used (\code{gdal = TRUE}),
#' otherwise sf/stars will be used.
#'
#' @returns For \code{add_predictors} the same input \code{sdm_area} object is returned including the
#' \code{pred} data binded to the previous \code{grid}.
#' \code{get_predictors} retrieves the grid from the \code{i} object.
#'
#' @seealso \code{\link{sdm_area} \link{predictors} \link{bioc}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com) and Reginaldo Ré.
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Retrieve predictors data:
#' get_predictors(sa)
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr inner_join join_by select
#' @importFrom sf st_crs st_bbox st_intersection st_cast
#' @importFrom tidyr drop_na
#'
#' @global cell_id geometry
#'
#' @export
add_predictors <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  if (!is_sdm_area(sa)) {
    cli::cli_abort(c(
      "x" = "The sa argument must be an instance of class sdm_area."
    ))
  }
  assert_cli(
    check_character_cli(
      variables_selected,
      unique = TRUE,
      null.ok = TRUE,
      len = 1
    )
  )
  assert_logical_cli(
    gdal,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  .check_sdm_area(sa)
  UseMethod("add_predictors", pred)
}

#' @export
add_predictors.RasterStack <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred <- sa  |>
    .add_predictors(pred, variables_selected, gdal)
  return(invisible(pred))
}

#' @export
add_predictors.SpatRaster <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred <- sa  |>
    .add_predictors(pred, variables_selected, gdal)
  return(invisible(pred))
}

#' @export
add_predictors.character <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred <- sa  |>
    .add_predictors(pred, variables_selected, gdal)
  return(invisible(pred))
}

#' @export
add_predictors.stars <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred <- sa  |>
    .add_predictors(pred, variables_selected, gdal)
  return(invisible(pred))
}

#' @export
add_predictors.sf <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred <- sa  |>
    .add_predictors(pred, variables_selected, gdal)
  return(invisible(pred))
}


.add_predictors <- function(sa, pred, variables_selected = NULL, gdal = TRUE) {
  pred_sa <- pred |>
    sdm_area(
      cell_size = sa$cell_size,
      crs = sa$grid |> sf::st_crs(),
      variables_selected = variables_selected,
      gdal = gdal,
      crop_by = sa$grid
    )
  if (is.null(pred_sa)){
    return(sa)
  }

  if(unique(st_geometry_type(sa$grid)) == "LINESTRING") {
    grd <- sa$grid |>
      sf::st_intersection(dplyr::select(pred_sa$grid, -cell_id))|>
      suppressWarnings()
    if("POINT" %in% st_geometry_type(grd)) {
      grd <- grd[st_geometry_type(grd) %in% c("LINESTRING","MULTILINESTRING"),]
    }
    grd <- grd |>
      sf::st_cast("LINESTRING") |>
      suppressWarnings()
    grd$cell_id <- 1:nrow(grd)
  } else {
    grd <- sa$grid |>
      dplyr::inner_join(
        pred_sa$grid |>
          as.data.frame() |>
          dplyr::select(-geometry),
        dplyr::join_by(cell_id)
      )
  }

  var_names <- colnames(grd)
  num_vars <- grepl("^[[:digit:]]+", names(grd))
  if(any(num_vars)){
    var_names2 <- gsub("^([0-9])", "X\\1", var_names)
    colnames(grd) <- var_names2
  }
  sa$grid <- grd

  return(sa)
}

#' @rdname add_predictors
#' @export
get_predictors <- function(i) {
  assert_cli(
    check_class_cli(i, c('input_sdm')),
    check_class_cli(i, c('sdm_area'))
  )
  if (is_input_sdm(i)) {
    i <- i$predictors
  }
  return(i$grid)
}
