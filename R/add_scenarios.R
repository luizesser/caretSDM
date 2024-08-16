#' Add scenarios to \code{sdm_area}
#'
#' This function includes scenarios in the \code{sdm_area} object.
#'
#' @usage add_scenarios(sdm_area, scen = NULL, variables_selected = NULL)
#'
#' @param sdm_area A \code{sdm_area} object.
#' @param scen \code{RasterStack}, \code{SpatRaster} or \code{stars} object. If \code{NULL} adds
#' predictors as a scenario.
#' @param variables_selected Character vector with variables names in \code{scen} to be used as
#' predictors. If \code{NULL} adds all variables.
#'
#' @return The input \code{sdm_area} object with a new slot called scenarios with \code{scen} data
#' as a \code{list}, where each slot of the \code{list} is a scenario and each scenario is a
#' \code{sf}.
#'
#' @seealso \code{\link{sdm_area} \link{scenarios}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' # Include predictors
#' sa <- add_predictors(sa, bioc)
#'
#' # Include scenarios
#' sa <- add_scenarios(sa, scen)
#'
#' @importFrom stars read_stars st_as_stars st_dimensions st_get_dimension_values
#' @importFrom sf st_transform st_crs st_as_sf st_crop
#' @importFrom dplyr select all_of
#'
#' @export
add_scenarios <- function(sdm_area, scen = NULL, variables_selected = NULL, ...) {
  UseMethod("add_scenarios", scen)
}

#' @export
add_scenarios.NULL <- function(sdm_area, scen = NULL, variables_selected = NULL, ...) {
  sa_teste <- sdm_area
  sa_teste$data <- list(current = sa_teste$grid)
  sdm_area$scenarios <- sa_teste
  return(sdm_area)
}

# #' @export
# add_scenarios.character <- function(sdm_area, scen, scenarios_names = NULL, pred_as_scen = TRUE,
#                                     variables_selected = NULL, ...) {
#   assert_file_exists_cli(scen)
#
#   sa_teste <- sdm_area
#   if (length(scen) > 1) {
#     s <- stars::read_stars(scen)
#     if (is.null(scenarios_names)) {
#       if (length(unique(scen)) == length(scen)) {
#         scenarios_names <- basename(scen)
#       } else {
#         paste0("scenario_", 1:length(scen))
#       }
#     }
#   } else {
#     l <- scen
#     s <- stars::read_stars(l, along='band')
#     if (is.null(scenarios_names)) {
#       if (length(unique(l)) == length(l)) {
#         scenarios_names <- basename(l)
#       } else {
#         paste0("scenario_", 1:length(l))
#       }
#     }
#   }
#   names(s) <- scenarios_names
#   res <- add_scenarios(sdm_area, s, scenarios_names)
#   return(res)
# }

#' @export
add_scenarios.RasterStack <- function(sdm_area, scen, scenarios_names = NULL, pred_as_scen = TRUE,
                                      variables_selected = NULL, ...) {
  scen <- stars::st_as_stars(scen)
  sa <- add_scenarios(sdm_area, scen, scenarios_names, pred_as_scen, variables_selected)
  return(sa)
}

#' @export
add_scenarios.SpatRaster <- function(sdm_area, scen, scenarios_names = NULL, pred_as_scen = TRUE,
                                     variables_selected = NULL, ...) {
  scen <- stars::st_as_stars(scen)
  names(stars::st_dimensions(scen)) <- c("x", "y", "band")
  sa <- add_scenarios(sdm_area, scen, scenarios_names, pred_as_scen, variables_selected)
  return(sa)
}

#' @export
add_scenarios.stars <- function(sdm_area, scen, scenarios_names = NULL, pred_as_scen = TRUE,
                                variables_selected = NULL, ...) {
  pres_names <- get_predictor_names(sdm_area)

  assert_choice_cli(
    x = variables_selected,
    choices = pres_names,
    null.ok = T,
    .var.name = "variables_selected"
  )

  sa_data <- sdm_area
  sa <- sdm_area

  if (!is.null(variables_selected)) {
    assert_names_cli(
      variables_selected,
      subset.of = stars::st_get_dimension_values(scen, "band")
    )
    scen <- scen[, , , variables_selected]
  } else {
    variables_selected <- get_predictor_names(sa)
  }

  if (is.null(scenarios_names)) {
    scenarios_names <- names(scen)
  }

  grid_t <- sf::st_transform(sa$grid, sf::st_crs(scen))
  suppressWarnings(scen <- sf::st_crop(scen, grid_t))

  l <- list()
  for (i in 1:length(scen)) {
    l[[scenarios_names[i]]] <- scen[i] |>
      aggregate(grid_t, mean) |>
      sf::st_as_sf() |>
      sf::st_transform(sf::st_crs(sdm_area$grid)) |>
      cbind(select(sdm_area$grid, "cell_id")) |>
      dplyr::select(dplyr::all_of(c("cell_id", variables_selected)))
  }

  if (pred_as_scen) {
    l[["current"]] <- sdm_area$grid |>
      dplyr::select(c(cell_id, dplyr::all_of(variables_selected)))
  }

  sa_data$data <- l
  sa_data$grid <- sdm_area$grid |>
    dplyr::select(c(cell_id, dplyr::all_of(variables_selected)))
  sdm_area$scenarios <- sa_data
  sdm_area$grid <- sa_data$grid

  return(sdm_area)
}
