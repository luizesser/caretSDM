#' Add scenarios to \code{sdm_area}
#'
#' This function includes scenarios in the \code{sdm_area} object.
#'
#' @usage add_scenarios(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
#'                      variables_selected = NULL, stationary = NULL, ...)
#'
#' @param sa A \code{sdm_area} object.
#' @param scen \code{RasterStack}, \code{SpatRaster} or \code{stars} object. If \code{NULL} adds
#' predictors as a scenario.
#' @param variables_selected Character vector with variables names in \code{scen} to be used as
#' predictors. If \code{NULL} adds all variables.
#' @param stationary Names of variables from \code{sa} that should be used in scenarios as
#' stationary variables.
#' @param scenarios_names Character vector with names of scenarios.
#' @param pred_as_scen Logical. If \code{TRUE} adds the current predictors as a scenario.
#'
#' @return The input \code{sdm_area} object with a new slot called scenarios with \code{scen} data
#' as a \code{list}, where each slot of the \code{list} is a scenario and each scenario is a
#' \code{sf}.
#'
#' @seealso \code{\link{sdm_area} \link{input_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa)
#'
#' @importFrom stars read_stars st_as_stars st_dimensions st_get_dimension_values
#' @importFrom sf st_transform st_crs st_as_sf st_crop st_join
#' @importFrom dplyr select all_of
#' @importFrom tidyr drop_na
#' @importFrom cli cli_progress_along
#'
#' @export
add_scenarios <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                          variables_selected = NULL, stationary = NULL, ...) {
  UseMethod("add_scenarios", scen)
}

#' @export
add_scenarios.NULL <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                               variables_selected = NULL, stationary = NULL, ...) {
  if(is_sdm_area(sa)){
    sa_teste <- sa
    sa_teste$data <- list(current = sa_teste$grid)
    sa$scenarios <- sa_teste
    return(sa)
  } else if (is_input_sdm(sa)){
    sa_teste <- sa$predictors
    sa_teste$data <- list(current = sa_teste$grid)
    sa$scenarios <- sa_teste
    return(sa)
  }

}

# #' @export
# add_scenarios.character <- function(sa, scen, scenarios_names = NULL, pred_as_scen = TRUE,
#                                     variables_selected = NULL, ...) {
#   assert_file_exists_cli(scen)
#
#   sa_teste <- sa
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
#   res <- add_scenarios(sa, s, scenarios_names)
#   return(res)
# }

#' @export
add_scenarios.RasterStack <- function(sa, scen=NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                      variables_selected = NULL, stationary = NULL, ...) {
  scen <- stars::st_as_stars(scen)
  sa <- add_scenarios(sa, scen, scenarios_names, pred_as_scen, variables_selected, stationary)
  return(sa)
}

#' @export
add_scenarios.SpatRaster <- function(sa, scen=NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                     variables_selected = NULL, stationary = NULL, ...) {
  scen <- stars::st_as_stars(scen)
  names(stars::st_dimensions(scen)) <- c("x", "y", "band")
  sa <- add_scenarios(sa, scen, scenarios_names, pred_as_scen, variables_selected, stationary)
  return(sa)
}

#' @export
add_scenarios.stars <- function(sa, scen=NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                variables_selected = NULL, stationary = NULL, ...) {
  # stationary assertion must include an empty vector. Empty vector must be changed to NULL.
  if (is_input_sdm(sa)) {
    if("scenarios" %in% names(sa)){      # NEM SEMPRE TEM $scenarios: Precisa verificar.
        i2 <- sa
        sa <- i2$scenarios
    } else {
        sa <- add_scenarios(sa)
        i2 <- sa
        sa <- i2$scenarios
    }
    add_sc <- ifelse(length(sa$data)>0, TRUE, FALSE)
  } else if ( is_sdm_area(sa) ) {
    add_sc <- ifelse(length(sa$scenarios$data)>0, TRUE, FALSE)
  }

  if (is.null(scenarios_names)) { scenarios_names <- names(scen) }
  pres_names <- get_predictor_names(sa)

  if(!is.null(stationary) & exists("i2")){
    stationary_grd <- sa$grid |> dplyr::select(all_of(c("cell_id", stationary)))
    stationary_grd <- sf::st_transform(stationary_grd, sf::st_crs(scen))
    variables_selected <- pres_names[!pres_names %in% stationary]
    missing_vars <- variables_selected[!variables_selected %in% stars::st_get_dimension_values(scen, "band")]
    if(length(missing_vars > 0)) {
      len <- length(missing_vars)
      cli::cli_abort(c("{.var scen} does not have all variables from {.var variables_selected}:",
                  "x" = "There {?is/are} {len} variables missing from {.var scen}.",
                  "i" = "Check for: {missing_vars}"))
    }
    scen <- scen[, , , variables_selected]
    l <- list()
    for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
      l[[scenarios_names[i]]]  <- scen[i] |>
        aggregate(stationary_grd, mean) |>
        sf::st_as_sf() |>
        cbind(stationary_grd) |>
        sf::st_transform(sf::st_crs(sa$grid)) |>
        tidyr::drop_na() |>
        dplyr::select(dplyr::all_of(c("cell_id", pres_names, "geometry")))
    }
    if(!"current" %in% scenarios_names(i2)){
      if (pred_as_scen) {
        l[["current"]] <- sa$grid |>
          dplyr::select(dplyr::all_of(c("cell_id", pres_names)))
      }
    }

    sa_data <- sa

    if(add_sc){
      sa_data$data <- c(sa$data,l)
    } else {
      sa_data$data <- l
    }

    sa_data$grid <- sa$grid
    i2$scenarios <- sa_data
    return(i2)

  } else if ( !is.null(stationary) & !exists("i2") ) {
    stationary_grd <- sa$grid |> dplyr::select(all_of(c("cell_id", stationary)))
    variables_selected <- pres_names[!pres_names %in% stationary]
    scen <- scen[, , , variables_selected]

    l <- list()
    if(unique(st_geometry_type(sa$grid)) == "LINESTRING") {
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        scen_area <- scen[i] |>
          sdm_area(
            cell_size = sa$cell_size,
            crs = stationary_grd |> sf::st_crs(),
            variables_selected = variables_selected,
            gdal = TRUE,
            crop_by = stationary_grd
          )

        l[[scenarios_names[i]]]  <- scen_area$grid |>
          dplyr::select(-"cell_id") |>
          aggregate(stationary_grd, mean) |>
          sf::st_cast("LINESTRING") |>
          cbind(stationary_grd) |>
          dplyr::select(-"geometry.1") |>
          dplyr::relocate(c("cell_id", variables_selected, stationary, "geometry"))
      }
    } else {
      stationary_grd <- sf::st_transform(stationary_grd, sf::st_crs(scen))
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        l[[scenarios_names[i]]]  <- scen[i] |>
          aggregate(stationary_grd, mean) |>
          sf::st_as_sf() |>
          cbind(stationary_grd) |>
          sf::st_transform(sf::st_crs(sa$grid)) |>
          tidyr::drop_na() |>
          dplyr::select(dplyr::all_of(c("cell_id", pres_names, "geometry")))
      }
    }

    sa_data <- sa

    if(!"current" %in% scenarios_names(sa) & !"current" %in% names(l)){
      if (pred_as_scen) {
        l[["current"]] <- sa$grid |>
          dplyr::select(dplyr::all_of(c("cell_id", pres_names)))
      }
    }

    if(add_sc){
      sa_data$data <- c(sa$scenarios$data,l)
    } else {
      sa_data$data <- l
    }

    sa_data$grid <- sa$grid
    sa$scenarios <- sa_data
    return(sa)

  } else if ( is.null(stationary) ) {

    if ( !test_variables_names(sa, scen) ) {
      scen <- set_variables_names(scen, sa)
    }

    caretSDM:::assert_choice_cli(
      x = variables_selected,
      choices = pres_names,
      null.ok = T,
      .var.name = "variables_selected"
    )

    sa_data <- sa

    if (!is.null(variables_selected)) {
      caretSDM:::assert_names_cli(
        variables_selected,
        subset.of = stars::st_get_dimension_values(scen, "band")
      )
      scen <- scen[, , , variables_selected]
    } else {
      variables_selected <- get_predictor_names(sa)
      if(!all(variables_selected %in% st_get_dimension_values(scen, "band"))) {
        variables_selected <- st_get_dimension_values(scen, "band")[st_get_dimension_values(scen, "band") %in% variables_selected]
        cli::cli_warn(c("Some variables in {.var variables_selected} are not present in {.var scen}.",
                        "i" = "Using only variables present in {.var scen}: {variables_selected}"))
      }
    }

    #grid_t <- sf::st_transform(sa$grid, sf::st_crs(scen))
    grid_t <- sa$grid

    l <- list()
    if(unique(st_geometry_type(grid_t)) == "LINESTRING") {
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        scen_area <- scen[i] |>
          sdm_area(
            cell_size = sa$cell_size,
            crs = grid_t |> sf::st_crs(),
            variables_selected = variables_selected,
            gdal = TRUE,
            crop_by = grid_t
          )

        l[[scenarios_names[i]]]  <- scen_area$grid |>
          dplyr::select(-"cell_id") |>
          aggregate(grid_t, mean) |>
          sf::st_cast("LINESTRING") |>
          cbind(grid_t) |>
          dplyr::select(c("cell_id", variables_selected, "geometry"))
      }
    } else {
      grid_t <- st_transform(grid_t, sf::st_crs(scen))
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        l[[scenarios_names[i]]]  <- scen[i] |>
          aggregate(grid_t, mean) |>
          sf::st_as_sf() |>
          cbind(grid_t) |>
          sf::st_transform(sf::st_crs(sa$grid)) |>
          tidyr::drop_na() |>
          dplyr::select(dplyr::all_of(c("cell_id", variables_selected, "geometry")))
      }
    }

    if (pred_as_scen) {
      l[["current"]] <- sa$grid |>
        dplyr::select(c(cell_id, dplyr::all_of(variables_selected)))
    }

    if(add_sc){
      sa_data$data <- c(sa$data,l)
    } else {
      sa_data$data <- l
    }

    #sa_data$data <- l # =i$scenarios$data
    sa_data$grid <- sa$grid |>
      dplyr::select(c(cell_id, dplyr::all_of(variables_selected)))
    if ( !is.null(stationary) ) { sa_data$stationary <- stationary }
    sa_data$cell_size <- sa$cell_size

    sa$scenarios <- sa_data
    sa$grid <- sa_data$grid

    if (exists("i2")) {
      i2$scenarios <- sa$scenarios
      sa <- i2
    }

    return(sa)
  }
}

#' @rdname add_scenarios
#' @export
select_scenarios <- function(i, scenarios_names = NULL) {
  caretSDM:::assert_subset_cli(scenarios_names, scenarios_names(i), empty.ok = FALSE)
  if (is_input_sdm(i) | is_sdm_area(i)) {
    i$scenarios$data <- i$scenarios$data[scenarios_names]
  }
  return(i)
}

#' @rdname add_scenarios
#' @export
set_scenarios_names <- function(i, scenarios_names = NULL) {
  assert_class_cli(scenarios_names, "character")
  if(!length(scenarios_names) == length(scenarios_names(i))){
    cli::cli_abort(c("Length of {.var scenarios_names} must be equal to the number of scenarios in {.var i}.",
                     "x" = "Length of {.var scenarios_names}: {length(scenarios_names)}",
                     "i" = "Number of scenarios in {.var i}: {length(scenarios_names(i))}"))
  }
  if (is_input_sdm(i) | is_sdm_area(i)) {
    if("scenarios" %in% names(i)){names(i$scenarios$data) <- scenarios_names}
    if("predictions" %in% names(i)){
      names(i$predictions$predictions) <- scenarios_names
      colnames(i$predictions$ensembles) <- scenarios_names
    }
  }
  return(i)
}
