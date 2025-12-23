#' Add scenarios to \code{sdm_area}
#'
#' This function includes scenarios in the \code{sdm_area} object.
#'
#' @usage add_scenarios(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
#'                      variables_selected = NULL, stationary = NULL, crop_area = NULL)
#'
#' @param sa A \code{sdm_area} or \code{input_sdm} object.
#' @param scen \code{RasterStack}, \code{SpatRaster} or \code{stars} object. If \code{NULL} adds
#' predictors as a scenario.
#' @param scenarios_names Character vector with names of scenarios.
#' @param variables_selected Character vector with variables names in \code{scen} to be used as
#' variables. If \code{NULL} adds all variables.
#' @param stationary Names of variables from \code{sa} that should be used in scenarios as
#' stationary variables.
#' @param pred_as_scen Logical. If \code{TRUE} adds the current predictors as a scenario.
#' @param crop_area A \code{sf} object to crop the \code{scen} object if necessary.
#' @param i A \code{sdm_area} or \code{input_sdm} object.
#'
#' @details
#' The function \code{add_scenarios} adds scenarios to the \code{sdm_area} or \code{input_sdm}
#' object. If \code{scen} has variables that are not present as predictors the function will use
#' only variables present in both objects. \code{stationary} variables are those that don't change
#' through the scenarios. It is useful for hidrological variables in fish habitat modeling, for
#' example (see examples below). When adding multiple scenarios in multiple runs, the function will
#' always add a new "current" scenario. To avoid that, set \code{pred_as_scen = FALSE}.
#'
#' @return \code{add_scenarios} returns the input \code{sdm_area} or \code{input_sdm} object with a
#' new slot called scenarios with \code{scen} data as a \code{list}, where each slot of the
#' \code{list} holds a scenario and each scenario is a \code{sf} object.
#' \code{set_scenarios_names} sets new names for scenarios in \code{sdm_area}/\code{input_sdm}
#' object.
#' \code{scenarios_names} returns scenarios' names.
#' \code{get_scenarios_data} retrieves scenarios data as a \code{list} of \code{sf} objects.
#' \code{select_scenarios} selects scenarios from \code{sdm_area}/\code{input_sdm} object.
#'
#' @seealso \code{\link{sdm_area} \link{input_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen[1:2]) |> select_predictors(c("bio1", "bio12"))
#'
#' # Set scenarios names:
#' sa <- set_scenarios_names(sa, scenarios_names = c("future_1", "future_2",
#'                                                   "current"))
#' scenarios_names(sa)
#'
#' # Get scenarios data:
#' scenarios_grid <- get_scenarios_data(sa)
#' scenarios_grid
#'
#' # Select scenarios:
#' sa <- select_scenarios(sa, scenarios_names = c("future_1"))
#'
#' # Setting stationary variables in scenarios:
#' sa <- sdm_area(rivs[c(1:200),], cell_size = 100000, crs = 6933, lines_as_sdm_area = TRUE) |>
#'   add_predictors(bioc) |>
#'   add_scenarios(scen, stationary = c("LENGTH_KM", "DIST_DN_KM"))
#'
#'
#' @importFrom stars read_stars st_as_stars st_dimensions st_get_dimension_values st_warp
#' @importFrom sf st_transform st_crs st_as_sf st_crop st_join st_geometry_type st_cast
#' @importFrom dplyr select all_of relocate
#' @importFrom tidyr drop_na
#' @importFrom cli cli_progress_along cli_abort cli_warn
#' @importFrom stats aggregate
#'
#' @export
add_scenarios <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                          variables_selected = NULL, stationary = NULL, crop_area = NULL) {
  UseMethod("add_scenarios", scen)
}

#' @export
add_scenarios.NULL <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                               variables_selected = NULL, stationary = NULL, crop_area = NULL) {
  if(is_sdm_area(sa)){
    sa_teste <- sa
  } else if (is_input_sdm(sa)){
    sa_teste <- sa$predictors
  }
  sa_teste$data[["current"]] <- sa_teste$grid
  sa$scenarios <- sa_teste
  return(sa)
}

#' @export
add_scenarios.character <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                               variables_selected = NULL, stationary = NULL, crop_area = NULL) {
  # check folder or document
  #scen = "/Users/luizesser/Documents/Mapas/Rasters/WorldClim 2.1/future_10m"
  if(checkmate::testDirectory(scen)) {
    files <- list.files(scen, pattern = ".tif", full.names = TRUE)
  } else if(checkmate::testFile(scen, extension = ".tif")) {
    files <- scen
  } else {
    stop()
  }

  if(!is.null(crop_area)) {
    s <- stars::read_stars(files, proxy = TRUE)
    if(sf::st_crs(s) != sf::st_crs(crop_area)) {
      crop_area <- sf::st_transform(crop_area, crs=sf::st_crs(s))
      crop_area <- .adjust_bbox(x = crop_area, cell_size = sa$cell_size)   # crop/ mudar bbox
    }
    s <- sf::st_crop(s, crop_area)
  } else {
    s <- stars::read_stars(files, proxy = FALSE)
  }

  # st_warp / gdal_warp (testar presença de GDAL)
  scen <- stars::st_as_stars(scen)
  sa <- add_scenarios(sa, s, scenarios_names, pred_as_scen, variables_selected, stationary,
                      crop_area)
  return(sa)
}

#' @export
add_scenarios.RasterStack <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                      variables_selected = NULL, stationary = NULL,
                                      crop_area = NULL) {
  scen <- stars::st_as_stars(scen)
  sa <- add_scenarios(sa, scen, scenarios_names, pred_as_scen, variables_selected, stationary,
                      crop_area)
  return(sa)
}

#' @export
add_scenarios.SpatRaster <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                     variables_selected = NULL, stationary = NULL,
                                     crop_area = NULL) {
  scen <- stars::st_as_stars(scen)
  names(stars::st_dimensions(scen)) <- c("x", "y", "band")
  sa <- add_scenarios(sa, scen, scenarios_names, pred_as_scen, variables_selected, stationary,
                      crop_area)
  return(sa)
}

#' @export
add_scenarios.stars <- function(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                                variables_selected = NULL, stationary = NULL,
                                crop_area = NULL) {
  # stationary assertion must include an empty vector. Empty vector must be changed to NULL.
  if (is_input_sdm(sa)) {
    if("scenarios" %in% names(sa)){
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

  if(sf::st_crs(sa$grid) != sf::st_crs(scen)) {
    scen <- stars::st_warp(scen, crs = sf::st_crs(sa$grid))
  }

  if(!is.null(crop_area)){
    assert_class_cli(crop_area, "sf")
    if(sf::st_crs(sa$grid) != sf::st_crs(crop_area)) {
      crop_area <- sf::st_transform(crop_area, sf::st_crs(sa$grid))
    }
    crop_area <- .adjust_bbox(crop_area, cell_size = sa$cell_size)
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

    bbox1 <- st_as_sfc(st_bbox(scen))
    bbox2 <- st_as_sfc(st_bbox(st_transform(stationary_grd, crs = st_crs(scen))))
    suppressMessages(intersects <- st_intersects(bbox1,
                                                 bbox2,
                                                 sparse = FALSE)[1,1])

    if(!intersects) {
      cli::cli_abort(c("Stationary data do not intersect with scenarios data",
                       "i" = "If you are projecting for the same area as modeling, your scenarios
                              are not intersecting with current area.",
                       "i" = "If you are trying to make a invasiveness assessment, you need to add
                              a current scenario before adding future scenarios."))
    }

    l <- list()
    for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
      l[[scenarios_names[i]]]  <- scen[i] |>
        stats::aggregate(stationary_grd, mean) |>
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
    bbox1 <- st_as_sfc(st_bbox(scen))
    bbox2 <- st_as_sfc(st_bbox(st_transform(stationary_grd, crs = st_crs(scen))))
    suppressMessages(intersects <- st_intersects(bbox1,
                                                    bbox2,
                                                    sparse = FALSE)[1,1])
    if(!intersects) {
      cli::cli_abort(c("Stationary data do not intersect with scenarios data",
                       "i" = "If you are projecting for the same area as modeling, your scenarios
                              are not intersecting with current area.",
                       "i" = "If you are trying to make a invasiveness assessment, you need to add
                              a current scenario before adding future scenarios."))
    }
    l <- list()
    if(unique(sf::st_geometry_type(sa$grid)) == "LINESTRING") {
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
          stats::aggregate(stationary_grd, mean) |>
          sf::st_cast("LINESTRING") |>
          suppressWarnings() |>
          cbind(stationary_grd) |>
          dplyr::select(-"geometry.1") |>
          dplyr::relocate(c("cell_id", all_of(c(variables_selected, stationary)), "geometry"))
      }
    } else {
      stationary_grd <- sf::st_transform(stationary_grd, sf::st_crs(scen))
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        l[[scenarios_names[i]]]  <- scen[i] |>
          stats::aggregate(stationary_grd, mean) |>
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

####################################################################################################
  } else if ( is.null(stationary) ) {

    if ( !test_variables_names(sa, scen) ) {
      scen <- set_variables_names(scen, sa)
    }

    assert_choice_cli(
      x = variables_selected,
      choices = pres_names,
      null.ok = TRUE,
      .var.name = "variables_selected"
    )

    sa_data <- sa

    if (!is.null(variables_selected)) {
      assert_names_cli(
        variables_selected,
        subset.of = stars::st_get_dimension_values(scen, "band")
      )
      scen <- scen[, , , variables_selected]
    } else {
      variables_selected <- get_predictor_names(sa)
      if(!all(variables_selected %in% stars::st_get_dimension_values(scen, "band"))) {
        variables_selected <- stars::st_get_dimension_values(scen, "band")[stars::st_get_dimension_values(scen, "band") %in% variables_selected]
        cli::cli_warn(c("Some variables in {.var variables_selected} are not present in {.var scen}.",
                        "i" = "Using only variables present in {.var scen}: {variables_selected}"))
      }
    }

    grid_t <- sa$grid

    l <- list()
    if(unique(sf::st_geometry_type(grid_t)) == "LINESTRING") {
      for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
        scen_area <- scen[i] |>
          sdm_area(
            cell_size = sa$cell_size,
            crs = grid_t |> sf::st_crs(),
            variables_selected = variables_selected,
            gdal = sa$parameters$gdal,
            lines_as_sdm_area = sa$parameters$lines_as_sdm_area
          )

        l[[scenarios_names[i]]]  <- scen_area$grid #|>
          #dplyr::select(-"cell_id") |>
          #stats::aggregate(grid_t, mean) |>
          #sf::st_cast("LINESTRING") |>
          #suppressWarnings() |>
          #cbind(grid_t) |>
          #dplyr::select(c("cell_id", variables_selected, "geometry"))
      }
    } else {
      #grid_t <- sf::st_transform(grid_t, sf::st_crs(scen))
      if(!is.null(crop_area)) {
        sa_crop <- sdm_area(x = crop_area,
                 cell_size = sa$cell_size,
                 crs = grid_t |> sf::st_crs(),
                 variables_selected = NULL, # Remove
                 gdal = sa$parameters$gdal,
                 crop_by = NULL, # Remove
                 lines_as_sdm_area = sa$parameters$lines_as_sdm_area)
        for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
          l1 <- add_predictors(sa = sa_crop,
                         pred = scen[i],
                         variables_selected = variables_selected,
                         gdal = sa$parameters$gdal)
          l1 <- select_predictors(l1, variables_selected)
          l[[scenarios_names[i]]]  <- l1$grid
        }
      } else {
        for (i in cli::cli_progress_along(1:length(scen), "Reescaling data")) {
          scen_area <- scen[i] |>
            sdm_area(
              cell_size = sa$cell_size,
              crs = grid_t |> sf::st_crs(),
              variables_selected = variables_selected,
              gdal = sa$parameters$gdal,
              crop_by = crop_area,
              lines_as_sdm_area = sa$parameters$lines_as_sdm_area
            )

          l[[scenarios_names[i]]]  <- scen_area$grid
        }
      }
    }

    if (pred_as_scen & !"current" %in% scenarios_names(sa_data)) {
      l[["current"]] <- sa$grid |>
        dplyr::select(c(cell_id, dplyr::all_of(variables_selected)))
    }

    if(add_sc){
      sa_data$data <- c(sa$data,l)
    } else {
      sa_data$data <- l
    }

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

#' @rdname add_scenarios
#' @export
scenarios_names <- function(i) {
  if (is_input_sdm(i) | is_sdm_area(i)) {
    if("scenarios" %in% names(i)) {
      return(names(i$scenarios$data))
    }
    if("data" %in% names(i)) {
      return(names(i$data))
    }
  }
  return(NULL)
}

#' @rdname add_scenarios
#' @export
get_scenarios_data <- function(i) {
  if (is_input_sdm(i) | is_sdm_area(i)) {
    return(i$scenarios$data)
  }
  return(NULL)
}

#' @rdname add_scenarios
#' @export
select_scenarios <- function(i, scenarios_names = NULL) {
  assert_subset_cli(scenarios_names, scenarios_names(i), empty.ok = FALSE)
  if (is_input_sdm(i) | is_sdm_area(i)) {
    i$scenarios$data <- i$scenarios$data[scenarios_names]
  }
  return(i)
}

