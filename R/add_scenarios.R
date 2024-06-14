#' Include scenarios in the SDM workflow.
#'
#' This function includes scenarios in the \code{sdm_area} object.
#'
#' @usage add_scenarios(sdm_area, scen = NULL, variables_selected = NULL)
#'
#' @param sdm_area A \code{sdm_area} object.
#' @param scen \code{RasterStack}, \code{SpatRaster}, \code{stars} object or a directory with
#' scenarios data. If \code{NULL} adds predictors as a scenario.
#' @param variables_selected Character vector with variables names in \code{scen} to be used as
#' predictors. If \code{NULL} adds all variables.
#'
#' @return The input \code{sdm_area} object with a new slot called scenarios with \code{scen} data
#' as a \code{list}, where each slot of the \code{list} is a scenario.
#'
#' @seealso \code{\link{sdm_area}}
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
#' # Include scenarios
#' sa <- add_scenarios(sa, scen)
#'
#' @import checkmate
#' @import cli
#' @import stars
#' @import tibble
#' @import dplyr
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

#' @export
add_scenarios.character <- function(sdm_area, scen, scenarios_names = NULL,
                                    variables_selected = NULL, ...) {
  sa_teste <- sdm_area
  if (length(scen) > 1) {
    s <- read_stars(scen)
    if (is.null(scenarios_names)) {
      if (length(unique(scen)) == length(scen)) {
        scenarios_names <- basename(scen)
      } else {
        paste0("scenario_", 1:length(scen))
      }
    }
  } else {
    l <- list.files(x, full.names = T, ...)
    s <- read_stars(l)
    if (is.null(scenarios_names)) {
      if (length(unique(l)) == length(l)) {
        scenarios_names <- basename(l)
      } else {
        paste0("scenario_", 1:length(l))
      }
    }
  }
  names(s) <- scenarios_names
  res <- add_scenarios(sdm_area, s, scenarios_names)
  return(res)
}

#' @export
add_scenarios.RasterStack <- function(sdm_area, scen, scenarios_names = NULL,
                                      variables_selected = NULL, ...) {
  scen <- st_as_stars(scen)
  sa <- sdm_area(sdm_area, scen, scenarios_names, variables_selected)
  return(sa)
}

#' @export
add_scenarios.SpatRaster <- function(sdm_area, scen, scenarios_names = NULL,
                                     variables_selected = NULL, ...) {
  scen <- st_as_stars(scen)
  names(st_dimensions(scen)) <- c("x", "y", "band")
  sa <- sdm_area(sdm_area, scen, scenarios_names, variables_selected)
  return(sa)
}

#' @export
add_scenarios.stars <- function(sdm_area, scen, scenarios_names = NULL, pred_as_scen = TRUE,
                                variables_selected = NULL, ...) {
  sa_data <- sdm_area
  sa <- sdm_area
  if (!is.null(variables_selected)) {
    scen <- scen[,,,variables_selected]
  }
  fut_names <- scen[1] |>
    split("band") |>
    names()
  pres_names <- sdm_area$predictors
  if (!any(fut_names %in% pres_names)) {
    stop("The names of future variables do not match with any predictors")
  }

  if (is.null(scenarios_names)) {
    scenarios_names <- names(scen)
  }

  grid_t <- st_transform(sa$grid, st_crs(scen))
  scen <- st_crop(scen, grid_t)

  l <- list()
  for (i in 1:length(scen)) {
    l[[scenarios_names[i]]] <- scen[i] |>
      aggregate(grid_t, mean) |>
      st_as_sf() |>
      st_transform(st_crs(sdm_area$grid)) |>
      cbind(sdm_area$grid) |>
      select(c(cell_id, sdm_area$predictors))
  }
  if (pred_as_scen) {
    l[["current"]] <- sdm_area$grid
  }
  sa_data$data <- l
  sdm_area$scenarios <- sa_data
  return(sdm_area)
}

#' @export
.sdm_area <- function(x) {
  sa <- structure(
    list(
      grid = x$grid,
      bbox = x$bbox,
      cell_size = x$cell_size,
      epsg = x$epsg,
      predictors = x$predictors
    ),
    class = "sdm_area"
  )
  return(sa)
}

#' Print method for predictors
#' @exportS3Method base::print
print.sdm_area <- function(x) {
  cat("          caretSDM         \n")
  cat("...........................\n")
  cat("Class                     : sdm_area\n")
  cat("Extent                    :", x$bbox, "(xmin, xmax, ymin, ymax)\n")
  cat("EPSG                      :", x$epsg, "\n")
  cat("Resolution                :", x$cell_size, "(x, y)\n")
  if (!is.null(x$predictors)) {
    cat("Number of Predictors      :", length(x$predictors), "\n")
    cat(cat("Predictors Names          : "), cat(x$predictors, sep = ", "), "\n")
  }
}
