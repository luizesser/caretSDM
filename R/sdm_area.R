#' Create a \code{sdm_area} object
#'
#' This function creates a new \code{sdm_area} object.
#'
#' @param x A shapefile or a raster. Usually a shapefile from \code{sf} class, but rasters from
#' \code{stars}, \code{rasterStack} or \code{SpatRaster} class are also allowed.
#' @param cell_size \code{numeric}. The cell size to be used in models.
#' @param epsg \code{numeric}. Indicates which EPSG should the output grid be in. If \code{NULL},
#' epsg from \code{x} is used.
#'
#' @details
#' The function returns a \code{sdm_area} object with a grid built upon the \code{x} parameter.
#'
#' @returns A \code{sdm_area} object.
#'
#' @seealso \code{\link{WorldClim_data} \link{parana} \link{input_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' @import checkmate
#' @import cli
#' @import stars
#' @import tibble
#' @import dplyr
#'
#' @export
sdm_area <- function(x, cell_size, epsg) {
  UseMethod("sdm_area")
}

#' @export
sdm_area.RasterStack <- function(x, cell_size = NULL, epsg = NULL) {
  xs <- st_as_stars(x)
  sa <- sdm_area(xs, cell_size, epsg)
  return(sa)
}

#' @export
sdm_area.SpatRaster <- function(x, cell_size = NULL, epsg = NULL) {
  xs <- st_as_stars(x)
  names(st_dimensions(xs)) <- c("x", "y", "band")
  sa <- sdm_area(xs, cell_size, epsg)
  return(sa)
}

#' @export
sdm_area.character <- function(x, cell_size = NULL, epsg = NULL) {
  xs <- tryCatch(st_read(x), error = function(e) NA)
  if (length(xs) == 1) {
    if (is.na(xs)) {
      if (file_test("-d", x)) {
        x <- list.files(x, full.names = T)
      }
      xs <- tryCatch(read_stars(x, along = "band", normalize_path = FALSE), error = function(e) NA)
    }
  }
  if (!class(xs)[1] %in% c("stars", "sf")) {
    if (length(xs) & is.na(xs)) {
      cli_abort(c("Could not find files."))
    }
  }
  sa <- sdm_area(xs, cell_size, epsg)
  return(sa)
}

#' @export
sdm_area.stars <- function(x, cell_size = NULL, epsg = NULL) {
  if (length(names(x)) > 1) {
    cli_abort(c(
      "x has more than 1 attribute:",
      "i" = "Try to change attributes to bands using ?merge()",
    ))
  }
  if (is.null(cell_size)) {
    cell_size <- c(diff(st_bbox(x)[c(1, 3)]), diff(st_bbox(x)[c(2, 4)])) / c(10, 10)
  }
  if (is.null(epsg)) {
    epsg <- st_crs(x)
  } else if (st_crs(epsg) != st_crs(x)) {
    x <- st_transform(x, st_crs(epsg))
  }
  grd <- x %>%
    st_make_grid(cellsize = cell_size) %>%
    st_as_sf() %>%
    cbind(., cell_id = seq(1, nrow(.))) %>%
    st_join(st_as_sf(x), left = FALSE)
  st_geometry(grd) <- "geometry"
  epsg <- st_crs(grd)[1]$input
  bbox <- st_bbox(grd)
  var_names <- grd %>%
    as_tibble() %>%
    select(-c("geometry", "cell_id")) %>%
    colnames()
  l <- list(
    grid = grd,
    bbox = bbox,
    cell_size = cell_size,
    epsg = epsg,
    predictors = var_names
  )
  sa <- .sdm_area(l)
  return(sa)
}

#' @export
sdm_area.sf <- function(x, cell_size = NULL, epsg = NULL, lines_as_area = FALSE) {
  if (is.na(st_crs(x))) {
    cli_abort("Set a crs for x.")
  }
  if (is.null(cell_size)) {
    cell_size <- c(diff(st_bbox(x)[c(1, 3)]), diff(st_bbox(x)[c(2, 4)])) / c(10, 10)
  }
  if (is.null(epsg)) {
    epsg <- st_crs(x)
  } else if (st_crs(epsg) != st_crs(x)) {
    x <- st_transform(x, st_crs(epsg))
  }
  grd <- x |>
    st_make_grid(cellsize = cell_size) |>
    st_as_sf() |>
    mutate(cell_id = row_number()) |>
    st_join(st_make_valid(x), left = FALSE) |>
    rename(geometry = x) |>
    group_by(cell_id) |>
    summarise_all(mean)

  if (all(st_is(x, "LINESTRING")) & lines_as_area) {
    grd <- st_intersection(x, grd)
  }
  bbox <- st_bbox(grd)
  epsg <- st_crs(grd)[1]$input
  var_names <- grd %>%
    as_tibble() %>%
    select(-c("geometry", "cell_id")) %>%
    colnames()
  l <- list(
    grid = grd,
    bbox = bbox,
    cell_size = cell_size,
    epsg = epsg,
    predictors = var_names
  )
  sa <- .sdm_area(l)
  return(sa)
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
