#' Scenarios Managing
#'
#' This function creates and manage \code{scenarios} objects.
#'
#' @usage
#' scenarios_sdm(x,
#'               study_area = NULL,
#'               vars_study_area = NULL,
#'               predictors_names = NULL,
#'               rescaling = NULL,
#'               scenarios_names = NULL)
#'
#' @param x A \code{character} vector with the path to stack files, a \code{stars}, a
#' \code{RasterStack}, a \code{SpatRaster} or a \code{list}.
#' @param study_area A \code{sf} object with the study area to mask scenarios. If \code{NULL}, the
#' whole area is used.
#' @param vars_study_area Logical. Should the function use the table within the \code{study_area} as
#' predictors? Standard is \code{NULL}.
#' @param scenarios_names A \code{character} vector with names to be addressed to each scenario.
#' @param predictors_names A \code{character} vector with names to be addressed to each predictor.
#' @param rescaling A list of parameters to pass on rescaling function (optional, see details).
#' @param i \code{input_sdm} object.
#'
#' @details
#' If using a \code{sdm_area} object as predictors, add scenarios through \code{add_scenarios}
#' function.
#'
#' Rescaling is particularly usefull for aquatic environments, once creates a grid around riverlines
#' allowing modelers to account for the surrounding environment. Rescaling needs a \code{list} with
#' \code{cell_size} and \code{epsg}.
#'
#' \code{scenarios_names} returns the scenarios names in \code{input_sdm} object.
#'
#' \code{get_scenarios_data} retrieve scenarios data as a \code{list} of \code{sf}s to each scenario.
#'
#' @returns A \code{scenarios} object.
#'
#' @seealso \code{\link{input_sdm} \link{sdm_area} \link{add_scenarios} \link{scen}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' i <- input_sdm(
#'      occurrences_sdm(occ_data),
#'      predictors_sdm(bioc, study_area = parana),
#'      scenarios_sdm(scen, study_area = parana)
#'      )
#' i
#'
#' @import stars
#' @import raster
#' @importFrom here here
#' @importFrom gtools mixedsort
#'
#' @export
scenarios_sdm <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL, rescaling = NULL, scenarios_names = NULL) {
  UseMethod("scenarios_sdm")
}

#' @export
scenarios_sdm.RasterStack <- function(x, study_area = NULL, predictors_names = NULL, rescaling = NULL,
                                  scenarios_names = NULL) {
  if (is.null(predictors_names)) {
    predictors_names <- names(x)
  }
  if (!is.null(study_area) & is.null(rescaling)) {
    x <- mask(crop(x, study_area), study_area)
  }
  xb <- x
  x <- st_as_stars(cbind(coordinates(x), as.data.frame(x)))
  st_crs(x) <- as.character(crs(xb))
  if (is.null(scenarios_names)) {
    if (length(unique(l)) == length(l)) {
      scenarios_names <- basename(l)
    } else {
      paste0("scenario_", 1:length(l))
    }
  }
  names(x) <- scenarios_names
  grd <- NULL
  if (!is.null(study_area) & !is.null(rescaling)) {
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if (is.null(rescaling$epsg)) {
      crs2 <- as.character(crs(xb))[1]
    } else {
      crs2 <- as.character(st_crs(rescaling$epsg))[1]
    }
    study_area <- st_transform(study_area, crs2)
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs = crs2)
    grd <- st_as_sf(data.frame(cell_id = seq(1, length(grd)), as.data.frame(grd)))
    grd <- grd[study_area, ] # save grd in output? makek write_functions to save things.
    # rescaling
    if (is.na(st_crs(grd))) {
      st_crs(grd) <- st_crs(x)
    }
    x <- aggregate(x, grd, FUN = function(y) {
      mean(na.omit(y))
    })
    # x %>% split("band") %>% as.data.frame() %>% head()
    resolution <- c(x = cellsize, y = cellsize)
    coords <- as.data.frame(st_coordinates(st_centroid(grd)))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id = 1:ncell(x), df))[, "cell_id"]
  }
  bbox <- st_bbox(x)[c(1, 3, 2, 4)]
  epsg <- as.character(st_crs(x))[1]

  x <- list(
    coords = coords,
    bbox = bbox,
    resolution = resolution,
    epsg = epsg,
    cell_id = cell_id,
    paths = NULL,
    data = x
  )

  if (!is.null(grd)) {
    x$grid <- grd
  }
  if (!is.null(rescaling)) {
    x$rescaling <- rescaling
  }
  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios_sdm.data.frame <- function(x, study_area = NULL, predictors_names = NULL, rescaling = NULL,
                                 scenarios_names = NULL, epsg = NULL) { # pode entrar tanto uma tabela com coord e spp quanto sem.
  x <- st_as_stars(x)
  if (is.null(scenarios_names)) {
    if (length(unique(x)) == length(x)) {
      scenarios_names <- basename(x)
    } else {
      paste0("scenario_", 1:length(x))
    }
  }
  names(x) <- scenarios_names
  grd <- NULL
  if (!is.null(study_area) & !is.null(rescaling)) {
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if (is.null(rescaling$epsg)) {
      crs2 <- as.character(st_crs(epsg))[1]
    } else {
      crs2 <- as.character(st_crs(rescaling$epsg))[1]
    }
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs = crs2)
    grd <- st_as_sf(data.frame(cell_id = seq(1, length(grd)), as.data.frame(grd)))
    grd <- grd[study_area, ] # save grd in output?
    # rescaling
    if (is.na(st_crs(grd))) {
      st_crs(grd) <- st_crs(x)
    }
    x <- aggregate(x, grd, FUN = function(y) {
      mean(na.omit(y))
    })
    # x %>% split("band") %>% as.data.frame() %>% head()
    resolution <- c(x = cellsize, y = cellsize)
    coords <- as.data.frame(st_coordinates(st_centroid(grd)))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id = 1:ncell(x), df))[, "cell_id"]
  }
  bbox <- st_bbox(x)[c(1, 3, 2, 4)] # st_bbox
  epsg <- as.character(st_crs(x))[1]

  if (is.null(grd)) {
    x <- list(
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      paths = NULL,
      data = x
    )
  } else {
    x <- list(
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      data = x,
      paths = NULL,
      grid = grd
    )
  }

  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios_sdm.SpatRaster <- function(x, study_area = NULL, predictors_names = NULL, rescaling = NULL) {
  x <- st_as_stars(x)
  names(st_dimensions(x)) <- c("x", "y", "band")
  occ <- scenarios_sdm(x, study_area, predictors_names, rescaling)
  return(occ)
}

#' @export
scenarios_sdm.stars <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL,
                            rescaling = NULL, scenarios_names = NULL) {
  if (is.null(predictors_names)) {
    predictors_names <- names(x)
  }
  if (!is.null(study_area) & is.null(rescaling)) {
    # if(!all(st_is_valid(study_area))){study_area <- st_make_valid(study_area)}
    # suppressWarnings(x <- st_crop(x,st_as_sf(st_union(study_area))))
    # resolution <- st_res(x)
    # coords <- st_coordinates(x)[,c('x','y')]
    # df <- as.data.frame(split(x,'band'))
    # cell_id <- na.omit(data.frame(cell_id=1:nrow(df), df))[,'cell_id']
    # grd <- st_make_grid(x, n=c(ncol(x),nrow(x)))
    if (!all(st_is_valid(study_area))) {
      study_area <- st_make_valid(study_area)
    }
    if (!is.null(vars_study_area)) {
      x <- x[study_area]
      ext_x <- starsExtra::extract2(x, study_area, fun = mean, na.rm = T)
      x <- cbind(ext_x, study_area)
    } else {
      n <- names(x)
      suppressWarnings(x <- x[study_area])
      resolution <- st_res(x)
      x <- st_xy2sfc(x, as_points = FALSE)
      names(x) <- n
    }
    grd <- st_as_sf(x, as_points = TRUE)
    grd <- select(cbind(cell_id = seq(1, nrow(grd)), grd), c("cell_id", "geometry"))
    suppressWarnings(coords <- as.data.frame(st_coordinates(st_centroid(grd))))
    cell_id <- grd$cell_id
  } else if (!is.null(study_area) & !is.null(rescaling)) {
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if (is.null(rescaling$epsg)) {
      crs2 <- as.character(st_crs(x))[1]
    } else {
      crs2 <- as.character(st_crs(rescaling$epsg))[1]
    }
    study_area <- st_transform(study_area, crs2)
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs = crs2)
    grd <- st_as_sf(data.frame(cell_id = seq(1, length(grd)), as.data.frame(grd)))
    grd <- grd[study_area, ] # save grd in output? makek write_functions to save things.
    # rescaling
    if (is.na(st_crs(grd))) {
      st_crs(grd) <- st_crs(x)
    }
    if (!st_crs(grd) == st_crs(x)) {
      x <- st_transform(x, crs = crs2)
    }
    x <- aggregate(x, grd, FUN = function(y) {
      mean(na.omit(y))
    })
    resolution <- c(x = cellsize, y = cellsize)
    suppressWarnings(coords <- as.data.frame(st_coordinates(st_centroid(grd))))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id = 1:ncell(x), df))[, "cell_id"]
  }
  bbox <- st_bbox(x)[c(1, 3, 2, 4)]
  epsg <- as.character(st_crs(x))[1]

  if (is.null(grd)) {
    x <- list(
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      paths = NULL,
      data = x
    )
  } else {
    x <- list(
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      data = x,
      paths = NULL,
      grid = grd
    )
  }

  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios_sdm.character <- function(x, study_area = NULL, predictors_names = NULL, rescaling = NULL,
                                scenarios_names = NULL, ...) {
  l <- list.files(x, full.names = T, ...)
  # s <- lapply(l, function(x){s <- read_stars(x)})
  s <- read_stars(l)
  if (is.null(scenarios_names)) {
    if (length(unique(l)) == length(l)) {
      scenarios_names <- basename(l)
    } else {
      paste0("scenario_", 1:length(l))
    }
  }
  names(s) <- scenarios_names
  scen <- scenarios_sdm(x = s, study_area = study_area, predictors_names = predictors_names, rescaling = rescaling, scenarios_names = scenarios_names)
  return(scen)
}

#' @export
scenarios_sdm.list <- function(x, study_area = NULL, predictors_names = NULL, rescaling = NULL,
                           scenarios_names = NULL, ...) {
  s <- x
  s <- sapply(s, function(x) {
    scenarios_sdm(x, study_area = study_area, predictors_names = predictors_names, rescaling = rescaling, ...)
  }, simplify = F, USE.NAMES = TRUE)

  coords <- lapply(s, function(x) {
    x$coords
  })
  if (all(apply(combn(length(coords), 2), 2, function(x) {
    all.equal(coords[[x[1]]], coords[[x[2]]])
  }))) {
    coords <- coords[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  bbox <- lapply(s, function(x) {
    x$bbox
  })
  if (all(apply(combn(length(bbox), 2), 2, function(x) {
    all.equal(bbox[[x[1]]], bbox[[x[2]]])
  }))) {
    bbox <- bbox[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  resolution <- lapply(s, function(x) {
    x$resolution
  })
  if (all(apply(combn(length(resolution), 2), 2, function(x) {
    all.equal(resolution[[x[1]]], resolution[[x[2]]])
  }))) {
    resolution <- resolution[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  epsg <- lapply(s, function(x) {
    x$epsg
  })
  if (all(apply(combn(length(epsg), 2), 2, function(x) {
    all.equal(epsg[[x[1]]], epsg[[x[2]]])
  }))) {
    epsg <- epsg[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  cell_id <- lapply(s, function(x) {
    x$cell_id
  })
  if (all(apply(combn(length(cell_id), 2), 2, function(x) {
    all.equal(cell_id[[x[1]]], cell_id[[x[2]]])
  }))) {
    cell_id <- cell_id[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  paths <- lapply(s, function(x) {
    x$paths
  })
  if (all(apply(combn(length(paths), 2), 2, function(x) {
    all.equal(paths[[x[1]]], paths[[x[2]]])
  }))) {
    paths <- paths[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  grid <- lapply(s, function(x) {
    x$grid
  })
  if (all(apply(combn(length(grid), 2), 2, function(x) {
    all.equal(grid[[x[1]]], grid[[x[2]]])
  }))) {
    grid <- grid[[1]]
  } else {
    stop("Scenarios are different from each other.")
  }
  s_data <- sapply(s, function(x) {
    x$data
  }, simplify = F, USE.NAMES = TRUE)

  x <- list(
    coords = coords,
    bbox = bbox,
    resolution = resolution,
    epsg = epsg,
    cell_id = cell_id,
    paths = paths,
    grid = grid,
    data = s_data
  )
  scen <- .scenarios(x)
  return(scen)
}


#' @rdname scenarios_sdm
#' @export
scenarios_names <- function(i) {
  x=i
  if (class(x) == "input_sdm") {
    y <- x$scenarios
  } else {
    y <- x
  }
  res <- names(y$data)
  return(res)
}

#' @rdname scenarios_sdm
#' @export
get_scenarios_data <- function(i) {
  x=i
  if (class(x) == "input_sdm") {
    y <- x$scenarios
  } else {
    y <- x
  }
  return(y$data)
}

#' @export
.scenarios <- function(x) {
  scen <- structure(
    list(
      coords = x$coords,
      bbox = x$bbox,
      resolution = x$resolution,
      epsg = x$epsg,
      cell_id = x$cell_id,
      paths = x$paths,
      grid = x$grid,
      data = x$data
    ),
    class = "scenarios"
  )
  return(scen)
}

#' Print method for scenarios
#' @exportS3Method base::print
print.scenarios <- function(x) {
  cat("          caretSDM         \n")
  cat("...........................\n")
  cat("Class                     : Scenarios\n")
  cat("Scenarios Names           :", names(x$data), "\n")
  cat("Number of Scenarios       :", length(x$data), "\n")
  if (!is.null(x$bbox)) {
    cat("Extent                    :", x$bbox, "(xmin, xmax, ymin, ymax)\n")
  }
  if (!is.null(x$epsg)) {
    cat("EPSG                      :", x$epsg, "\n")
  }
  if (!is.null(x$resolution)) {
    cat("Resolution                :", x$resolution, "(x, y)\n")
  }
  if (!is.null(x$variable_selection$vif)) {
    cat(cat("Selected Variables (VIF)  :"), cat(x$variable_selection$vif$selected_variables, sep = ", "), "\n")
  }
}
