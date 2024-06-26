#' Predictors Managing
#'
#' This function creates and manage \code{predictors} objects.
#'
#' @usage
#' predictors_sdm(x,
#'                study_area = NULL,
#'                vars_study_area = NULL,
#'                predictors_names = NULL,
#'                rescaling = NULL)
#'
#' @param x A string with the path to predictors files, a \code{stars}, a \code{RasterStack} or
#' \code{SpatRaster}.
#' @param study_area A string with the path to study area shapefile (.shp or .gpkg) or a \code{sf}
#' polygon. If \code{study_area} is not \code{NULL}, the function masks predictors values to given
#' study area.
#' @param predictors_names A vector with names to be addressed to each predictor.
#' @param rescaling A list of parameters to pass on rescaling function (optional, see details).
#' @param i \code{input_sdm} object.
#'
#' @details
#' If using \code{sdm_area} object in the workflow, add predictors through \code{add_predictors}
#' function.
#'
#' Rescaling is particularly usefull for aquatic environments, once creates a grid around riverlines
#' allowing modelers to account for the surrounding environment. Rescaling needs a \code{list} with
#' \code{cell_size} and \code{epsg}.
#'
#' \code{predictors_names} returns the predictors names in \code{input_sdm} object.
#'
#' \code{set_predictors_names} change the predictors names in \code{input_sdm} object. Useful to
#' make sure the predictors names are equal the names in scenarios.
#'
#' \code{get_predictors} retrieve predictors data in \code{sf} format.
#'
#' @return A \code{predictors} object.
#'
#' @seealso \code{\link{WorldClim_data} \link{input_sdm} \link{sdm_area} \link{add_predictors}
#'  \link{bioc}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' i <- input_sdm(
#'      occurrences_sdm(occ_data),
#'      predictors_sdm(bioc, study_area = parana)
#'      )
#' i
#'
#' @import raster
#' @import sf
#' @import stars
#' @import dplyr
#' @importFrom gtools mixedsort
#'
#' @export
predictors_sdm <- function(x, ...) {
  UseMethod("predictors_sdm")
}

#' @export
predictors_sdm.RasterStack <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL, rescaling = NULL) {
  if (is.null(predictors_names)) {
    predictors_names <- names(x)
  }
  if (!is.null(study_area) & is.null(rescaling)) {
    x <- mask(crop(x, study_area), study_area)
  }
  xb <- x
  x <- st_as_stars(cbind(coordinates(x), as.data.frame(x)), along = "band")
  st_crs(x) <- as.character(crs(xb))
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
    grd <- grd[study_area, ] # save grd in output?
    # rescaling
    if (is.na(st_crs(grd))) {
      st_crs(grd) <- st_crs(x)
    }
    x <- st_transform(x, crs = crs2)
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
  bbox <- st_bbox(x)[c(1, 3, 2, 4)] # st_bbox
  epsg <- as.character(st_crs(x))[1]

  if (is.null(grd)) {
    x <- list(
      predictors_names = predictors_names,
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      data = x
    )
  } else {
    x <- list(
      predictors_names = predictors_names,
      coords = coords,
      bbox = bbox,
      resolution = resolution,
      epsg = epsg,
      cell_id = cell_id,
      data = x,
      grid = grd
    )
  }
  occ <- .predictors(x)
  return(occ)
}

#' @export
predictors_sdm.SpatRaster <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL, rescaling = NULL) {
  x <- st_as_stars(x)
  names(x) <- "current"
  names(st_dimensions(x)) <- c("x", "y", "band")
  occ <- predictors_sdm(x, study_area, vars_study_area, predictors_names, rescaling)
  return(occ)
}

#' @export
predictors_sdm.character <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL, rescaling = NULL) {
  l <- list.files(x, full.names = T)
  x <- read_stars(l, along = "band", normalize_path = FALSE)
  names(x) <- "current"
  x_dims <- st_dimensions(x)
  x_dims$band$values <- sort(paste0("bio", 1:19))
  st_dimensions(x) <- x_dims
  p <- predictors_sdm(x, study_area, vars_study_area, predictors_names, rescaling)
  return(p)
}

#' @export
predictors_sdm.stars <- function(x, study_area = NULL, vars_study_area = NULL, predictors_names = NULL, rescaling = NULL) {
  if (is.null(predictors_names)) {
    predictors_names <- st_dimensions(x)$band$values
  }
  if (!is.null(study_area) & is.null(rescaling)) {
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
    # teste <- as.data.frame(x)
    # vals <- teste[teste$band==predictors_names[1],4]
    # grd$vals <- vals
    # grd <- select(na.omit(grd),-vals)
    suppressWarnings(coords <- as.data.frame(st_coordinates(st_centroid(grd))))
    # df <- as.data.frame(st_as_sf(x))
    # cell_id <- na.omit(data.frame(cell_id=1:nrow(df), df))[,'cell_id']
    cell_id <- grd$cell_id
    ##### TAMANHO DA GRD É MAIOR QUE O DE X
  } else if (!is.null(study_area) & !is.null(rescaling)) {
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if (is.null(rescaling$epsg)) {
      crs2 <- as.character(crs(x))[1]
    } else {
      crs2 <- as.character(st_crs(rescaling$epsg))[1]
    }
    study_area <- st_transform(study_area, crs2)
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs = crs2)
    grd <- st_as_sf(data.frame(cell_id = seq(1, length(grd)), as.data.frame(grd)))
    grd <- grd[study_area, ] # save grd in output?
    # rescaling
    if (is.na(st_crs(grd))) {
      st_crs(grd) <- st_crs(x)
    }
    x <- st_transform(x, crs = crs2)
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
    grd <- st_make_grid(x, n = c(ncol(x), nrow(x)))
  }
  bbox <- st_bbox(x)[c(1, 3, 2, 4)] # st_bbox
  epsg <- as.character(st_crs(x))[1]

  x <- list(
    predictors_names = predictors_names,
    coords = coords,
    bbox = bbox,
    resolution = resolution,
    epsg = epsg,
    cell_id = cell_id,
    data = x,
    grid = grd
  )

  occ <- .predictors(x)
  return(occ)
}

#' @export
predictors_sdm.sdm_area <- function(sdm_area) { # pode entrar tanto uma tabela com coord e spp quanto sem.
  facnum <- function(x) {
    return(as.numeric(as.factor(x)))
  }

  x <- sdm_area$grid |>
    mutate_if(is.character, facnum) |>
    select(-cell_id) |>
    st_rasterize() |>
    st_xy2sfc(as_points = FALSE) |>
    merge(name = "band")

  coords <- sdm_area$grid |>
    st_coordinates() |>
    as.data.frame() |>
    select(X, Y)

  x <- list(
    predictors_names = predictors_names(sdm_area),
    coords = coords,
    bbox = sdm_area$bbox,
    resolution = sdm_area$cell_size,
    epsg = sdm_area$epsg,
    cell_id = sdm_area$grid$id,
    data = x,
    grid = sdm_area$grid
  )

  occ <- .predictors(x)
  return(occ)
}

#' @export
predictors_sdm.data.frame <- function(x, study_area, predictors_names = NULL, rescaling = NULL, epsg = NA) { # pode entrar tanto uma tabela com coord e spp quanto sem.
  x <- st_as_stars(x)
  if (is.null(epsg)) {
    st_crs(x) <- st_crs(4326)
  } else {
    st_crs(x) <- st_crs(epsg)
  }
  grd <- NULL
  if (!is.null(study_area) & !is.null(rescaling)) {
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if (is.null(rescaling$epsg)) {
      crs2 <- as.character(st_crs(epsg))[1]
    } else {
      crs2 <- as.character(st_crs(rescaling$epsg))[1]
    }
    study_area <- st_transform(study_area, crs2)
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
    resolution <- c(x = cellsize, y = cellsize)
    suppressWarnings(coords <- as.data.frame(st_coordinates(st_centroid(grd))))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id = 1:ncell(x), df))[, "cell_id"]
  }
  bbox <- st_bbox(x)[c(1, 3, 2, 4)] # st_bbox
  epsg <- as.character(st_crs(x))[1]

  x <- list(
    predictors_names = predictors_names,
    coords = coords,
    bbox = bbox,
    resolution = resolution,
    epsg = epsg,
    cell_id = cell_id,
    data = x
  )

  if (!is.null(grd)) {
    x$grid <- grd
  }
  if (!is.null(rescaling)) {
    x$rescaling <- rescaling
  }
  occ <- .predictors(x)
  return(occ)
}

#' @export
.predictors <- function(x) {
  occ <- structure(
    list(
      predictors_names = x$predictors_names,
      coords = x$coords,
      bbox = x$bbox,
      resolution = x$resolution,
      epsg = x$epsg,
      grid = x$grid,
      rescaling = x$rescaling,
      data = x$data
    ),
    class = "predictors"
  )
  return(occ)
}

#' @rdname predictors_sdm
#' @export
predictors_names <- function(i) {
  x=i
  if (is_sdm_area(x) ) {
    return(x$predictors)
  } else if (is_input_sdm(x)) {
    y <- x$predictors
  } else if (class(x) == 'predictors') {
    y <- x
  }
  if("variable_selection" %in% names(y)){
    if ("vif" %in% names(y$variable_selection)) {
      res <- y$variable_selection$vif$selected_variables
    }
  } else {
    res <- y$predictors_names
  }
  return(res)
}

#' @rdname predictors_sdm
#' @export
get_predictors <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$predictors
    if (is_sdm_area(y)){
      res <- y$grid
    } else {
      res <- cbind(y$grid, y$data$current)
      names(res) <- c("cell_id", y$predictors_names, "geometry")
    }
  } else if (is_predictors(x)){
    y <- x
    res <- cbind(y$grid, y$data$current)
    names(res) <- c("cell_id", y$predictors_names, "geometry")
  } else if (is_sdm_area(x)){
    res <- x$grid
  }
  return(res)
}

#' @rdname predictors_sdm
#' @export
set_predictors_names <- function(i, new_names) {
  x=i
  if(is_sdm_area(x)){
    grd <- get_predictors(x)
    grd <- grd[,c("cell_id",names(grd)[!names(grd)=='cell_id'])]
    colnames(grd) <- c("cell_id", new_names, "geometry")
    x$predictors$predictors <- new_names
    x$predictors$grid <- grd
    return(x)
  }
}

#' Print method for predictors
#' @exportS3Method base::print
print.predictors <- function(x) {
  cat("          caretSDM         \n")
  cat("...........................\n")
  cat("Class                     : Predictors\n")
  cat("Number of Predictors      :", length(x$predictors_names), "\n")
  cat(cat("Predictors Names          : "), cat(x$predictors_names, sep = ", "), "\n")
  if (!is.null(x$bbox)) {
    cat("Extent                    :", x$bbox, "(xmin, xmax, ymin, ymax)\n")
  }
  if (!is.null(x$epsg)) {
    cat("EPSG                      :", x$epsg, "\n")
  }
  if (!is.null(x$resolution)) {
    cat("Resolution                :", x$resolution, "(x, y)\n")
  }
  if (!is.null(x$rescaling)) {
    cat(
      "Rescaling                 :",
      "        Cellsize          :", x$rescaling$cellsize, "\n"
    )
  }
  if (!is.null(x$variable_selection$vif)) {
    cat(cat("Selected Variables (VIF)  : "), cat(x$variable_selection$vif$selected_variables, sep = ", "), "\n")
  }
}
