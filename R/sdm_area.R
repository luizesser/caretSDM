#' Create a \code{sdm_area} object
#'
#' This function creates a new \code{sdm_area} object.
#'
#' @usage sdm_area(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
#'                 gdal = TRUE, crop_by = NULL, lines_as_sdm_area = FALSE)
#'
#' @param x A shape or a raster. Usually a shape from \code{sf} class, but rasters from
#' \code{stars}, \code{rasterStack} or \code{SpatRaster} class are also allowed.
#' @param cell_size \code{numeric}. The cell size to be used in models.
#' @param crs \code{numeric}. Indicates which EPSG should the output grid be in. If \code{NULL},
#' epsg from \code{x} is used.
#' @param variables_selected A \code{character} vector with variables in \code{x} to be used in models.
#' If \code{NULL} (standard), all variables in \code{x} are used.
#' @param gdal Boolean. Force the use or not of GDAL when available. See details.
#' @param crop_by A shape from \code{sf} to crop \code{x}.
#' @param lines_as_sdm_area Boolean. If \code{x} is a \code{sf} with LINESTRING geometry, it can be used
#' to model species distribution in lines and not grid cells.
#' @param i A \code{sdm_area} or a \code{input_sdm} object.
#'
#' @details
#' The function returns a \code{sdm_area} object with a grid built upon the \code{x} parameter.
#' There are two ways to make the grid and resample the variables in \code{sdm_area}: with and
#' without gdal. As standard, if gdal is available in you machine it will be used (\code{gdal = TRUE}),
#' otherwise sf/stars will be used.
#' \code{get_sdm_area} will return the grid built by \code{sdm_area}.
#'
#' @returns A \code{sdm_area} object containing:
#'    \item{grid}{\code{sf} with \code{POLYGON} geometry representing the grid for the study area.}
#'    \item{cell_size}{\code{numeric} information regarding the size of the cell used to rescale
#'    variables to the study area, representing also the cell size in the \code{grid}.}
#'
#' @seealso \code{\link{WorldClim_data} \link{parana} \link{input_sdm}, \link{add_predictors}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com) and Reginaldo Ré.
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa_area <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Create sdm_area using lines:
#' sa_rivers <- sdm_area(rivs, cell_size = 25000, crs = 6933, lines_as_sdm_area = TRUE)
#'
#' @importFrom stars st_as_stars read_stars write_stars st_dimensions
#' @importFrom sf st_crs st_read st_bbox st_as_sf gdal_utils st_crop st_make_valid st_transform
#' st_write st_intersects st_length st_intersection st_join st_area sf_extSoftVersion
#' st_geometry_type st_as_sfc st_centroid st_geometry st_drop_geometry st_cast
#' st_collection_extract st_nearest_feature
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom dplyr setdiff select all_of any_of join_by relocate mutate arrange desc filter
#' group_by reframe across inner_join join_by distinct n rename row_number where
#' @importFrom checkmate test_class makeAssertCollection test_list
#' @importFrom tidyr drop_na
#' @importFrom fs path dir_exists dir_delete dir_create dir_ls path_file path_ext_remove
#' @importFrom purrr map map_chr compact list_rbind detect_index
#' @importFrom future plan multisession sequential
#' @importFrom parallelly availableCores
#' @importFrom progressr handlers progressor
#' @importFrom glue glue
#' @importFrom furrr future_map2
#' @importFrom stringr str_replace_all str_replace
#' @importFrom lwgeom st_split
#' @importFrom stats setNames
#' @importFrom methods as
#' @importFrom utils file_test
#'
#' @global original_id ..weighting_factor
#'
#' @export
sdm_area <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                     gdal = TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  assert_cli(
    check_int_cli(
      crs,
      na.ok = TRUE,
      null.ok = TRUE
    ),
    check_class_cli(
      crs,
      "crs",
      null.ok = TRUE
    )
  )

  assert_number_cli(
    cell_size,
    na.ok = FALSE,
    null.ok = TRUE,
    lower = 0
  )

  assert_cli(
    check_list_cli(
      variables_selected,
      unique = TRUE,
      null.ok = TRUE,
      any.missing = FALSE
    ),
    check_character_cli(
      variables_selected,
      unique = TRUE,
      null.ok = TRUE,
      min.chars = 1,
      any.missing = FALSE,
      all.missing = FALSE,
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

  if (!is.null(crop_by)) {
    assert_class_cli(
      try(sf::st_bbox(crop_by), silent = TRUE),
      classes = "bbox",
      null.ok = FALSE,
      .var.name = "crop_by"
    )
    if (is.null(crs)){
      if (sf::st_crs(x) != sf::st_crs(crop_by)){
        cli::cli_abort(
          c(
            "x" = "The CRS of crop_by must be equal to CRS of x."
          ))
      }
    } else {
      if (sf::st_crs(crs) != sf::st_crs(crop_by)){
        cli::cli_abort(
          c(
            "x" = "The CRS of crop_by must be equal to CRS of parameter crs."
          ))
      }
    }
  }

  UseMethod("sdm_area")
}

#' @export
sdm_area.RasterStack <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                                 gdal= TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  xs <- stars::st_as_stars(x)
  sa <- sdm_area(xs, cell_size, crs, variables_selected, gdal, crop_by)
  return(invisible(sa))
}

#' @export
sdm_area.SpatRaster <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                                gdal= TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  xs <- stars::st_as_stars(x)
  names(stars::st_dimensions(xs)) <- c("x", "y", "band")
  sa <- sdm_area(xs, cell_size, crs, variables_selected, gdal, crop_by)
  return(invisible(sa))
}

#' @export
sdm_area.character <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                               gdal= TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  xs <- tryCatch(
    sf::st_read(x, quiet = TRUE),
    error = function(e) NA
  )
  if (length(xs) == 1) {
    if (is.na(xs)) {
      if (utils::file_test("-d", x)) {
        x <- x |> fs::dir_ls(type = "file")
      }
      xs <- tryCatch(
        stars::read_stars(
          x,
          normalize_path = FALSE,
          quiet = TRUE
        ),
        error = function(e) {
          cli::cli_abort(c("x" = e$message))
        }
      )
    }
  }
  if (!class(xs)[1] %in% c("stars", "sf")) {
    if (length(xs) && is.na(xs)) {
      cli::cli_abort(c("x" = "Files not found."))
    }
  }
  sa <- sdm_area(xs, cell_size, crs, variables_selected, gdal, crop_by, lines_as_sdm_area)
  return(invisible(sa))
}

#' @export
sdm_area.stars <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                           gdal= TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  assert_number_cli(
    cell_size,
    na.ok = FALSE,
    null.ok = FALSE,
    lower = 0
  )

  x_var <- x |>
    .try_split() |>
    .adjust_cell_id_name(variables_selected)
  x <- x_var$x
  variables_selected <- x_var$variables_selected

  x <- x |>
    .select_variables(variables_selected)
  var_names_final <- c(
    "cell_id",
    x |> names() |> dplyr::setdiff("geometry"),
    "geometry"
  )

  assert_number_cli(
    cell_size,
    na.ok = FALSE,
    null.ok = FALSE,
    lower = 0
  )
  assert_class_cli(
    sf::st_crs(x),
    classes = "crs",
    null.ok = FALSE,
    .var.name = "x"
  )
  if (is.null(crs)) {
    crs <- sf::st_crs(x)
  } else {
    crs <- tryCatch(
      suppressWarnings(sf::st_crs(crs)),
      error = function(e) NA
    )
  }
  if (is.na(crs) || is.na(crs$input)){
    cli_abort(c("x" = "crs is invalid."))
  }

  if (.is_gdal_installed() && gdal){
    cli::cli_inform(c(
      "!" = "Making grid over the study area is an expensive task. Please, be patient!",
      "i" = "Using GDAL to make the grid and resample the variables."
    ))
    l <- list(
      grid = .sdm_area_from_stars_using_gdal(x, cell_size, crs, crop_by) |>
        dplyr::select(dplyr::all_of(var_names_final)),
      cell_size = cell_size
    )

  } else {
    info_msg <- c("!" = "Making grid over study area is an expensive task. Please, be patient!")
    if (gdal) info_msg <- append(info_msg, c("i" = "GDAl not installed. Please, read the
                                             instructions {.href [here](https://gdal.org/download.html)}."))
    info_msg <- c(info_msg, c("i"="Using sf/Stars to make the grid and resample the variables."))
    cli::cli_inform(info_msg)
    l <- list(
      grid = .sdm_area_from_stars_using_stars(x, cell_size, crs, crop_by) |>
        dplyr::select(dplyr::all_of(var_names_final)),
      cell_size = cell_size
    )
  }
  if ((l$grid |> nrow()) > 0) {
    sa <- .sdm_area(l)
  } else {
    cli::cli_warn(c(
      "!" = "crop_by not applied.",
      "i" = "The area of crop_by does not intersects with area of x."
    ))
    return(NULL)
  }
  return(invisible(sa))
}

.sdm_area_from_stars_using_gdal <- function(x, cell_size = NULL, crs = NULL, crop_by = NULL) {
  in_dir <- fs::path(tempdir(), Sys.time() |> as.integer() |> as.character(), "in_dir")
  if (fs::dir_exists(in_dir)) {
    fs::dir_delete(in_dir)
  }

  var_names <- ""
  file_list <- ""
  del_in_dir <- FALSE
  if (checkmate::test_class(x, "stars", ordered = TRUE)) {
    tryCatch(
      {
        fs::dir_create(in_dir)

        var_names <- names(x)
        if (length(var_names)==0) {
          x$cell_id <- 1
          var_names <- names(x)
        }

        var_names |>
          purrr::map(
            \(v_name) {
              x[v_name] |>
                stars::write_stars(
                  fs::path(in_dir, v_name, ext="tif"),
                  options="COMPRESS=LZW"
                )
            }
          )
        del_in_dir <- TRUE
      },
      error = function(e) {
        cli::cli_abort(c(
          "x" = "It was not possible to write raster on disk.",
          "i" = e$message
        ))
      }
    )
    file_list <- in_dir |>
      fs::dir_ls(type = "file")
  } else {
    var_names <- names(x)
    file_list <- x |>
      purrr::map_chr(\(x) x) |> unname()
  }

  out_dir <- fs::path(tempdir(), Sys.time() |> as.integer() |> as.character(), "out_dir")
  if (fs::dir_exists(out_dir)) {
    fs::dir_delete(out_dir)
  }
  tryCatch(
    {
      fs::dir_create(out_dir)
      warp_method <- x |> purrr::map(
        \(e) {
          if (.is_float(e)) {
            return("average")
          } else {
            return("med")
          }
        }
      )

      file_list |> purrr::map(
        \(file_name){
          in_file <- file_name
          out_file <- fs::path(out_dir, fs::path_file(file_name))
          options <- c(
            "-overwrite",
            "-of", "GTiff",
            "-t_srs", crs$wkt, # output file SRS
            "-r", warp_method[[file_name |> fs::path_file() |> fs::path_ext_remove()]],
            "-co", "BIGTIFF=YES",
            "-co", "COMPRESS=LZW",
            "-dstnodata", "-999999",
            "-tr", cell_size, cell_size,
            "-ot", "Float32"
          )
          if (!is.null(crop_by)){
            options <- c(
              options,
              "-te", c(sf::st_bbox(crop_by) |> methods::as("vector"))
            )
          }
          sf::gdal_utils(
            util = "warp",
            source = in_file,
            destination = out_file,
            options = options
          )
       }
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "It was not possible to write raster on disk.",
        "i" = e$message
      ))
    }
  )

  if (del_in_dir && fs::dir_exists(in_dir)){
    fs::dir_delete(in_dir)
  }

  grd <- out_dir |>
    list.files(full.names = T) |>
    stars::read_stars(normalize_path = F) |>
    stats::setNames(var_names)

  n_col <- attr(grd, "dimensions")[["x"]]$to
  n_row <- attr(grd, "dimensions")[["y"]]$to
  grd$cell_id <- seq(1, n_col * n_row)

  grd <- grd |>
    sf::st_as_sf()

  if(is.null(crop_by)){
    grd_bbox <- sf::st_bbox(grd)
  }

  grd <- grd |>
    tidyr::drop_na() |>
    dplyr::relocate(cell_id)

  if (!is.null(crop_by)){
    attr(sf::st_geometry(grd), "bbox") <- sf::st_bbox(crop_by)
  } else {
    attr(sf::st_geometry(grd), "bbox") <- grd_bbox
  }

  return(grd)
}

.sdm_area_from_stars_using_stars <- function(x, cell_size = NULL, crs = NULL, crop_by = NULL) {
  if (sf::st_crs(crs) != sf::st_crs(x)) {
    x <- sf::st_transform(x, sf::st_crs(crs))
  }

  x <- x |>
    sf::st_as_sf() |>
    sf::st_make_valid()

  if (!is.null(crop_by)){
    suppressWarnings(
      tmp_x <- x |>
        sf::st_crop(crop_by |> sf::st_bbox())
    )
    if (nrow(tmp_x) > 0) {
      x <- tmp_x
    } else {
      cli::cli_warn(c(
        "!" = "crop_by not applied.",
        "i" = "The area of crop_by does not intersects with area of x."
      ))
    }
  }

  grd <- x |>
    .adjust_bbox(cell_size) |>
    .sdm_area_from_sf_using_stars(cell_size, crs)

  return(grd)
}

#' @export
sdm_area.sf <- function(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                        gdal= TRUE, crop_by = NULL, lines_as_sdm_area = FALSE) {
  assert_class_cli(
    sf::st_crs(x),
    classes = "crs",
    null.ok = FALSE,
    .var.name = "x"
  )

  assert_number_cli(
    cell_size,
    na.ok = FALSE,
    null.ok = FALSE,
    lower = 0
  )

  if (is.null(crs)) {
    crs <- sf::st_crs(x)
  } else {
    crs <- tryCatch(
      suppressWarnings(sf::st_crs(crs)),
      error = function(e) NA
    )
  }
  if (is.na(crs) || is.na(crs$input)){
    cli_abort(c("x" = "crs is invalid."))
  }

  sa <- .detect_sdm_area(x, cell_size, crs)
  if (checkmate::test_class(sa, "sdm_area")){
    return(invisible(sa))
  }

  if (sf::st_crs(crs) != sf::st_crs(x)) {
    x <- sf::st_transform(x, sf::st_crs(crs))
  }
  x <- x |>
    sf::st_make_valid()

  if (!is.null(crop_by)) {
    suppressWarnings(
      tmp_x <- x |>
        sf::st_crop(crop_by)
    )
    if (nrow(tmp_x) > 0) {
      x <- tmp_x
    } else {
      cli::cli_warn(c(
        "!" = "crop_by not applied.",
        "i" = "The area of crop_by does not intersects with area of x."
      ))
    }
  }

  x_var <- x |>
    .adjust_bbox(cell_size) |>
    .adjust_cell_id_name(variables_selected)
  x <- x_var$x
  variables_selected <- x_var$variables_selected

  x <- x |>
    .adjust_geom_col() |>
    .select_variables(variables_selected)
  var_names_final <- c(
    "cell_id",
    x |> names() |> dplyr::setdiff("geometry"),
    "geometry"
  )

  if (.is_gdal_installed() && gdal){
    cli::cli_inform(c(
      "!" = "Making grid over study area is an expensive task. Please, be patient!",
      "i" = "Using GDAL to make the grid and resample the variables."
    ))
    l <- list(
      grid = .sdm_area_from_sf_using_gdal(x, cell_size, crs) |>
        dplyr::select(dplyr::all_of(var_names_final)),
      cell_size = cell_size
    )
  } else {
    info_msg <- c("!" = "Making grid over the study area is an expensive task. Please, be patient!")
    if (gdal) info_msg <- append(info_msg, c("i" = "GDAl not installed. Please, read the
                                             instructions {.href [here](https://gdal.org/download.html)}."))
    info_msg <- c(info_msg, c("i"="Using sf/Stars to make the grid and resample the variables."))
    cli::cli_inform(info_msg)
    l <- list(
      grid = .sdm_area_from_sf_using_stars(x, cell_size, crs) |>
        dplyr::select(dplyr::all_of(var_names_final)),
      cell_size = cell_size
    )
  }

  if (lines_as_sdm_area) {
    l$grid <- .attribute_bioclim_to_rivers(x, l$grid) |>
        dplyr::select(dplyr::all_of(var_names_final)) |>
        dplyr::mutate(cell_id = dplyr::row_number())
  }

  sa <- .sdm_area(l)
  return(invisible(sa))
}

.sdm_area_from_sf_using_gdal <- function(x, cell_size = NULL, crs = NULL) {
  in_dir <- fs::path(
    tempdir(),
    Sys.time() |> as.integer() |> as.character(),
    "in_dir"
  )
  if (fs::dir_exists(in_dir)) {
    fs::dir_delete(in_dir)
  }

  if (length(names(x))==1){
    x$cell_id <- 1
  }
  in_file <- fs::path(in_dir, "sdm_area_tmp", ext = "gpkg")
  tryCatch(
    {
      fs::dir_create(in_dir)
      x |>
        sf::st_write(dsn = in_file, delete_dsn = TRUE, quiet =TRUE)
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "It was not possible to write shp/gpkg on disk.",
        "i" = e$message
      ))
    }
  )

  out_dir <- fs::path(
    tempdir(),
    Sys.time() |> as.integer() |> as.character(),
    "out_dir"
  )
  if (fs::dir_exists(out_dir)) {
    fs::dir_delete(out_dir)
  }
  tryCatch(
    {
      fs::dir_create(out_dir)

      progressr::handlers("cli")
      p_bar_rasterize <- progressr::progressor(length(names(x)))
      names(x) |>
        dplyr::setdiff("geometry") |>
        purrr::map(
          \(var_name) {
            p_bar_rasterize(message = glue::glue("Rasterizing: { var_name }"))
            sf::gdal_utils(
              util = "rasterize",
              source = in_file,
              destination = fs::path(out_dir, var_name, ext = "tif"),
              options = c(
                #"-overwrite",
                "-of", "GTiff",
                "-at",
                "-a", var_name,
                #"-t_srs", crs$wkt, # output file SRS
                "-te", c(sf::st_bbox(x) |> methods::as("vector")),
                #"-r", "average",
                "-co", "BIGTIFF=YES",
                "-co", "COMPRESS=LZW",
                "-a_nodata", "-999999",
                "-tr", cell_size / 15, cell_size / 15,
                "-ot", "Float32"
              )
            )
          }
        )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "It was not possible to write raster on disk.",
        "i" = e$message
      ))
    }
  )

  out_dir_warp <- fs::path(tempdir(), Sys.time() |> as.integer() |> as.character(), "out_dir_warp")
  if (fs::dir_exists(out_dir_warp)) {
    fs::dir_delete(out_dir_warp)
  }
  tryCatch(
    {
      fs::dir_create(out_dir_warp)

      warp_method <- x |> purrr::map(
        \(e) {
          if (.is_float(e)) {
            return("average")
          } else {
            return("med")
          }
        }
      )

      progressr::handlers("cli")
      p_bar_warp <- progressr::progressor(length(names(x)))
      files_list <- out_dir |>
        fs::dir_ls(type = "file")

      files_list |>
        purrr::map(
          \(file_name){
            short_file_name <- file_name |>
              fs::path_file() |>
              fs::path_ext_remove()
            p_bar_warp(message = glue::glue("Resampling: { short_file_name }"))
            in_file <- file_name
            out_file <- fs::path(out_dir_warp, fs::path_file(file_name))
            sf::gdal_utils(
              util = "warp",
              source = in_file,
              destination = out_file,
              options = c(
                "-overwrite",
                "-of", "GTiff",
                "-r",  warp_method[[short_file_name]],
                "-co", "BIGTIFF=YES",
                "-co", "COMPRESS=LZW",
                "-dstnodata", "-999999",
                "-tr", cell_size, cell_size,
                "-ot", "Float32"
              )
            )
          }
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "It was not possible to write raster on disk.",
        "i" = e$message
      ))
    }
  )


  grd <- fs::dir_ls(out_dir_warp, type = "file") |>
    stars::read_stars(normalize_path = F) |>
    stats::setNames(
      fs::dir_ls(out_dir, type = "file") |>
        fs::path_file() |>
        fs::path_ext_remove()
    )

  n_col <- attr(grd, "dimensions")[["x"]]$to
  n_row <- attr(grd, "dimensions")[["y"]]$to
  grd$cell_id <- seq(1, n_col * n_row)


  grd <- grd |>
    sf::st_as_sf()

  grd_bbox <- sf::st_bbox(grd)

  if (length(grd |> names() |> setdiff(c("cell_id", "geometry"))) == 0) {
    grd <- grd[x, ]
  }

  grd <- grd |>
    tidyr::drop_na()

  attr(sf::st_geometry(grd), "bbox") <- grd_bbox

  return(grd)
}

.sdm_area_from_sf_using_stars <- function(x, cell_size = NULL, crs = NULL) {
  bbox <- sf::st_bbox(x)
  n_col <- ((bbox$xmax - bbox$xmin) / cell_size) |> unname() |> ceiling()
  n_row <- ((bbox$ymax - bbox$ymin) / cell_size) |> unname() |> ceiling()

  grd <- x |>
    .make_full_grid(cell_size)

  grd_geometry <- grd |>
    dplyr::select(cell_id)

  has_num_cols <-
    (Filter(is.numeric, x) |>
      names() |>
      dplyr::setdiff(c("geometry", "cell_id")) |>
      length()) > 0

  is_linestring <- .is_line_string(x)

  if (has_num_cols) {
    int_list <- grd |>
      sf::st_intersects(x) |>
      stats::setNames(seq(1, nrow(grd))) |>
      purrr::compact()

    if (is_linestring) {
      weighting_factor_calc_func <- sf::st_length
    } else {
      weighting_factor_calc_func <- sf::st_area
    }


    progressr::handlers("cli")
    p_bar_inter <- progressr::progressor(length(int_list) + 1)
    no_cores <- parallelly::availableCores() - 1
    future::plan(future::multisession, workers=no_cores)
    p_bar_inter("Calculating intersections.")
    grd <- names(int_list) |>
      as.integer() |>
      furrr::future_map2(
        int_list,
        \(n_e, e) {
          p_bar_inter(message = glue::glue("Resampling cell: { grd[n_e, ]$cell_id }"))
          suppressWarnings(
            cell_grd <- grd[n_e, ] |>
              sf::st_intersection(x[e, ]) |>
              dplyr::mutate(..weighting_factor = as.numeric(weighting_factor_calc_func(geometry))) |>
              dplyr::arrange(dplyr::desc(..weighting_factor))
          )
        },
        .options = furrr::furrr_options(seed = TRUE)
      ) |>
      purrr::list_rbind() |>
      tidyr::drop_na() |>
      dplyr::filter(..weighting_factor > 0 & !is.na(..weighting_factor))

    future::plan(future::sequential)
  } else {
    grd <- grd |>
      sf::st_join(x, left = FALSE) |>
      tidyr::drop_na()
    grd$..weighting_factor <- 1
  }

  suppressWarnings(
    grd <- grd |>
      as.data.frame() |>
      select(-geometry) |>
      dplyr::group_by(cell_id) |>
      dplyr::reframe(
        dplyr::across(dplyr::where(.is_float), ~ weighted.mean(.x, w=..weighting_factor, na.rm = TRUE)),
        dplyr::across(dplyr::where(.is_integer), ~ median(.x, na.rm = TRUE)),
        dplyr::across(dplyr::where(is.character), ~ .x[[1]])
      ) |>
      dplyr::distinct() |>
      dplyr::select(-..weighting_factor)
  )
  grd <- grd_geometry |>
    dplyr::inner_join(grd, by = dplyr::join_by(cell_id))

  return(grd)
}

.attribute_bioclim_to_rivers <- function(rivers_sf, grid_sf) {
  # Check if inputs are sf objects
  if (!all(inherits(rivers_sf, "sf"), inherits(grid_sf, "sf"))) {
    stop("Both inputs must be sf objects")
  }

  # Ensure CRS are identical
  if (sf::st_crs(rivers_sf) != sf::st_crs(grid_sf)) {
    message("Transforming grid to match rivers CRS")
    grid_sf <- sf::st_transform(grid_sf, sf::st_crs(rivers_sf))
  }

  # Check for overlaps
  if (!any(lengths(sf::st_intersects(rivers_sf, grid_sf)) > 0)) {
    stop("No overlaps between rivers and grid")
  }
  rivers_sf <- rivers_sf[grid_sf,]

  # Get only bioclimatic columns (exclude geometry)
  bioclim_cols <- setdiff(names(grid_sf), c("geometry", "grid_id"))

  # Function to process each river feature
  process_river_feature <- function(river_feature, grid) {
    # Convert to single part if it's a multilinestring
    if (sf::st_geometry_type(river_feature) == "MULTILINESTRING") {
      suppressWarnings(river_lines <- sf::st_cast(river_feature, "LINESTRING"))
      river_lines <- river_lines |>
        dplyr::mutate(original_id = 1:dplyr::n())
    } else {
      river_lines <- river_feature |>
        dplyr::mutate(original_id = 1)
    }

    # Process each line segment
    result <- lapply(1:nrow(river_lines), function(i) {
      line <- river_lines[i, ]

      # Find intersecting grid cells
      intersects <- sf::st_intersects(line, grid, sparse = FALSE)[1, ]
      if (!any(intersects)) return(NULL)

      intersecting_grids <- grid[intersects, ]

      # Split line by grid boundaries
      if (nrow(intersecting_grids) > 1) {
        # Create union of all grid boundaries
        #grid_bounds <- st_union(st_geometry(intersecting_grids))

        # Split line by grid boundaries
        suppressWarnings(split_lines <- lwgeom::st_split(sf::st_geometry(line), intersecting_grids) |>
          sf::st_collection_extract("LINESTRING") |>
          sf::st_as_sf() |>
          dplyr::rename(geometry = x))

        # For each split segment, find which grid cell it's in
        segs_with_bioclim <- lapply(1:nrow(split_lines), function(j) {
          seg <- split_lines[j, ]

          # Find which grid cell contains this segment (using centroid if needed)
          seg_centroid <- sf::st_centroid(seg)
          grid_index <- sf::st_intersects(seg_centroid, intersecting_grids, sparse = FALSE)[1, ]
          if(!any(grid_index)) {
            # If centroid doesn't work, try the nearest from centroid
            grid_index <- sf::st_nearest_feature(seg_centroid, intersecting_grids)
          }

          if (!any(grid_index)) return(NULL)

          # Get bioclimatic data
          bioclim_data <- sf::st_drop_geometry(intersecting_grids[grid_index, bioclim_cols, drop = FALSE])

          # Combine with original river attributes
          river_attrs <- sf::st_drop_geometry(line) |>
            dplyr::select(-original_id)

          # Create new sf object for this segment
          seg_result <- cbind(river_attrs, bioclim_data, seg) |>
            sf::st_as_sf()

          return(seg_result)
        })

        # Combine all segments
        x <- do.call(rbind, segs_with_bioclim)
        return(x)
      } else {
        # Only one grid cell - just add bioclim data
        bioclim_data <- sf::st_drop_geometry(intersecting_grids[, bioclim_cols, drop = FALSE])
        river_attrs <- sf::st_drop_geometry(line) |>
          dplyr::select(-original_id)
        if(nrow(bioclim_data) > 1) {
          cli::cli_warn("Multiple grid cells found for a single line segment. Taking the first one.")
          bioclim_data <- bioclim_data[1, ]
        }
        x <- cbind(river_attrs, bioclim_data, sf::st_geometry(line)) |>
          sf::st_as_sf()
        return(x)
      }
    })

    # Combine results for all lines in this feature
    x <- do.call(rbind, result)
    return(x)
  }

  # Process all river features
  final_result <- lapply(1:nrow(rivers_sf), function(i) {
    process_river_feature(rivers_sf[i, ], grid_sf)
  })

  # Combine all results
  final_sf <- do.call(rbind, final_result)

  # Clean up geometry column name if needed
  if (!inherits(final_sf, "sf")) {
    final_sf <- sf::st_as_sf(final_sf)
  }

  return(final_sf)
}

#' @export
.sdm_area <- function(x) {
  sa <- structure(
    list(
      grid = x$grid,
      cell_size = x$cell_size
    ),
    class = "sdm_area"
  )
  .check_sdm_area(sa)
  return(sa)
}

.check_sdm_area <- function(x) {
  error_collection <- checkmate::makeAssertCollection()

  assert_class_cli(
    x,
    classes = "sdm_area",
    null.ok = FALSE,
    .var.name = "sdm_area",
    add = error_collection
  )

  assert_class_cli(
    x$grid,
    classes = c("sf"),
    null.ok = FALSE,
    .var.name = "grid",
    add = error_collection
  )
  assert_data_frame_cli(
    x$grid,
    min.rows = 1,
    min.cols = 2,
    null.ok = FALSE,
    .var.name = "grid",
    add = error_collection
  )
  x_colnames <- x$grid |> colnames()
  assert_names_cli(
    x_colnames,
    must.include = c("cell_id", "geometry"),
    .var.name = "grid",
    add = error_collection
  )
  assert_character_cli(
    x_colnames,
    any.missing = FALSE,
    all.missing = FALSE,
    min.len = 2,
    unique = TRUE,
    null.ok = FALSE,
    .var.name = "grid",
    add = error_collection
  )
  assert_class_cli(
    x$grid$geometry,
    classes = c("sfc"),
    null.ok = FALSE,
    .var.name = "geometry",
    add = error_collection
  )

  cell_id <- x$grid[["cell_id"]]
  assert_numeric_cli(
    cell_id,
    any.missing = FALSE,
    all.missing = FALSE,
    lower = 1,
    null.ok = FALSE,
    unique = TRUE,
    typed.missing = TRUE,
    .var.name = "cell_id",
    add = error_collection
  )

  assert_number_cli(
    x$cell_size,
    na.ok = FALSE,
    null.ok = FALSE,
    lower = 0,
    .var.name = "cell_size",
    add = error_collection
  )

  # Next check is only executed if no errors are
  # To improve performance found up to this line.
  if (error_collection$isEmpty() & "POLYGON" %in% st_geometry_type(x$grid)){
    polygon_area <- x$cell_size * x$cell_size
    invalid_row <- x$grid$geometry |>
      purrr::detect_index(
        \(p) !is.logical(all.equal(sf::st_area(p), polygon_area))
      )
    if (invalid_row > 0){
      error_collection$push(
        glue::glue("The polygon of the row { invalid_row }",
                   " of the grid is different from cell_size.")
      )
    }
  }

  if (!error_collection$isEmpty()){
    error_messages <- error_collection$getMessages() |>
      fmt_bullet_cli(cli_bullet="i")
    error_messages <- c(
      c(
        "x" = "sdm_area object is corrupted!",
        "!" = "sdm_area object was changed without using caretSDM functions.",
        "!" = "The following inconsistencies were found:"
      ),
      error_messages
    )

    cli::cli_abort(
      error_messages
    )
  }
}


.detect_sdm_area <- function(x, cell_size, crs){
  error_collection <- checkmate::makeAssertCollection()
  assert_class_cli(
    x,
    classes = c("sf"),
    null.ok = FALSE,
    .var.name = "sdm_area",
    add = error_collection
  )
  assert_data_frame_cli(
    x,
    min.rows = 1,
    min.cols = 2,
    null.ok = FALSE,
    .var.name = "grid",
    add = error_collection
  )
  x_colnames <- x |> colnames()
  assert_names_cli(
    x_colnames,
    must.include = c("cell_id", "geometry"),
    .var.name = "grid",
    add = error_collection
  )
  assert_character_cli(
    x_colnames,
    any.missing = FALSE,
    all.missing = FALSE,
    min.len = 2,
    unique = TRUE,
    null.ok = FALSE,
    .var.name = "grid",
    add = error_collection
  )
  assert_class_cli(
    x$geometry,
    classes = c("sfc"),
    null.ok = FALSE,
    .var.name = "geometry",
    add = error_collection
  )
  cell_id <- x[["cell_id"]]
  assert_numeric_cli(
    cell_id,
    any.missing = FALSE,
    all.missing = FALSE,
    lower = 1,
    null.ok = FALSE,
    unique = TRUE,
    typed.missing = TRUE,
    .var.name = "cell_id",
    add = error_collection
  )

  if (!error_collection$isEmpty()){
    return(invisible(error_collection$getMessages()))
  }

  polygon <- x$geometry[[1]]
  if (is.null(polygon) || .is_line_string(polygon)){
    return(invisible(glue::glue("x has other features than of polygons.")))
  }

  polygon_area <- cell_size * cell_size
  cell_size_calc <- (polygon |> as.matrix())[2,1] - (polygon |> as.matrix())[1,1]
  invalid_row <- x$geometry |>
    purrr::detect_index(
      \(p) !(sf::st_area(p) |> all.equal(polygon_area) |> is.logical())
    )
  if (invalid_row > 0){
    error_collection$push(
      glue::glue("The cell size of the polygon of the row { invalid_row }",
                 " of the grid is different from the cell_size.")
    )
  }

  if (sf::st_crs(x) != sf::st_crs(crs)){
    crs_calc <- substr(sf::st_crs(x)$input, 1, 20)
    crs_param <- substr(sf::st_crs(crs)$input, 1, 20)
    error_collection$push(
      glue::glue("Detected CRS ({ crs_calc }) is different from ",
                 "informed one ({ crs_param }).")
    )
  }
  if (!error_collection$isEmpty()) {
    error_messages <- error_collection$getMessages() |>
      fmt_bullet_cli(cli_bullet="i")
    cli::cli_warn(c(
      "!" = "A sdm_area object was detected but some parameters are different. Please check it!",
      error_messages
    ))
  }
  l <- list(
    grid = x,
    cell_size = cell_size_calc
  )
  sa <- .sdm_area(l)
  return(sa)
}

.is_gdal_installed <- function() {
  gdal_installed <- sf::sf_extSoftVersion()["GDAL"] |> unname()
  return((!is.na(gdal_installed) || !is.null(gdal_installed)) &&
           gdal_installed != "")
}

.select_variables <- function(x = NULL, variables_selected = NULL) {
  if (!is.null(variables_selected)){
    if (checkmate::test_list(variables_selected, len = 0)){
      x <- x |>
        dplyr::select(NULL)
    }
    else {
      variables_selected <- variables_selected |>
        unlist()
      var_not_found <- variables_selected |>
        dplyr::setdiff(names(x))
      x <- x |>
        dplyr::select(dplyr::any_of(variables_selected))
      if (length(var_not_found)>0) {
        cli::cli_warn(c(
          "!" = "Some selected variables not found!",
          "i" = "Variables: { var_not_found }."
        ))
      }
    }
  }
  x <- x |>
    dplyr::select(-dplyr::any_of("fid"))
  return(x)
}

.make_full_grid <- function(x = NULL, cell_size = NULL){
  bbox <- sf::st_bbox(x)
  n_col <- ((bbox$xmax - bbox$xmin) / cell_size) |> unname() |> ceiling()
  n_row <- ((bbox$ymax - bbox$ymin) / cell_size) |> unname() |> ceiling()

  grd <- bbox |>
    stars::st_as_stars(
      nx = n_col,
      ny = n_row,
      dx = cell_size,
      dy = cell_size,
      values = seq(1, n_row * n_col)
    ) |>
    stats::setNames("cell_id")
  attr(grd, "dimensions")[[2]]$delta <- (-cell_size)

  grd <- grd |>
    sf::st_as_sf()
  return(grd)
}

.is_line_string <- function(x = NULL) {
  x |>
    sf::st_geometry_type(by_geometry = F) %in%
    c("LINESTRING", "MULTILINESTRING", "CIRCULARSTRING", "MULTICURVE")
}

.try_split <- function(x = NULL) {
  x_tmp <- try(split(x), silent = T)
  if (!("try-error" %in% class(x_tmp))) {
    return(x_tmp)
  } else {
    return(x)
  }
}

.adjust_bbox <- function(x = NULL, cell_size = NULL) {
  bbox <- x |>
    sf::st_bbox()

  centroid <- (bbox |> sf::st_as_sfc() |> sf::st_centroid())[[1]] |>
    as.list() |>
    stats::setNames(c("x", "y"))

  n_col <- ((bbox$xmax - bbox$xmin) / cell_size) |> ceiling()
  n_row <- ((bbox$ymax - bbox$ymin) / cell_size) |> ceiling()
  tot_w <- n_col * cell_size
  tot_h <- n_row * cell_size
  new_bbox <- c(
    xmin = (centroid$x - (tot_w/2)) |> unname(),
    ymin = (centroid$y - (tot_h/2)) |> unname(),
    xmax = (centroid$x - (tot_w/2) + tot_w) |> unname(),
    ymax = (centroid$y - (tot_h/2) + tot_h) |> unname()
  )
  attr(new_bbox, "class") <- "bbox"
  attr(sf::st_geometry(x), "bbox") <- new_bbox

  return(x)
}

.adjust_cell_id_name <- function(x = NULL, variables_selected = NULL) {
  if ("cell_id" %in% names(x)) {
    pos_cell_id <- match("cell_id", names(x))
    names(x)[pos_cell_id] <- paste0("cell_id.", pos_cell_id)
    if (length(variables_selected) > 0) {
      variables_selected <- variables_selected |>
        stringr::str_replace_all("cell_id", paste0("cell_id.", pos_cell_id))
    }
  }
  return(
    list(
      x = x,
      variables_selected = variables_selected
    )
  )
}

.adjust_geom_col <- function(x = NULL) {
  geom_name <- attr(x, "sf_column")
  x <- x |>
    stats::setNames(names(x) |> stringr::str_replace(geom_name, "geometry"))
  sf::st_geometry(x) <- "geometry"
  return(x)
}

.is_float <- function(n) {
  is.numeric(n) && !all(n %% 1 == 0, na.rm = TRUE)
}

.is_integer <- function(n) {
  (is.numeric(n) && !.is_float(n))
}

#' @rdname sdm_area
#' @export
get_sdm_area <- function(i) {
  if( is_input_sdm(i)) {
    x <- i$predictors
  }
  if( is_sdm_area(i)) {
    x <- i
  }
  assert_class_cli(x, "sdm_area")
  x <- x$grid |> select(c(cell_id))
  return(x)
}


#' @exportS3Method base::print
print.sdm_area <- function(x, ...) {
  .check_sdm_area(x)
  cat("          caretSDM         \n")
  cat("...........................\n")
  cat("Class                     : sdm_area\n")
  cat("Extent                    :", sf::st_bbox(x$grid), "(xmin, xmax, ymin, ymax)\n")
  cat("CRS                       :", substr(sf::st_crs(x$grid)$input, 1, 20), "\n")
  cat("Resolution                :", paste0("(", x$cell_size, ", ", x$cell_size, ")"), "(x, y)\n")
  predictors_sdm <- predictors(x)
  if (length(predictors_sdm)>0) {
    cat("Number of Predictors      :", length(predictors_sdm), "\n")
    cat(cat("Predictors Names          : "), cat(predictors_sdm, sep = ", "),
        "\n")
  }
  scen_names <- scenarios_names(x)
  if (length(scen_names)>0) {
    cat("Number of Scenarios      :", length(scen_names), "\n")
    cat(cat("Scenarios Names          : "), cat(scen_names, sep = ", "),
        "\n")
  }
  invisible(x)
}
