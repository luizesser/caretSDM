#' Write caretSDM data
#'
#' This function exports caretSDM data.
#'
#' @usage write_ensembles(x, path = "results/ensembles", ext = ".tif")
#'
#' @param x Object to be written. Can be of class \code{input_sdm}, \code{predictions} or
#' \code{models}.
#' @param path A path with filename and the proper extension (see details) or the directory to save
#' files in.
#' @param extension How it should be saved?
#' @param file_path A path to save the \code{sdm_area} GeoPackage file.
#' @param file_name The name of the \code{sdm_area} GeoPackage file to be saved without extension.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
write_ensembles <- function(x, path = "results/ensembles", ext = ".tif") {
  if (class(x) == "input_sdm") {
    y <- x$predictions
  } else {
    y <- x
  }
  ext_sf <- c("bna", "csv", "e00", "gdb", "geojson", "gml", "gmt", "gpkg", "gps", "gtm", "gxt", "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp", "sqlite", "vdv", "xls", "xlsx")
  scen <- colnames(y$ensembles)
  spp <- rownames(y$ensembles)
  grd <- y$grid
  for (sp in spp) {
    for (sc in scen) {
      v <- y[["ensembles"]][[sp, sc]]
      if (is.data.frame(v)) {
        result <- merge(grd, v, by = "cell_id")
        if (!dir.exists(paste0(path, "/", sp))) {
          dir.create(paste0(path, "/", sp), recursive = T)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(st_rasterize(result))
          write_stars(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext %in% ext_sf) {
          st_write(result, paste0(path, "/", sp, "/", sc, ext))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_predictions <- function(x, path = "results/predictions", ext = ".tif") {
  if (class(x) == "input_sdm") {
    y <- x$predictions
  } else {
    y <- x
  }
  ext_sf <- c("bna", "csv", "e00", "gdb", "geojson", "gml", "gmt", "gpkg", "gps", "gtm", "gxt", "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp", "sqlite", "vdv", "xls", "xlsx")
  scen <- names(y$predictions)
  spp <- names(y$predictions[[1]])
  grd <- y$grid
  for (sp in spp) {
    for (sc in scen) {
      cell_id <- y[["predictions"]][[sc]][[sp]][[1]]$cell_id
      for (id in names(y$predictions[[sc]][[sp]])) {
        v <- select(y$predictions[[sc]][[sp]][[id]], -"pseudoabsence")
        result <- merge(grd, v, by = "cell_id")
        if (!dir.exists(paste0(path, "/", sp))) {
          dir.create(paste0(path, "/", sp), recursive = T)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(st_rasterize(result))
          write_stars(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext %in% ext_sf) {
          st_write(result, paste0(path, "/", sp, "/", sc, ext))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_models <- function(x, path = "results/models") {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  spp <- names(y$models)
  for (sp in spp) {
    if (!dir.exists(paste0(path, "/", sp))) {
      dir.create(paste0(path, "/", sp), recursive = T)
    }
    saveRDS(y$models[[sp]], paste0(path, "/", sp, "/models.rds"))
  }
}

#' @rdname write_ensembles
#' @export
write_gpkg <- function(x, file_path, file_name) {
  assert_directory_cli(
    file_path
  )
  assert_character_cli(
    file_name,
    min.chars = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    typed.missing = TRUE,
    null.ok = FALSE
  )
  UseMethod("write_gpkg", x)
}

#' @rdname write_ensembles
#' @export
write_gpkg.sdm_area <- function(x, file_path, file_name) {
  file_name_ext <- "gpkg"
  file_name <- fs::path_file(file_name)
  file_path <- fs::path_dir(file_path)
  saving_local <- fs::path(file_path, file_name, ext = file_name_ext)
  cli::cli_inform(
    message = "Saving gpkg in: { saving_local }"
  )
  x$grid |>
    sf::st_write(
      dsn = saving_local,
      delete_dsn = TRUE,
      quiet =TRUE
    )
}
