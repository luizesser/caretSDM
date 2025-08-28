#' Write caretSDM data
#'
#' This function exports caretSDM data.
#'
#' @usage write_ensembles(x, path = NULL, ext = ".tif", centroid = FALSE)
#'
#' @param x Object to be written. Can be of class \code{input_sdm}, \code{occurrences},
#' \code{predictions} or \code{models}.
#' @param path A path with filename and the proper extension (see details) or the directory to save
#' files in.
#' @param ext How it should be saved?
#' @param centroid Should coordinates for the centroids of each cell be included? Standard is FALSE.
#' @param file_path A path to save the \code{sdm_area} GeoPackage file.
#' @param file_name The name of the \code{sdm_area} GeoPackage file to be saved without extension.
#' @param grid Boolean. Return a grid.
#' @param ... Arguments to pass to \code{sf::st_write} or \code{write.csv}.
#'
#' @details
#' \code{ext} can be set accordingly to the desired output. Possible values are .tif and .asc for
#' rasters, .csv for for a spreadsheet, but also  one of: c("bna", "csv", "e00", "gdb", "geojson",
#' "gml", "gmt", "gpkg", "gps", "gtm", "gxt", "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp",
#' "sqlite", "vdv", "xls", "xlsx").
#' \code{path} ideally should only provide the folder. We recommend using:
#' \code{results/what_are_you_writting}. So for writting ensembles users are advised to run:
#' \code{path = "results/ensembles"}
#'
#' @returns No return value, called for side effects.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom sf st_join st_write st_coordinates st_centroid
#' @importFrom stars write_stars st_rasterize
#' @importFrom fs path_file path_dir path
#' @importFrom cli cli_inform
#' @importFrom dplyr select
#' @importFrom utils write.csv
#'
#' @global cell_id.x species sp sc geometry
#'
#' @export
write_ensembles <- function(x, path = NULL, ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictions
  } else {
    y <- x
  }
  assert_character_cli(path, null.ok = FALSE)
  ext_sf <- c(".bna", ".csv", ".e00", ".gdb", ".geojson", ".gml", ".gmt", ".gpkg", ".gps", ".gtm", ".gxt", ".jml",
              ".map", ".mdb", ".nc", ".ods", ".osm", ".pbf", ".shp", ".sqlite", ".vdv", ".xls", ".xlsx")
  scen <- colnames(y$ensembles)
  spp <- rownames(y$ensembles)
  grd <- y$grid
  if(centroid){
    suppressWarnings(cent <- sf::st_coordinates(sf::st_centroid(grd)))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd <- cbind(grd, cent)
  }
  for (sp in spp) {
    for (sc in scen) {
      v <- y[["ensembles"]][[sp, sc]]
      if (is.data.frame(v)) {

        result <- merge(grd, v, by = "cell_id")
        if (!dir.exists(paste0(path, "/", sp))) {
          dir.create(paste0(path, "/", sp), recursive = TRUE)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(stars::st_rasterize(result))
          stars::write_stars(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext %in% ext_sf) {
          sf::st_write(result, paste0(path, "/", sp, "/", sc, ext))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_predictions <- function(x, path = NULL, ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictions
  } else {
    y <- x
  }
  #assert_character_cli(path, null.ok = FALSE)
  ext_sf <- c(".bna", ".csv", ".e00", ".gdb", ".geojson", ".gml", ".gmt", ".gpkg", ".gps", ".gtm", ".gxt", ".jml",
              ".map", ".mdb", ".nc", ".ods", ".osm", ".pbf", ".shp", ".sqlite", ".vdv", ".xls", ".xlsx")
  scen <- names(y$predictions)
  spp <- names(y$predictions[[1]])

  for (sp in spp) {
    for (sc in scen) {
      for (id in names(y$predictions[[sc]][[sp]])) {
        result <- dplyr::select(y$predictions[[sc]][[sp]][[id]], -"pseudoabsence")
        if(centroid){
          cent <- sf::st_coordinates(sf::st_centroid(result))
          colnames(cent) <- c("x_centroid", "y_centroid")
          result <- cbind(result, cent)
        }
        if (!dir.exists(paste0(path, "/", sp, "/", sc, "/predictions"))) {
          dir.create(paste0(path, "/", sp, "/", sc, "/predictions"), recursive = TRUE)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(stars::st_rasterize(result))
          stars::write_stars(result, paste0(path, "/", sp, "/", sc, "/predictions/", id, ext))
        } else if (ext %in% ext_sf) {
          sf::st_write(result, paste0(path, "/", sp, "/", sc, "/predictions/", id, ext))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_predictors <- function(x, path = NULL, ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictors
  } else {
    y <- x
  }
  assert_character_cli(path, null.ok = FALSE)

  ext_sf <- c(".bna", ".csv", ".e00", ".gdb", ".geojson", ".gml", ".gmt", ".gpkg", ".gps", ".gtm", ".gxt", ".jml",
              ".map", ".mdb", ".nc", ".ods", ".osm", ".pbf", ".shp", ".sqlite", ".vdv", ".xls", ".xlsx")
  grd <- y$grid[,get_predictor_names(y)]
  if(centroid){
    cent <- sf::st_coordinates(sf::st_centroid(grd))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd <- cbind(grd, cent)
  }

  if (ext == ".tif" | ext == ".asc") {
    grd <- merge(stars::st_rasterize(grd))
    stars::write_stars(grd, paste0(path, "/predictors", ext))
  } else if (ext %in% ext_sf) {
    sf::st_write(grd, paste0(path, "/predictors", ext))
  }
}

#' @rdname write_ensembles
#' @export
write_models <- function(x, path = NULL) {
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  assert_character_cli(path, null.ok = FALSE)

  spp <- names(y$models)
  for (sp in spp) {
    if (!dir.exists(paste0(path, "/", sp))) {
      dir.create(paste0(path, "/", sp), recursive = TRUE)
    }
    assert_directory_cli(dirname(path))
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

#' @rdname write_ensembles
#' @export
write_occurrences <- function(x, path = NULL, grid = FALSE, ...) {
  assert_character_cli(path, null.ok = FALSE)
  assert_logical_cli(grid, len = 1)

  if(is_input_sdm(x) & grid){
    suppressWarnings(dir.create(dirname(path), recursive = TRUE))
    assert_directory_cli(dirname(path))

    grd <- sf::st_join(x$predictors$grid, x$occurrences$occurrences) |>
      dplyr::select(c(cell_id.x, species))
    colnames(grd) <- c("cell_id", "species", "geometry")
        sf::st_write(grd,
                 dsn = path,
                 delete_dsn = TRUE,
                 quiet =TRUE, ...)
  } else {
    if(is_input_sdm(x) & !grid) { #### Quando grid==T fazer grid com 0 e 1
      x <- x$occurrences
    }
    assert_class_cli(x, "occurrences")
    suppressWarnings(dir.create(dirname(path), recursive = T))
    assert_directory_cli(dirname(path))

    df <- occurrences_as_df(x)
    utils::write.csv(df, path, ...)
  }
}

#' @rdname write_ensembles
#' @export
write_pseudoabsences <- function(x, path = NULL, ext = ".csv", centroid = FALSE) {
  assert_character_cli(path, null.ok = FALSE)
  assert_directory_cli(dirname(path))
  assert_logical_cli(centroid, len = 1)

  y <- pseudoabsence_data(x)
  ext_sf <- c(".bna", ".csv", ".e00", ".gdb", ".geojson", ".gml", ".gmt", ".gpkg", ".gps", ".gtm", ".gxt", ".jml",
              ".map", ".mdb", ".nc", ".ods", ".osm", ".pbf", ".shp", ".sqlite", ".vdv", ".xls", ".xlsx")
  spp <- species_names(x)
  grd <- get_sdm_area(x)
  if(centroid){
    suppressWarnings(cent <- sf::st_coordinates(sf::st_centroid(grd)))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd <- cbind(grd, cent)
  }
  for (sp in spp) {
    v <- y[[sp]]
    for (n in 1:length(v)) {
      v2 <- v[[n]] |> as.data.frame() |> dplyr::select(-geometry)
      result <- merge(grd, as.data.frame(v2), by = "cell_id")
      if (!dir.exists(paste0(path, "/", sp))) {
        dir.create(paste0(path, "/", sp), recursive = TRUE)
      }
      if (ext == ".tif" | ext == ".asc") {
        result <- merge(stars::st_rasterize(result))
        stars::write_stars(result, paste0(path, "/", sp, "/pseudoabsences_", n, ext))
      } else if (ext %in% ext_sf) {
        sf::st_write(result, paste0(path, "/", sp, "/pseudoabsences_", n, ext))
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_grid <- function(x, path = NULL, centroid = FALSE) {
  if(is_input_sdm(x)){
    x <- x$predictors
  }
  assert_character_cli(path, null.ok = FALSE)
  assert_logical_cli(centroid, len = 1)

  assert_class_cli(x, "sdm_area")
  suppressWarnings(dir.create(dirname(path), recursive = TRUE))
  assert_directory_cli(dirname(path))

  grd <- x$grid
  if(centroid){
    cent <- sf::st_coordinates(sf::st_centroid(grd))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd <- cbind(grd, cent)
  }
  grd |>
    sf::st_write(
      dsn = path,
      delete_dsn = TRUE,
      quiet =TRUE
    )
}

#' @rdname write_ensembles
#' @export
write_validation_metrics <- function(x, path = NULL) {
  assert_character_cli(path, null.ok = FALSE)
  assert_directory_cli(dirname(path))
  spp <- species_names(x)
  val <- get_validation_metrics(x)
  for (sp in spp) {
    suppressWarnings(dir.create(paste0(path, "/", sp), recursive = TRUE))
    utils::write.csv(val[[sp]], paste0(path, "/", sp, "/validation_metrics.csv"))
  }
}
