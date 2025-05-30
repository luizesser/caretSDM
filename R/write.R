#' Write caretSDM data
#'
#' This function exports caretSDM data.
#'
#' @usage write_ensembles(x, path = "results/ensembles", ext = ".tif", centroid = FALSE)
#'
#' @param x Object to be written. Can be of class \code{input_sdm}, \code{occurrences},
#' \code{predictions} or \code{models}.
#' @param path A path with filename and the proper extension (see details) or the directory to save
#' files in.
#' @param extension How it should be saved?
#' @param file_path A path to save the \code{sdm_area} GeoPackage file.
#' @param file_name The name of the \code{sdm_area} GeoPackage file to be saved without extension.
#' @param centroid Should coordinates for the centroids of each cell be included? Standard is FALSE.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom sf st_join st_write st_coordinates st_centroid
#' @importFrom stars write_stars
#' @importFrom fs path_file path_dir path
#' @importFrom cli cli_inform
#' @importFrom dplyr select
#'
#' @export
write_ensembles <- function(x, path = "results/ensembles", ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictions
  } else {
    y <- x
  }
  ext_sf <- c("bna", "csv", "e00", "gdb", "geojson", "gml", "gmt", "gpkg", "gps", "gtm", "gxt", "jml",
              "map", "mdb", "nc", "ods", "osm", "pbf", "shp", "sqlite", "vdv", "xls", "xlsx")
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
          dir.create(paste0(path, "/", sp), recursive = T)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(st_rasterize(result))
          stars::write_stars(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext %in% ext_sf) {
          sf::st_write(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext == ".csv") {
          write.csv(result, paste0(path, "/", sp, "/", sc, ".csv"))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_predictions <- function(x, path = "results/predictions", ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictions
  } else {
    y <- x
  }
  ext_sf <- c(".bna", ".csv", ".e00", ".gdb", ".geojson", ".gml", ".gmt", ".gpkg", ".gps", ".gtm", ".gxt", ".jml",
              ".map", ".mdb", ".nc", ".ods", ".osm", ".pbf", ".shp", ".sqlite", ".vdv", ".xls", ".xlsx")
  scen <- names(y$predictions)
  spp <- names(y$predictions[[1]])
  grd <- y$grid
  if(centroid){
    cent <- sf::st_coordinates(sf::st_centroid(grd))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd <- cbind(grd, cent)
  }
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
          stars::write_stars(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext %in% ext_sf) {
          sf::st_write(result, paste0(path, "/", sp, "/", sc, ext))
        } else if (ext == ".csv") {
          write.csv(result, paste0(path, "/", sp, "/", sc, ".csv"))
        }
      }
    }
  }
}

#' @rdname write_ensembles
#' @export
write_predictors <- function(x, path = "results/predictors", ext = ".tif", centroid = FALSE) {
  if (is_input_sdm(x)) {
    y <- x$predictors
  } else {
    y <- x
  }
  ext_sf <- c("bna", "csv", "e00", "gdb", "geojson", "gml", "gmt", "gpkg", "gps", "gtm", "gxt",
              "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp", "sqlite", "vdv", "xls", "xlsx")
  grd <- y$grid
  if(centroid){
    cent <- sf::st_coordinates(sf::st_centroid(grd))
    colnames(cent) <- c("x_centroid", "y_centroid")
    grd2 <- cbind(grd, cent)
  }
  if (!dir.exists(paste0(path, "/", sp))) {
    dir.create(paste0(path, "/", sp), recursive = T)
  }
  if (ext == ".tif" | ext == ".asc") {
    result <- merge(st_rasterize(result))
    stars::write_stars(result, paste0(path, "/", sp, "/", sc, ext))
  } else if (ext %in% ext_sf) {
    sf::st_write(result, paste0(path, "/", sp, "/", sc, ext))
  } else if (ext == ".csv") {
    write.csv(result, paste0(path, "/", sp, "/", sc, ".csv"))
  }
}

#' @rdname write_ensembles
#' @export
write_models <- function(x, path = "results/models") {
  if (is_input_sdm(x)) {
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

#' @rdname write_ensembles
#' @export
write_occurrences <- function(x, path = "results/occurrences.csv", grid = FALSE, ...) {
  if(is_input_sdm(x) & grid){
    suppressWarnings(dir.create(dirname(path), recursive = T))
    assert_directory_cli(dirname(path))

    grd <- sf::st_join(x$predictors$grid, x$occurrences$occurrences) |>
      dplyr::select(c(cell_id.x, species))
    colnames(grd) <- c("cell_id", "species", "geometry")
    #df <- sf_to_df_sdm(grd)
    #df$values <- rep(1, nrow(df))
    #df$species <- ifelse(is.na(df$species), "delete", df$species[])

    #tidyr::pivot_wider(df, names_from = species, values_from = values)
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
    write.csv(df, path, ...)
  }
}

#' @rdname write_ensembles
#' @export
write_pseudoabsences <- function(x, path = "results/predictors", ext = ".tif", ...) {
  grid <- ifelse(ext == ".tif", TRUE, FALSE)
  if(is_input_sdm(x) & grid){
    suppressWarnings(dir.create(dirname(path), recursive = T))
    assert_directory_cli(dirname(path))

    grd <- sf::st_join(x$predictors$grid, x$occurrences$occurrences) |>
      select(c(cell_id.x, species))
    colnames(grd) <- c("cell_id", "species", "geometry")

    sf::st_write(grd,
                 dsn = path,
                 delete_dsn = TRUE,
                 quiet =TRUE, ...)
  } else {
    if(is_input_sdm(x) & !grid) {
      x <- x$occurrences
    }
    assert_class_cli(x, "occurrences")
    suppressWarnings(dir.create(dirname(path), recursive = T))
    assert_directory_cli(dirname(path))

    df <- occurrences_as_df(x)
    write.csv(df, path, ...)
  }
}

#' @rdname write_ensembles
#' @export
write_scenarios <- function(x, path = "results/predictors", ext = ".tif", ...) {

}

#' @rdname write_ensembles
#' @export
write_grid <- function(x, path = "results/grid_study_area.gpkg", centroid = FALSE, ...) {
  if(is_input_sdm(x)){
    x <- x$predictors
  }
  assert_class_cli(x, "sdm_area")
  suppressWarnings(dir.create(dirname(path), recursive = T))
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
write_validation_metrics <- function(x, path = "results/validation_metrics", ...) {
  #suppressWarnings(dir.create(path, recursive = T))
  #caretSDM:::assert_directory_cli(dirname(path))
  spp <- species_names(x)
  val <- get_validation_metrics(x)
  for (sp in spp) {
    suppressWarnings(dir.create(paste0(path, "/", sp), recursive = T))
    write.csv(val[[sp]], paste0(path, "/", sp, "/validation_metrics.csv"))
  }
}
