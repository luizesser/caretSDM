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
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom stars st_rasterize write_stars
#' @importFrom sf st_write
#' @importFrom dplyr select
#'
#' @export
write_ensembles <- function(x, path = "results/ensembles", ext = ".tif") {
  if (is_input_sdm(x)) {
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
write_predictions <- function(x, path = "results/predictions", ext = ".tif") {
  if (is_input_sdm(x)) {
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
        v <- dplyr::select(y$predictions[[sc]][[sp]][[id]], -"pseudoabsence")
        result <- merge(grd, v, by = "cell_id")
        if (!dir.exists(paste0(path, "/", sp))) {
          dir.create(paste0(path, "/", sp), recursive = T)
        }
        if (ext == ".tif" | ext == ".asc") {
          result <- merge(st_rasterize(result))
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
