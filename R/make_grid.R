#' Make grid
#'
#' Build a grid out of given shape and variables.
#'
#' @param pred A predictors object
#' @param th Threshold
#' @param maxobservations Number of sets of pseudoabsence to create
#'
#' @return A predictors object with VIF data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import sf
#' @import dplyr
#'
#' @export
make_grid <- function(shp, cell_width=0, cell_height=0, var_names=NULL, centroid=F, epsg=NULL){
  sfc_as_cols <- function(x, geometry, names = c("x","y")) {
    if (missing(geometry)) {
      geometry <- sf::st_geometry(x)
    } else {
      geometry <- rlang::eval_tidy(enquo(geometry), x)
    }
    stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
    ret <- sf::st_coordinates(geometry)
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    x <- x[ , !names(x) %in% names]
    ret <- setNames(ret,names)
    dplyr::bind_cols(x,ret)
  }

  if (!(sf::st_geometry(shp) %>% class() %in% "sfc" %>% any())){
    stop("Invalid study area file!")
  }
  if (cell_width<=0 || cell_height<=0){
    stop("Invalid cell size!")
  }
#  shp <- shp %>%
#    dplyr::rename_all(tolower)
#
#  if (!is.null(var_names)){
#    shp <- shp %>%
#      dplyr::select(var_names %>% tolower() %>% all_of())
#  }

  if(!is.null(epsg)){
    shp <- st_transform(shp, st_crs(paste0("+init=epsg:",epsg)))
  }

  bbox <- shp %>%
    sf::st_bbox() %>%
    as.vector()

  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]

  n_x_cell <- (abs(xmax - xmin) / cell_width) %>%
    ceiling()
  n_y_cell <- (abs(ymax - ymin) / cell_height) %>%
    ceiling()

  new_xmin <- ((xmin / cell_width) %>% trunc()) * cell_width
  new_ymin <- ((ymin / cell_height) %>% trunc()) * cell_height
  new_xmax <- new_xmin + (n_x_cell * cell_width)
  new_ymax <- new_ymin + (n_y_cell * cell_height)

  attr(sf::st_geometry(shp), "bbox") <- sf::st_bbox(
    c(xmin=new_xmin, xmax=new_xmax, ymax=new_ymax, ymin=new_ymin) %>%
      sf::st_bbox()
  )

  shp_grid <- shp %>%
    sf::st_make_grid(what = "polygons", n=c(n_x_cell, n_y_cell))

  shp_grid <- st_sf(geometry=shp_grid) %>%
    rowid_to_column("cell_id")

  grid_cells <- shp_grid %>%
    sf::st_intersects(shp) %>%
    as.data.frame() %>%
    rename(cell_id=row.id, area_id=col.id) %>%
    dplyr::left_join(shp_grid, by = "cell_id")

  shp_grid <- grid_cells %>%
    dplyr::left_join(
      shp %>%
        as.data.frame() %>%
        tibble::rowid_to_column("area_id") %>%
        dplyr::select(-geometry),
      by = "area_id"
    ) %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarise_all(~ ifelse(is.numeric(.), mean(.), .), na.rm = TRUE) %>%
    dplyr::select(-area_id) %>%
    #tibble::rowid_to_column("cell_id") %>%
    st_sf(crs=sf::st_crs(shp), sf_column_name = "geometry")

  if (centroid){
    centroids <-  shp_grid %>%
      sf::st_geometry() %>%
      sf::st_centroid() %>%
      sf::st_coordinates() %>%
      as.data.frame()

    shp_grid <- shp_grid %>%
      dplyr::bind_cols(centroids) %>%
      dplyr::rename(x_centroid = X, y_centroid = Y)
  }
  return(shp_grid)
}
