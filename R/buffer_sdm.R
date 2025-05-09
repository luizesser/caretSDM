#' Create buffer around occurrences
#'
#' Obtain the Partial Dependence Plots (PDP) to each variable.
#'
#' @usage buffer_sdm(occ_data, size = NULL, crs = NULL)
#'
#' @param occ_data A \code{data.frame} object with species, decimalLongitude and decimalLatitude columns.
#' Usually the output from \code{GBIF_data}.
#' @param size \code{numeric}. The distance between the record and the margin of the buffer (i.e.
#' buffer radius).
#' @param crs \code{numeric}. Indicates which EPSG it the occ_data in.
#'
#' @return A buffer around occ_data records.
#'
#' @seealso \code{\link{GBIF_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' study_area <- buffer_sdm(occ, size=50000, crs=6933)
#' plot(study_area)
#'
#' @importFrom sf st_as_sf st_buffer st_union st_crs st_set_geometry
#'
#' @export
buffer_sdm <- function(occ_data, size = NULL, crs = NULL) {
  assert_class_cli(size, "numeric")
  assert_class_cli(crs, "numeric")

  cnames <- find_columns(occ_data)
  if(length(cnames)>2){
    cnames <- cnames[c(2,3)]
  }
  x <- occ_data |>
    sf::st_as_sf(coords=cnames[c(1,2)]) |>
    sf::st_buffer(dist=size) |>
    sf::st_union() |>
    sf::st_as_sf(crs=sf::st_crs(crs))

  x <- sf::st_set_geometry(x, "geometry")

  return(x)
}
