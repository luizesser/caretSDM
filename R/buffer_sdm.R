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
#' study_area <- buffer_sdm(occ_data, size=5, crs=4326)
#' plot(study_area)
#'
#' @importFrom dplyr bind_rows all_of filter group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom pdp partial
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_smooth facet_wrap labs theme_minimal
#'
#' @export
buffer_sdm <- function(occ_data, size = NULL, crs = NULL) {
  assert_class_cli(size, "numeric")
  assert_class_cli(crs, "numeric")

  cnames <- find_columns(occ_data)
  x <- occ_data |>
    st_as_sf(coords=cnames[c(2,3)]) |>
    st_buffer(dist=size) |>
    st_union() |>
    st_as_sf(crs=st_crs(crs))

  x <- st_set_geometry(x, "geometry")

  return(x)
}
