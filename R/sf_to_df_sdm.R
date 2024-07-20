#' @keywords internal
#' Transform a occurrences simplefeature in a data.frame.
#' @param occ_sf SimpleFeatures to transform in data.frame. Usually from occurrences slot.
#' @return Data.frame with three columns: "species", "decimalLongitude" and "decimalLatitude".
#' @importFrom sf st_coordinates
#'
#' @export
sf_to_df_sdm <- function(occ_sf) {
  assert_class_cli(occ_sf, "sf")
  df <- occ_sf |>
    sf::st_coordinates() |>
    as.data.frame() |>
    cbind(species=occ_sf$species)
  df <- df[,c("species", "X", "Y")]
  colnames(df) <- c("species", "decimalLongitude", "decimalLatitude")
  return(df)
}
