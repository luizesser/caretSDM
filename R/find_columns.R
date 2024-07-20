#' @keywords internal
#' Find species, longitude and latitudecolumns in a data.frame.
#' @param data data.frame to search for.
#' @return Vector of column names that correspond to the species, longitude and latitude columns.
#' @export
find_columns <- function(df, col_names = NULL, spp = TRUE) {
  assert_data_frame_cli(df)
  if (!is.null(col_names)) {
    assert_character_cli(col_names, min.len = 2, max.len = 3)
  }

  if (is.null(col_names)) {
    col_names <- colnames(df)

    lon_keywords <- c("lon", "x")
    lat_keywords <- c("lat", "y")

    lon_col <- colnames(df)[grep(paste(lon_keywords, collapse = "|"), tolower(colnames(df)))]
    lat_col <- colnames(df)[grep(paste(lat_keywords, collapse = "|"), tolower(colnames(df)))]

    if(length(c(lat_col,lon_col))<2){
      cli::cli_abort(c(
        "x" = "A latitude or longitude column is missing on {.var data.frame}.",
        "i" = "Check if colnames are correct. They should resemble latitude and longitude like
        'decimalLatitude', 'decimalLongitude', 'latitude', 'longitude' or 'y', 'x'."
      ))
    }

    if (spp) {
      spp_keywords <- c("sp", "names")
      spp_col <- colnames(df)[grep(paste(spp_keywords, collapse = "|"), tolower(colnames(df)))]
      col_names <- c(spp_col, lon_col, lat_col)
    } else {
      col_names <- c(lon_col, lat_col)
    }
  } else {
    if (!all(col_names %in% colnames(df))) {
      cli::cli_abort(c(
        "x" = "Given {.var col_names} are not valid column names for {.var df}.",
        "i" = "Check if {.var col_names} are in {.var df} by ruinning colnames(df). Column names
        should resemble latitude, longitude and species like 'decimalLatitude', 'decimalLongitude',
        'latitude', 'longitude' or 'y', 'x' and 'sp', 'spp' or 'species'."
      ))
    }
  }
  return(col_names)
}
