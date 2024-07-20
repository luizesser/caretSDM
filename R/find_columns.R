#' @keywords internal
#' Find species, longitude and latitudecolumns in a data.frame.
#' @param data data.frame to search for.
#' @return Vector of column names that correspond to the species, longitude and latitude columns.
#' @import checkmate
#' @export
find_columns <- function(df, col_names = NULL, spp = TRUE) {
  assertDataFrame(df)
  if (!is.null(col_names)) {
    assertCharacter(col_names, min.len = 2, max.len = 3)
  }

  if (is.null(col_names)) {
    col_names <- colnames(df)

    lon_keywords <- c("lon", "x")
    lat_keywords <- c("lat", "y")

    lon_col <- colnames(df)[grep(paste(lon_keywords, collapse = "|"), tolower(colnames(df)))]
    lat_col <- colnames(df)[grep(paste(lat_keywords, collapse = "|"), tolower(colnames(df)))]

    if(length(c(lat_col,lon_col))<2){
      stop("A latitude or longitude column is missing on df.")
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
      stop("Given col_names are not valid column names for df.")
    }
  }
  return(col_names)
}
