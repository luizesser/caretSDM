#' Retrieve Species data from GBIF
#'
#' This function is a wrapper to get records from GBIF using \code{rgbif} and return a
#' \code{data.frame} ready to be used in caretSDM.
#'
#' @usage GBIF_data(s, file = NULL, as_df = FALSE, ...)
#'
#' @param s \code{character} vector of species names.
#' @param file \code{character} with file to save the output. If not informed, data will not be
#' saved on folder.
#' @param as_df Should the output be a \code{dataframe}? Default is \code{FALSE}, returning a
#' \code{occurrences} object.
#' @param ... Arguments to pass on \code{rgbif::occ_data()}.
#'
#' @returns A \code{data.frame} with species occurrences data, or an \code{occurrences} object if
#' \code{as_df = FALSE}.
#'
#' @references https://www.gbif.org
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' \donttest{
#' # Select species names:
#' s <- c("Araucaria angustifolia", "Salminus brasiliensis")
#'
#' # Run function:
#' oc <- GBIF_data(s)
#' }
#'
#' @importFrom rgbif occ_data
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom utils head write.csv read.csv
#'
#' @export
GBIF_data <- function(s, file = NULL, as_df = FALSE, ...) {
  assert_logical_cli(
    as_df,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  assert_vector_cli(
    s,
    any.missing = FALSE,
    all.missing = FALSE,
    min.len = 1,
    null.ok = FALSE,
    unique = TRUE
  )

  if(!is.null(file)){
    if(file.exists(file)){
      message(paste0("File already exists. Importing from: ", file))
      data <- utils::read.csv(file)
      if(!as_df){
        data <- occurrences_sdm(data, crs = 4326)
      }
      return(data)
    }
  }

  data <- lapply(s, function(x) {
    y <- rgbif::occ_data(scientificName = x, limit = 100000, hasCoordinate = TRUE, ...)
    if ("decimalLatitude" %in% names(y$data)) {
      y <- y$data[, c("species", "decimalLongitude", "decimalLatitude")]
      return(y)
    } else {
      print(paste0("Species with zero records found"))
      y <- NULL
      return(y)
    }
  })
  data <- lapply(data, function(x) {
    if (!is.null(x)) {
      x <- as.data.frame(x)
      s1 <- unique(x$species)
      x$species <- rep(gsub(" ", "_", s1), nrow(x))
      return(x)
    }
  })
  data <- dplyr::bind_rows(data)
  data <- stats::na.omit(data)
  if (!is.null(file)) {
    if (grepl("/", file)) {
      dir.create(paste(utils::head(unlist(strsplit(file, "/")), -1), collapse = "/"),
                 recursive = TRUE)
    }
    utils::write.csv(data, file, row.names = FALSE)
  }

  if(!as_df){
    data <- occurrences_sdm(data, crs = 4326)
  }

  return(data)
}
