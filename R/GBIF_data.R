#' Retrieve Species data from GBIF
#'
#' This function is a wrapper to get records from GBIF using \code{rgbif} and return a
#' \code{data.frame} ready to be used in caretSDM.
#'
#' @usage GBIF_data(s, file = "", as_df = FALSE, ...)
#'
#' @param s \code{character} vector of species names.
#' @param file File to save the output.
#' @param as_df Should the output be a \code{dataframe}? Default is \code{FALSE}, returning a
#' \code{occurrences} object.
#' @param ... Arguments to pass on \code{rgbif::occ_data()}.
#'
#' @references https://www.gbif.org
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Select species names:
#' s <- c("Araucaria angustifolia", "Paubrasilia echinata", "Eugenia uniflora")
#'
#' # Run function:
#' oc <- GBIF_data(s)
#'
#' @importFrom rgbif occ_data
#' @importFrom dplyr bind_rows
#'
#' @export
GBIF_data <- function(s, file = "", as_df = FALSE, ...) {
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

  if (!file.exists(file)) {
    data <- lapply(s, function(x) {
      y <- rgbif::occ_data(scientificName = x, limit = 100000, hasCoordinate = T, ...)
      if ("decimalLatitude" %in% names(y$data)) {
        y <- y$data[, c("species", "decimalLongitude", "decimalLatitude")]
        return(y)
      } else {
        print(paste0("Species with zero records found: ", s[ids %in% x]))
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
    data <- na.omit(data)

    if (!file == "") {
      if (grepl("/", file)) {
        dir.create(paste(head(unlist(strsplit(file, "/")), -1), collapse = "/"), recursive = T)
      }
      write.csv(data, file, row.names = FALSE)
    }
  } else {
    print(paste0("File already exists. Importing from: ", file))
    data <- read.csv(file)
  }

  if(!as_df){
    data <- occurrences_sdm(data, crs = 4326)
  }

  return(data)
}
