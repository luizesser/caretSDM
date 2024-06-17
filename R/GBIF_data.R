#' Retrieve Species data from GBIF
#'
#' This function is a wrapper to get records from GBIF using \code{rgbif} and return a
#' \code{data.frame} ready to be used in caretSDM.
#'
#' @usage GBIF_data(s)
#'
#' @param s \code{character} vector of species names.
#' @param file File to save the output.
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
#' data <- GBIF_data(s)
#'
#' @import rgbif
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#'
#' @export
GBIF_data <- function(s, file = "", ...) {
  if (!file.exists(file)) {
    data <- lapply(s, function(x) {
      y <- occ_data(scientificName = x, limit = 100000, hasCoordinate = T, ...)
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

    data <- bind_rows(data)
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
  return(data)
}
