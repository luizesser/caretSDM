#' Create a occurrences object
#'
#' This function creates a occurrences object
#'
#' @param x A data.frame
#' @param ... A vector with column names addressing the columns with species names, longitude and latitude.
#'
#' @return A occurrences object.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' df <- data.frame(spp_names = rep('Aa', 100), longitude = runif(100), decimalLatitude=runif(100))
#' occ <- occurrences(df)
#'
#' @import tibble
#' @importFrom here here
#'
#' @export
occurrences <- function(x, ...){
  UseMethod("occurrences")
}

occurrences.data.frame <- function(x, ...){
  occ <- .occurrences(x, ...)
  return(occ)
}

occurrences.tibble <- function(x, ...){
  x <- as.data.frame(x)
  occ <- .occurrences(x, ...)
  return(occ)
}

.occurrences <- function(x, ...){
  col_names <- find_columns(x, ...)
  x <- x[,col_names]
  occ <- structure(list(occurrences=x), class = "occurrences")
  return(occ)
}
