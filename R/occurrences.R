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

#' @export
occurrences.data.frame <- function(x, ...){
  occ <- .occurrences(x, ...)
  return(occ)
}

#' @export
occurrences.tibble <- function(x, ...){
  x <- as.data.frame(x)
  occ <- .occurrences(x, ...)
  return(occ)
}

#' @export
.occurrences <- function(x, ...){
  col_names <- find_columns(x, ...)
  x <- x[,col_names]
  if(length(col_names)==2){ x <- cbind(rep('Sp_unknown',nrow(x)),x) }
  spp_names <- unique(x[,1])
  occ <- structure(list(occurrences=x,
                        spp_names=spp_names,
                        n_presences=table(x[,1]),
                        pseudoabsences=NULL,
                        background=NULL), class = "occurrences")
  return(occ)
}

#' Print method for occurrences
#' @export
print.occurrences <- function(x) {
  cat("Occurrences Object:\n")
  cat("Species Names:", x$spp_names, "\n")
  cat("Number of presences:", table(x$occurrences[,1]), "\n")
  if(!is.null(x$pseudoabsences)){cat("Pseudoabsence sets:", length(x$pseudoabsences), "\n")}
  if(!is.null(x$background)){cat("Background sets:", length(x$background), "\n")}
  cat("\nData:\n")
  print(head(x$occurrences))
}
