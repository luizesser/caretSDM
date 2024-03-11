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
occurrences.data.frame <- function(x, independent_test=NULL, ...){
  occ <- .occurrences(x, independent_test, ...)
  return(occ)
}

#' @export
occurrences.tibble <- function(x, ...){
  x <- as.data.frame(x)
  occ <- .occurrences(x, ...)
  return(occ)
}

#' @export
occurrences.sf <- function(x, independent_test=NULL, ...){
  x <- cbind(select(as.data.frame(x), -'geometry'),st_coordinates(x))
  occ <- .occurrences(x, independent_test, ...)
  return(occ)
}

#' @export
.occurrences <- function(x, independent_test=NULL, epsg=NULL, ...){
  col_names <- find_columns(x, ...)
  x <- x[,col_names]
  if(length(col_names)==2){ x <- cbind(rep('Sp_unknown',nrow(x)),x) }
  spp_names <- unique(x[,1])
  if(is.null(epsg)){epsg <- 4326}
  if(!is.null(independent_test)){
    if(isTRUE(independent_test)){
      occ_data <- st_as_sf(x, coords=col_names[c(2,3)])
      n <- as.vector(caret::createDataPartition(x[,col_names[1]], list=F, p=0.1))
      x <- occ_data[-n,]
      indep_test_data <- occ_data[n,]
      occ <- structure(list(occurrences=x,
                            spp_names=spp_names,
                            n_presences=table(x[,1]),
                            independent_test=indep_test_data,
                            epsg=epsg), class = "occurrences")
    } else {
      independent_test <- as.data.frame(independent_test)
      col_names <- find_columns(independent_test, ...)
      independent_test <- independent_test[,col_names]
      if(length(col_names)==2){ independent_test <- cbind(rep('Sp_unknown',nrow(independent_test)),independent_test) }
      #spp_names2 <- unique(independent_test[,1]) ### Multiple Species?
      occ <- structure(list(occurrences=x,
                            spp_names=spp_names,
                            n_presences=table(x[,1]),
                            independent_test=independent_test,
                            epsg=epsg), class = "occurrences")
    }
  } else {
    occ <- structure(list(occurrences=x,
                          spp_names=spp_names,
                          n_presences=table(x[,1]),
                          epsg=epsg), class = "occurrences")
  }

  return(occ)
}

#' Print method for occurrences
#' @exportS3Method base::print
print.occurrences <- function(x) {
  cat("        caretSDM       \n")
  cat(".......................\n")
  cat("Class                 : Occurrences\n")
  cat("Species Names         :", x$spp_names, "\n")
  cat("Number of presences   :", x$n_presences, "\n")
  if(!is.null(x$pseudoabsences)){cat("Pseudoabsence methods :\n",
                                     "        Method to obtain PAs       :", x$pseudoabsences$method, "\n",
                                     "        Number of PA sets          :", x$pseudoabsences$n_set, "\n",
                                     "        Number of PAs in each set  :", x$pseudoabsences$n_pa, "\n" )}
  if(!is.null(x$background)){cat("Background sets       :", length(x$background), "\n")}
  if(!is.null(x$independent_test)){cat("Independent Test      : TRUE (number of records = ",nrow(x$independent_test),")\n")}
  if(!is.null(x$data_cleaning)){cat(cat("Data Cleaning         : "), cat(x$data_cleaning, sep=', '), "\n")}
  n <- max(nchar(colnames(x$occurrences)[1]),nchar(x$occurrences[,1])[1])
  n <- n+sum(nchar(colnames(x$occurrences)[-1]))+ncol(x$occurrences)+1
  cat(rep('=', n), sep='')
  cat("\nData:\n")
  print(head(x$occurrences))
}
