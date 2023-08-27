#' Indlude pseudoabsence data in a occurrences object
#'
#' This function includes pseudoabsences in a given occurrences object given a set of predictors
#'
#' @param occ A occurrences object
#' @param pred A predictors object
#' @param method Method to create pseudoabsences
#' @param n_set Number of sets of pseudoabsence to create
#' @param n_pa Number of pseudoabsences to be generated in each dataset
#'
#' @return A occurrences object with pseudoabsence data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#'
#' @export
pseudoabsences <- function(occ, pred, method='random', n_set=10, n_pa=NULL){
  if(is.null(n_pa)){n_pa <- nrow(occ$n_presences)}
  if(method=="random"){
    l <- list()
    for(i in 1:n_set){
      l[[i]] <- pred$df[sample(nrow(pred$df), size=n_pa),]
    }
    pa <- .pseudoabsence(occ,l)
    return(pa)
  }
}

.pseudoabsence <- function(occ, l){
  occ$pseudoabsences <- l
  return(occ)
}
