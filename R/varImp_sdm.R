#' Calculation of variable importance for models
#'
#' This function retrieves variable importance as a function of ROC curves to each predictor.
#'
#' @param m A models object
#' @param id model ids to filter varImp calculation
#' @param ... Parameters passing to caret::varImp()
#'
#' @return A occurrences object with pseudoabsence data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import caret
#'
#' @export
varImp_sdm <- function(m, id=NULL , ...){
  if(class(m)=='input_sdm'){
    y <- m$models
  } else {
    y <- m
  }
  if(!is.null(id)){y$models <- lapply(y$models, function(x){x[names(x) %in% id]})}
  s <- sapply(y$models, function(x){
    l <- sapply(x, function(z){
      vi <- caret::varImp(z, ...)$importance
      if('Overall' %in% colnames(vi)){
        vi <- select(vi, 'Overall')
      } else if ('presence' %in% colnames(vi)){
        vi <- select(vi, 'presence')
      } else {
        warning(print(paste0('col not detected.')))
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
    suppressMessages(l <- bind_cols(l))
    df <- data.frame(mean = apply(l, 1, mean), sd= apply(l, 1, sd))
    }, simplify = FALSE, USE.NAMES = TRUE)
  return(s)
}

