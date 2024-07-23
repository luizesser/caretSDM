#' Calculation of variable importance for models
#'
#' This function retrieves variable importance as a function of ROC curves to each predictor.
#'
#' @usage varImp_sdm(m, id = NULL, ...)
#'
#' @param m A \code{models} or \code{input_sdm} object.
#' @param id Vector of model ids to filter varImp calculation.
#' @param ... Parameters passing to caret::varImp().
#'
#' @returns A \code{data.frame} with variable importance data.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom caret varImp
#' @importFrom dplyr select bind_cols
#'
#' @export
varImp_sdm <- function(m, id = NULL, ...) {
  if (is_input_sdm(m)) {
    y <- m$models
  } else {
    y <- m
  }
  if (!is.null(id)) {
    y$models <- lapply(y$models, function(x) {
      x[names(x) %in% id]
    })
  }
  s <- sapply(y$models, function(x) {
    l <- sapply(x, function(z) {
      vi <- caret::varImp(z, ...)$importance
      if ("Overall" %in% colnames(vi)) {
        vi <- dplyr::select(vi, "Overall")
      } else if ("presence" %in% colnames(vi)) {
        vi <- dplyr::select(vi, "presence")
      } else {
        warning(print(paste0("col not detected.")))
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
    suppressMessages(l <- dplyr::bind_cols(l))
    df <- data.frame(mean = apply(l, 1, mean), sd = apply(l, 1, sd))
  }, simplify = FALSE, USE.NAMES = TRUE)
  return(s)
}
