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
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- vif_predictors(i)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method="bioclim", variables_selected = "vif")
#'
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "repeatedcv",
#'                                 number = 4,
#'                                 repeats = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("naive_bayes", "kknn"), variables_selected = "vif", ctrl=ctrl_sdm) |>
#' suppressWarnings()
#'
#' # Variable importance:
#' varImp_sdm(i)
#'
#' @importFrom caret varImp
#' @importFrom dplyr select bind_cols
#' @importFrom stats sd
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
    df <- data.frame(mean = apply(l, 1, mean), sd = apply(l, 1, stats::sd))
  }, simplify = FALSE, USE.NAMES = TRUE)
  return(s)
}
