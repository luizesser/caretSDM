#' Calculates performance across resamples
#'
#' This function is used in \code{caret::trainControl(summaryFunction=summary_sdm)} to calculate
#' performance metrics across resamples.
#'
#' @usage
#' summary_sdm(data, lev = NULL, model = NULL)
#'
#' @param data A \code{data.frame} with observed and predicted values.
#' @param lev A \code{character} vector of factors levels for the response.
#' @param model Models names taken from \code{train} object.
#'
#' @returns A \code{input_sdm} or a \code{predictions} object.
#'
#' @details
#' See \code{?caret::defaultSummary} for more details and options to pass on
#' \code{caret::trainControl}.
#'
#' @seealso \code{\link{train_sdm} \link{caret::trainControl}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
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
#' ctrl_sdm <- caret::trainControl(method = "repeatedcv", number = 4, repeats = 10, classProbs = TRUE,
#' returnResamp = "all", summaryFunction = summary_sdm, savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("nnet", "kknn"), variables_selected = "vif", ctrl = ctrl_sdm)
#' i
#'
#' @importFrom caret confusionMatrix sensitivity specificity
#' @importFrom pROC roc
#'
#' @export
summary_sdm <- function (data, lev = NULL, model = NULL, custom_fun=NULL, ...) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The summary function isn't appropriate."))
  }
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  #rocObject <- try(pROC::roc(data$obs,
  #                           as.ordered(data$pred),
  #                           quiet = TRUE, ...),
  #                 silent = TRUE)
  rocObject <- try(pROC::roc(data$obs,
                             data[, lev[1]],
                             direction = ">",
                             quiet = TRUE),
                   silent = TRUE)
  rocAUC <- if(inherits(rocObject, "try-error")) NA else rocObject$auc

  cm = caret::confusionMatrix(data$pred, data$obs)
  mets <- cm$overall |> round(3)
  mets_class <- cm$byClass |> round(3)
  sens = caret::sensitivity(data[, "pred"], data[, "obs"], lev[1])
  spec = caret::specificity(data[, "pred"], data[, "obs"], lev[2])
  P = as.numeric(table(data$obs)[lev[1]])
  N = as.numeric(table(data$obs)[lev[2]])
  TP = cm$table[1,1]
  FP = cm$table[2,1]
  TN = cm$table[2,2]
  FN = cm$table[1,2]

  #if(!is.null(custom_fun)){
  #  custom_fun()
  #}

  out <- c(rocAUC, sens+spec-1, mets_class, mets, P, N, TP, FP, TN, FN)
  names(out) <- c("ROC", "TSS", names(mets_class), names(mets), "Positive", "Negative",
                  "True Positive", "False Positive", "True Negative", "False Negative")
  return(out)
}
