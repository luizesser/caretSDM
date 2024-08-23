#ctrl <- caret::trainControl(
#  method = "repeatedcv", number = 4, repeats = 1, classProbs = TRUE, returnResamp = "all", # retornar folds
#  summaryFunction = twoClassSummary, savePredictions = "all", allowParallel = FALSE
#)

#data <- p$models$models$`Araucaria angustifolia`$m1.1$pred[,c('pred', 'obs')]
#lev <- c("obs", "pred")
#model <- p$models$models$`Araucaria angustifolia`$m1.1

# ... arguments to pass to pROC::roc(). Main purpose is to calculate partial.auc.


my_classification_summary <- function (data, lev = NULL, model = NULL, custom_fun=NULL, ...) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The summary function isn't appropriate."))
  }
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  rocObject <- try(pROC::roc(data$obs,
                             as.ordered(data$pred),
                             quiet = TRUE, ...),
                   silent = TRUE)
  rocAUC <- ifelse(inherits(rocObject, "try-error"), NA, rocObject$auc)

  cm = confusionMatrix(data$pred, data$obs)
  mets <- cm$overall |> round(3)
  mets_class <- cm$byClass |> round(3)
  sens = sensitivity(data[, "pred"], data[, "obs"], lev[1])
  spec = specificity(data[, "pred"], data[, "obs"], lev[2])
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
