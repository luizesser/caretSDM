#ctrl <- caret::trainControl(
#  method = "repeatedcv", number = 4, repeats = 1, classProbs = TRUE, returnResamp = "all", # retornar folds
#  summaryFunction = twoClassSummary, savePredictions = "all", allowParallel = FALSE
#)

#data <- i_sa$models$models$Colossoma.macropomum$m1.1$pred[,c('pred', 'obs')]

my_classification_summary <- function (data, lev = NULL, model = NULL) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The summary function isn't appropriate."))
  }
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  rocObject <- try(pROC::roc(data$obs,
                             as.ordered(data[, lev[1]]),
                             quiet = TRUE),
                   silent = TRUE)
  rocAUC <- if (inherits(rocObject, "try-error"))
    NA
  else rocObject$auc

  cm = confusionMatrix(data$pred, data$obs)
  mets <- cm$overall |> round(3)
  mets_class <- cm$byClass |> round(3)
  sens = sensitivity(data[, "pred"], data[, "obs"], lev[1])
  spec = specificity(data[, "pred"], data[, "obs"], lev[2])
  P = as.numeric(table(data$obs)[lev[1]])
  N = as.numeric(table(data$obs)[lev[2]])
  #TP =
  #FP =
  #TN =
  #FN =

  out <- c(rocAUC,
           sens+spec-1,
           mets_class,
           mets
           )
  names(out) <- c("ROC", "TSS", names(mets_class), names(mets))
  out
}
