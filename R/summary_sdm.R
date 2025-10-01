#' Calculates performance across resamples
#'
#' This function is used in \code{caret::trainControl(summaryFunction=summary_sdm)} to calculate
#' performance metrics across resamples.
#'
#' @usage
#' summary_sdm(data, lev = NULL, model = NULL, custom_fun=NULL)
#'
#' @param data A \code{data.frame} with observed and predicted values.
#' @param lev A \code{character} vector of factors levels for the response.
#' @param model Models names taken from \code{train} object.
#' @param custom_fun A custom function to be applied in models (not yet implemented).
#' @param data_independent independent data.frame to calculate metrics.
#' @param obs_col_name The name of the column with observed values.
#' @param threshold Threshold for presence-only models.
#'
#'
#' @returns A \code{input_sdm} or a \code{predictions} object.
#'
#' @details
#' See \code{?caret::defaultSummary} for more details and options to pass on
#' \code{caret::trainControl}.
#'
#' @seealso \code{\link{train_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples

#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method = "random")
#'
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "repeatedcv",
#'                                 number = 2,
#'                                 repeats = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
#' suppressWarnings()
#'
#' @importFrom caret confusionMatrix sensitivity specificity
#' @importFrom pROC roc auc
#' @importFrom stats quantile
#' @importFrom ecospat ecospat.boyce
#'
#' @export
summary_sdm <- function(data, lev = NULL, model = NULL, custom_fun = NULL) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The summary function isn't appropriate."))
  }
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }

  # ROC and AUC
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]],
                             direction = ">", quiet = TRUE), silent = TRUE)
  rocAUC <- if(inherits(rocObject, "try-error")) NA else rocObject$auc

  # Confusion matrix metrics
  cm <- caret::confusionMatrix(data$pred, data$obs)
  mets <- cm$overall |> round(3)
  mets_class <- cm$byClass |> round(3)

  TP <- cm$table[1, 1]
  FP <- cm$table[2, 1]
  TN <- cm$table[2, 2]
  FN <- cm$table[1, 2]

  # Convert observations to numeric (1 for presence/first level, 0 for absence/second level)
  obs_num <- ifelse(data$obs == lev[1], 1, 0)
  pred_prob <- data[, lev[1]]

  # Continuous Boyce Index using ecospat
  cbi <- tryCatch({
    # ecospat.boyce expects: fit (predicted values for all data),
    # obs (predicted values for presences only)
    presence_idx <- obs_num == 1
    boyce_result <- ecospat::ecospat.boyce(
      fit = pred_prob,
      obs = pred_prob[presence_idx],
      nclass = 0,  # Use quantiles
      window.w = "default",
      res = 100,
      PEplot = FALSE
    )
    round(boyce_result$cor, 3)
  }, error = function(e) {
    NA
  })

  # Partial ROC using pROC package
  partial_roc <- tryCatch({
    # Calculate partial AUC from 100% to 90% specificity (or 0 to 10% FPR)
    # This is equivalent to allowing 10% omission error
    roc_obj <- pROC::roc(obs_num, pred_prob, direction = ">", quiet = TRUE)

    # Partial AUC with specificity from 0.9 to 1.0
    p_auc <- pROC::auc(roc_obj, partial.auc = c(1, 0.9),
                       partial.auc.focus = "specificity",
                       partial.auc.correct = TRUE)

    round(as.numeric(p_auc), 3)
  }, error = function(e) {
    NA
  })

  # Omission rate at 10th percentile threshold
  omission_10 <- tryCatch({
    presence_probs <- pred_prob[obs_num == 1]
    threshold <- stats::quantile(presence_probs, probs = 0.10, na.rm = TRUE)
    omitted <- sum(presence_probs < threshold, na.rm = TRUE)
    total_presences <- sum(obs_num == 1)
    round(omitted / total_presences, 3)
  }, error = function(e) {
    NA
  })

  if(length(lev) > 1) {
    sens <- caret::sensitivity(data[, "pred"], data[, "obs"], lev[1])
    spec <- caret::specificity(data[, "pred"], data[, "obs"], lev[2])
    P <- as.numeric(table(data$obs)[lev[1]])
    N <- as.numeric(table(data$obs)[lev[2]])

    out <- c(rocAUC, sens + spec - 1, mets_class, mets,
             P, N, TP, FP, TN, FN, cbi, partial_roc, omission_10)
    names(out) <- c("ROC", "TSS", names(mets_class), names(mets), "Positive", "Negative",
                    "True Positive", "False Positive", "True Negative", "False Negative", "CBI", "pAUC", "Omission_10pct")
  } else {
    out <- c(rocAUC, mets_class, mets,
             TP, FP, TN, FN, cbi, partial_roc, omission_10)
    names(out) <- c("ROC",names(mets_class), names(mets),
                    "True Positive", "False Positive", "True Negative", "False Negative", "CBI", "pAUC", "Omission_10pct")
  }

  return(out)
}




#' @rdname summary_sdm
#' @export
summary_sdm_presence_only <- function(data, lev, threshold) {

  # Ensure there's only one observed class
  if (length(unique(data$obs)) > 1) {
    warning("This function is intended for presence-only data, but more than one class was found.")
  }

  # Get the column name for the presence class probability (assuming it's the second level)
  presence_class <- lev[1]
  presence_probs <- data[[presence_class]]

  # 1. Mean Prediction Value
  mean_pred_val <- mean(presence_probs, na.rm = TRUE)

  # 2. Omission Rate
  # Count how many presence points were predicted below the threshold
  omitted_count <- sum(presence_probs < threshold)
  total_count <- length(presence_probs)
  omission_rate <- omitted_count / total_count

  # Prepare the output
  out <- c(mean_pred_val, omission_rate, threshold, total_count)
  names(out) <- c("Mean_Prediction_Value", "Omission_Rate", "Threshold_Used", "N_Presences")

  return(round(out, 3))
}

#' @rdname summary_sdm
#' @export
validate_on_independent_data <- function(model, data_independent, obs_col_name) {
  # Extract the true observed outcomes
  obs <- data_independent[[obs_col_name]]
  levels(obs) <- c("presence", "pseudoabsence")
  # Get the levels from the observed data
  lev <- levels(obs)

  # Create the prediction data frame
  pred_df <- data.frame(obs = obs)

  # Add class predictions
  pred_df$pred <- predict(model, newdata = data_independent, type = "raw")

  # Add class probabilities and name the columns correctly
  prob_preds <- predict(model, newdata = data_independent, type = "prob")
  pred_df <- cbind(pred_df, prob_preds)

  # The MTP threshold is the minimum of these probabilities
  #mtp_threshold <- min(train_pred_probs[[presence_level]])

  # --- This is the crucial part ---
  # 'pred_df' now mimics the structure that caret passes to summaryFunction
  # It has columns: 'obs', 'pred', 'Absence', 'Presence' (or your level names)

  # Finally, call your summary function on this formatted data
  metrics1 <- summary_sdm_presence_only(data = pred_df, lev = lev, threshold = 0)
  metrics2 <- summary_sdm(data = pred_df, lev = lev)
  metrics = c(metrics1, metrics2)
  return(metrics)
}
