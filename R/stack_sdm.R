#' Train a Stacked Ensemble for SDM
#'
#' This function builds a meta-model (Layer 2) using the out-of-fold predictions
#' from models trained in Layer 1.
#'
#' @param m A \code{models} or \code{input_sdm} object.
#' @param meta_algo A character string specifying the algorithm for the meta-learner.
#' @param ctrl A \code{trainControl} object for the meta-learner. If NULL, a simple CV is used.
#' @param ... Additional arguments passed to \code{caret::train}.

#' @seealso \code{\link{input_sdm} \link{sdm_area} \link{algorithms} \link{train_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 100000, output_crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, occ_crs = 6933)
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
#' i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl_sdm) |>
#' suppressWarnings()
#'
#' # Train stacked ensemble:
#' i <- stack_sdm(i, meta_algo = "nnet", ctrl = ctrl_sdm)
#'
#' @return A \code{stacked_models} object.
#' @export
stack_sdm <- function(m, meta_algo = "glm", ctrl = NULL, ...) {
  # Handle input_sdm objects
  if (is_input_sdm(m)) {
    models_obj <- m$models
  } else {
    models_obj <- m
  }

  if (!inherits(models_obj, "models")) {
    cli::cli_abort("Input must be a 'models' or 'input_sdm' object.")
  }

  # Ensure models have saved predictions
  # Each model must have been trained with savePredictions = "final" or "all"
  all_spp <- names(models_obj$models)

  stack_results <- lapply(all_spp, function(sp) {
    mods <- models_obj$models[[sp]]

    # 1. Extract OOF predictions from all base models
    oof_list <- lapply(names(mods), function(mod_name) {
      fit <- mods[[mod_name]]
      if (is.null(fit$pred)) {
        cli::cli_abort(paste("Model", mod_name, "for species", sp,
                             "does not contain OOF predictions. Re-run train_sdm with savePredictions='final'."))
      }

      # Select the presence column and reorder by rowIndex to align with original data
      # We use the 'bestTune' rows only
      p <- fit$pred
      tuning_cols <- names(fit$bestTune)
      for(col in tuning_cols) {
        p <- p[p[[col]] == fit$bestTune[[col]], ]
      }

      # Prepare a clean dataframe with predictions
      res <- p[, c("rowIndex", "presence", "obs")]
      names(res)[2] <- mod_name # Rename 'presence' to the model name
      return(res)
    })

    # 2. Join all predictions into a single training frame for Layer 2
    # Using Reduce to merge all dataframes by rowIndex and obs
    #level2_data <- Reduce(function(x, y) merge(x, y, by = c("rowIndex", "obs")), oof_list)

    # Remove rowIndex for training
    #train_x <- level2_data[, !names(level2_data) %in% c("rowIndex", "obs")]
    #train_y <- level2_data$obs

    # 2. Join all predictions into a single training frame for Layer 2
    # Ensure all components are sorted identically by rowIndex
    oof_list_sorted <- lapply(oof_list, function(df) df[order(df$rowIndex), ])

    # Extract the target variable from the first model
    train_y <- oof_list_sorted[[1]]$obs

    # Extract only the prediction columns and bind them column-wise
    pred_cols <- lapply(oof_list_sorted, function(df) df[, 2, drop = FALSE])
    train_x <- do.call(cbind, pred_cols)

    # 3. Train the Meta-Learner
    if (is.null(ctrl)) {
      ctrl <- caret::trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = summary_sdm)
    }

    message(paste("Training meta-learner for:", sp))
    meta_fit <- caret::train(
      x = train_x,
      y = train_y,
      method = meta_algo,
      trControl = ctrl
    )

    return(meta_fit)
  })

  names(stack_results) <- all_spp

  # Create the output structure
  out <- list(
    meta_models = stack_results,
    base_models = models_obj,
    meta_algo = meta_algo
  )

  class(out) <- "stacked_models"

  if (is_input_sdm(m)) {
    m$stacked_models <- out
    return(m)
  }

  return(out)
}
