#' Ensemble Species Distribution Models
#'
#' Calculates ensemble predictions for species distribution models using custom or implemented
#' methods.
#'
#' @usage
#' ensemble_sdm(m,
#'             scen = NULL,
#'             method = "average",
#'             metric = NULL,
#'             fun = NULL
#'             )
#'
#' @param m A \code{input_sdm} or a \code{models} object.
#' @param scen A \code{scenarios} object or \code{NULL}. If \code{NULL} and \code{m} is a
#' \code{input_sdm} with a scenarios slot, it will be used.
#' @param method Character or a function. Which ensembles should be calculated? See details.
#' @param metric Character. Used with \code{method = "weighted_average"}: Which metric should
#' be used to weight predictions? If NULL
#' @param fun Function. If \code{method = "committee_average"}, the function will be used to binarize
#' the data. It will receive caret's train object and must return a numeric value (the threshold,
#' see details).
#'
#' @param i A \code{input_sdm} or a \code{predictions} object.
#' @param e1 A \code{ensembles} object.
#' @param e2 A \code{ensembles} object.
#'
#' @returns A \code{input_sdm} or a \code{predictions} object.
#'
#' @details
#' \code{ensembles} could be set to three different strategies OR a custom function.
#' The three implemented strategies are:
#' \code{average} is the mean occurrence probability, which is a simple mean of predictions;
#' \code{weighted_average} is the same average, but weighted by a metric, which needs to be
#' set using argument \code{metric} (see \link{mean_validation_metrics} for the metrics available).
#' \code{committee_average} is the committee average, as known as majority rule, where predictions
#' are binarized and then a mean is obtained. To binarize predictions, user can set a custom
#' function in the \code{fun} argument to calculate a threshold for each model. Standardly, the
#' committee average uses the \code{caret::thresholder} function to find the threshold that
#' maximizes the sum of sensitivity and specificity (through \code{caretSDM:::.MaxSeSp}).
#' Custom function (\code{fun}) must use the argument \code{mod}, which is the model output from
#' caret package (see \code{\link{get_models}}) and must return a \code{numeric} value (see example).
#' \code{method} can also be set to a custom function, which must receive the argument pred_mat,
#' which is a matrix of predictions (columns are models and rows are cells) and return a vector of
#' predictions (one value per cell). See the \code{median} example below for a custom function.
#'
#' \code{get_predictions} returns the \code{list} of all predictions to all scenarios, all species,
#' all algorithms and all repetitions. Useful for those who wish to implement their own ensemble
#' methods.
#'
#' \code{get_ensembles} returns a \code{matrix} of \code{data.frame}s, where each column is a
#' scenario and each row is a species.
#'
#' \code{scenarios_names} returns the scenarios names in a \code{sdm_area} or \code{input_sdm}
#' object.
#'
#' \code{get_scenarios_data} returns the data from scenarios in a \code{sdm_area} or
#' \code{input_sdm} object.
#'
#' @seealso \code{\link{sdm_area} \link{input_sdm} \link{mean_validation_metrics}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' if (interactive()) {
#'   # Create sdm_area object:
#'   set.seed(1)
#'   sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#'
#'   # Include predictors:
#'   sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#'
#'   # Include scenarios:
#'   sa <- add_scenarios(sa)
#'
#'   # Create occurrences:
#'   oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#'   # Create input_sdm:
#'   i <- input_sdm(oc, sa)
#'
#'   # Pseudoabsence generation:
#'   i <- pseudoabsences(i, method="random", n_set=2)
#'
#'   # Custom trainControl:
#'   ctrl_sdm <- caret::trainControl(method = "boot",
#'                                   number = 1,
#'                                   repeats = 1,
#'                                   classProbs = TRUE,
#'                                   returnResamp = "all",
#'                                   summaryFunction = summary_sdm,
#'                                   savePredictions = "all")
#'
#'   # Train models:
#'   i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
#'     suppressWarnings()
#'
#'   # Predict models:
#'   i  <- predict_sdm(i, th = 0.8)
#'
#'   # Ensemble:
#'   i <- ensemble_sdm(i, method = "average")
#'   i
#' }
#'
#' # Example from a custom function to obtain the threshold that maximizes
#' # the sensitivity plus specificity:
#' MaxSeSp <- function(mod) {
#'    th <- caret::thresholder(mod,
#'                             threshold = seq(0, 1, by = 0.001),
#'                             final = TRUE,
#'                             statistics = c("Sensitivity", "Specificity")
#'                             )
#'    th <- th$prob_threshold[which.max(th$Sensitivity + th$Specificity)]
#'    if (length(th) > 1) mean(th) else th
#' }
#'
#' # Example from a custom function to obtain ensembles using the median instead of the mean:
#' median_ensemble <- function(pred_mat) {
#'  apply(pred_mat, 1, median, na.rm = TRUE)
#' }
#'
#' @importFrom caret thresholder
#' @import checkCLI
#'
#' @export
ensemble_sdm <- function(m, scen = NULL, method = "average", metric = NULL, fun = NULL) {
  if (is_input_sdm(m)) {
    y <- m$models
    scen <- m$scenarios
  }
  assert_cli(assert_class_cli(method, "character"),
             assert_class_cli(method, "function"))
  if(is.character(method)) {
    assert_subset_cli(method, c("average", "weighted_average", "committee_average"))
  }
  assert_class_cli(fun, "function", null.ok=T)
  valid_metrics <- colnames(get_validation_metrics(m)[[1]])[-1]
  assert_choice_cli(metric, valid_metrics, null.ok=T)
  if(is.null(metric)){
    metric <- "ROC"
  }
  p <- get_predictions(m)

  if(is.character(method)) {
    method_name <- method
  } else {
    method_name <- deparse(substitute(method))
  }

  message("Ensemble function: ", method_name)

  if("committee_average" %in% method) {
    if (is.null(fun)) {
      MaxSeSp <- function(mod) {
        th <- caret::thresholder(
          mod,
          threshold = seq(0, 1, by = 0.001),
          final = TRUE,
          statistics = c("Sensitivity", "Specificity")
        )
        th <- th$prob_threshold[which.max(th$Sensitivity + th$Specificity)] # max(sens+spec)
        if (length(th) > 1) mean(th) else th
      }
      fun <- MaxSeSp
      message ("Binarization Function: Maximum Sensitivity plus Specificity (.MaxSeSp)")
    } else {
      #perform checks on the custom function
      fun_name <- deparse(substitute(fun))
      assert_function_cli(fun, args = "mod")
      message (paste0("Binarization Function: ", fun_name))
    }
    bin_thresholds <- lapply(names(y$models),
                             function(sp) {
                               vapply(y$models[[sp]], fun, numeric(1))
                             })
    names(bin_thresholds) <- names(y$models)
  }

  e <- lapply(names(p), function(scn) {
    message("  ", scn)
    p_scn <- p[[scn]]

    e1 <- lapply(names(p_scn), function(sp) {

      x <- p_scn[[sp]]
      if (length(x) == 0) return(NULL)

      pred_mat <- do.call(
        cbind,
        lapply(x, `[[`, "presence")
      )

      pred_mat <- as.matrix(pred_mat)
      storage.mode(pred_mat) <- "double" # this accelerates calculations, since makes functions like

      l <- list()

      if(is.function(method)){
        assert_function_cli(method, args = "pred_mat")
        ens <- method(pred_mat)
        assert_numeric_cli(ens, len = nrow(pred_mat))
        assert_vector_cli(ens, len = nrow(pred_mat))
        l[[method_name]] <- ens
      } else {
        if("average" %in% method){
          ens <- rowMeans(pred_mat, na.rm = TRUE)
          l[["average"]] <- ens
        }
        if("weighted_average" %in% method){
          w <- get_validation_metrics(m)[[sp]][,metric]
          ens <- rowSums(pred_mat * w[col(pred_mat)], na.rm = TRUE) / sum(w, na.rm = TRUE)
          l[["weighted_average"]] <- ens
        }
        if("committee_average" %in% method){
          th_bin <- bin_thresholds[[sp]]
          ens <- rowMeans(pred_mat > th_bin[col(pred_mat)])
          l[["committee_average"]] <- ens
        }
      }

      res <- data.frame(
        cell_id = x[[1]]$cell_id,
        do.call(cbind, l)
      )

      colnames(res) <- c("cell_id", method_name)
      return(res)
    })
    names(e1) <- names(p_scn)
    return(e1)
  })
  names(e) <- names(p)
  scenarios <- names(e)
  species   <- names(e[[1]])

  e0 <- matrix(
    vector("list", length(species) * length(scenarios)),
    nrow = length(species),
    ncol = length(scenarios),
    dimnames = list(species, scenarios)
  )

  for (scn in scenarios) {
    for (sp in species) {
      e0[sp, scn][[1]] <- e[[scn]][[sp]]
    }
  }

  #####################################################################

  e2 <- list(
    method = method_name,
    data = e0
  )

  ens <- .ensembles(e2)
  if (is_input_sdm(m)) {
    m$ensembles <- ens
    ens <- m
  }
  return(ens)
}

#' @rdname ensemble_sdm
#' @export
get_ensembles <- function(i) {
  if (is_input_sdm(i)) {
    return(i$ensembles$data)
  }
  return(NULL)
}

#' @rdname ensemble_sdm
#' @export
add_ensembles <- function(e1, e2) {
  assert_cli(check_class_cli(e1, "ensembles", null.ok = TRUE),
             check_class_cli(e1, "input_sdm", null.ok = TRUE))
  assert_class_cli(e2, "ensembles", null.ok = TRUE)
  if(is.null(e1)) {return(e2)}
  if(is.null(e2)) {return(e1)}
  if(is_input_sdm(e1)){
    res <- e1
    e1 <- e1$ensembles
  } else {
    res <- NULL
  }

  m1 <- e1$data
  m2 <- e2$data
  # union of species and scenarios
  species  <- union(rownames(m1), rownames(m2))
  scenarios <- union(colnames(m1), colnames(m2))

  # initialize output
  out <- matrix(
    vector("list", length(species) * length(scenarios)),
    nrow = length(species),
    ncol = length(scenarios),
    dimnames = list(species, scenarios)
  )

  # helper to safely extract cells
  get_cell <- function(m, sp, sc) {
    if (!is.null(m) && sp %in% rownames(m) && sc %in% colnames(m)) {
      return(m[sp, sc][[1]])
    }
    return(NULL)
  }

  # loop over all cells
  for (sp in species) {
    for (sc in scenarios) {

      df1 <- get_cell(m1, sp, sc)
      df2 <- get_cell(m2, sp, sc)

      # cases
      if (is.null(df1) && is.null(df2)) {
        out[sp, sc][[1]] <- NULL

      } else if (is.null(df1)) {
        out[sp, sc][[1]] <- df2

      } else if (is.null(df2)) {
        out[sp, sc][[1]] <- df1

      } else {
        # merge by cell_id
        out[sp, sc][[1]] <- merge(df1, df2, by = "cell_id", all = TRUE)
      }
    }
  }

  e <- list(
    method=c(e1$method, e2$method),
    data=out
  )
  ens <- .ensembles(e)
  if(!is.null(res)){
    res$ensembles <- ens
    ens <- res
  }
  return(ens)
}


.MaxSeSp <- function(mod) {
    th <- caret::thresholder(
      mod,
      threshold = seq(0, 1, by = 0.001),
      final = TRUE,
      statistics = c("Sensitivity", "Specificity")
    )
    th <- th$prob_threshold[which.max(th$Sensitivity + th$Specificity)] # max(sens+spec)
    if (length(th) > 1) mean(th) else th
}

.ensembles <- function(x) {
  ensembles <- structure(
    list(
      method = x$method,
      data = x$data
    ),
    class = "ensembles"
  )
  return(ensembles)
}

#' Print method for ensembles
#' @param x ensembles object
#' @param ... passed to other methods
#' @returns Concatenate structured characters to showcase what is stored in the object.
#' @exportS3Method base::print
print.ensembles <- function(x, ...) {
  cat("         caretSDM        \n")
  cat(".........................\n")
  cat("Class             : Ensembles\n")
  cat("        Methods   :", x$method, "\n")
  invisible(x)
}
