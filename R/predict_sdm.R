#' Predict SDM models in new data
#'
#' This function projects SDM models to new scenarios
#'
#' @usage
#' predict_sdm(m,
#'             scen = NULL,
#'             metric = "ROC",
#'             th = 0.9,
#'             tp = "prob",
#'             file = NULL,
#'             add.current = TRUE)
#'
#' @param m A \code{input_sdm} or a \code{models} object.
#' @param scen A \code{scenarios} object or \code{NULL}. If \code{NULL} and \code{m} is a
#' \code{input_sdm} with a scenarios slot, it will be used.
#' @param th Thresholds for metrics. Can be numeric or a function.
#' @param metric A character containing the metric in which the \code{th} will be
#' calculated/applied. Default is ROC. See \code{?mean_validation_metrics} for the metrics
#' available.
#' @param tp Type of output to be retrieved. See details.
#'
#' @param i A \code{input_sdm} or a \code{predictions} object.
#' @param file File to sabe predictions.
#' @param add.current If current scenario is not available, predictors will be used as the current
#' scenario.
#' @param p1 A \code{predictions} object.
#' @param p2 A \code{predictions} object.
#'
#' @returns A \code{input_sdm} or a \code{predictions} object.
#'
#' @details
#' \code{tp} is a parameter to be passed on caret to retrieve either the probabilities of classes
#' (tp="prob") or the raw output (tp="raw"), which could vary depending on the algorithm used, but
#' usually would be on of the classes (factor vector with presences and pseudoabsences).
#'
#' \code{get_predictions} returns the \code{list} of all predictions to all scenarios, all species,
#' all algorithms and all repetitions. Useful for those who wish to implement their own ensemble
#' methods.
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
#'   i
#' }
#' @importFrom dplyr bind_cols select mutate all_of filter contains bind_rows across row_number distinct left_join group_split
#' @importFrom stringdist stringdist
#' @importFrom gtools mixedsort
#' @importFrom caret thresholder
#' @importFrom stars st_dimensions
#' @importFrom sf st_centroid st_as_sf st_coordinates
#' @importFrom stats na.omit weighted.mean
#' @importFrom cli cli_abort
#' @import checkCLI
#'
#' @global algo .data Sensitivity Specificity scenario
#'
#' @export
predict_sdm <- function(m, scen = NULL, metric = "ROC", th = 0.9, tp = "prob", file = NULL, add.current = TRUE) {
  if (is_input_sdm(m) & is.null(scen)) {
    scen <- m$scenarios
  }
  UseMethod("predict_sdm", scen)
}

#' @export
predict_sdm.sdm_area <- function(m, scen, metric = "ROC", th = 0.9, tp = "prob", file = NULL, add.current = TRUE) {
  if (is_input_sdm(m)) {
    y <- m$models
    scen <- m$scenarios
  }
  assert_subset_cli(metric, colnames(dplyr::select(mean_validation_metrics(m)[[1]], -algo)))
  assert_subset_cli(tp, c("prob", "raw"))

  if (is.function(th)) {
    tm <- deparse(substitute(th))
    th1 <- sapply(names(y$models), function(sp) {
      th2 <- th(y$validation$metrics[[sp]][,metric])
      res1 <- dplyr::filter(mean_validation_metrics(m)[[sp]], .data[[metric]] > th2)
      if (nrow(res1)==0){
        cli::cli_abort("Threshold is too high. No models passing the threshold.")
      }
      res <- dplyr::filter(get_validation_metrics(m)[[sp]], algo %in% res1$algo)
      return(res)
    }, simplify = FALSE, USE.NAMES = TRUE)
    th <- sapply(names(y$models), function(sp) {
      th(y$validation$metrics[[sp]][,metric])
      }, simplify = TRUE, USE.NAMES = TRUE)
  } else {
    if (is.numeric(th)) {
      tm <- paste0("threshold")
      th1 <- sapply(names(y$models), function(sp) {
        res1 <- dplyr::filter(mean_validation_metrics(m)[[sp]], .data[[metric]] > th)
        if (nrow(res1)==0){
          cli::cli_abort("Threshold is too high. No models passing the threshold.")
        }
        res <- dplyr::filter(get_validation_metrics(m)[[sp]], algo %in% res1$algo)
        return(res)
      }, simplify = FALSE, USE.NAMES = TRUE)
    } else {
      assert_numeric_cli(th, lower = 0, upper = 1, len = 1)
    }
  }

  m1 <- sapply(names(y$models), function(sp) {
    subset(y$models[[sp]], names(y$models[[sp]]) %in% rownames(th1[[sp]]))
  }, simplify = FALSE, USE.NAMES = TRUE)

  env_list <- scen$data

  env_long <- lapply(names(env_list), function(sc) {
    df <- env_list[[sc]]
    df$scenario  <- sc
    df
  }) |>
    dplyr::bind_rows() |>
    na.omit()

  pred_cols <- names(env_long)[!names(env_long) %in% c("cell_id", "geometry", "scenario")]

  env_unique <- env_long |>
    dplyr::distinct(dplyr::across(dplyr::all_of(pred_cols))) |>
    dplyr::mutate(unique_id = dplyr::row_number())

  # Join unique_id back to the long table
  env_long <- env_long |>
    dplyr::left_join(env_unique, by = pred_cols)

  # Prepare data for prediction
  newdata_for_pred <- env_unique |> dplyr::select(dplyr::all_of(pred_cols))

  # Predict with your fitted model (e.g., a caret or caretSDM model)
  # Example with a single model:
  p <- lapply(m1, function(m2) {
    p1 <- predict(m2,
                  newdata = newdata_for_pred,
                  type = "prob")
    p0 <- lapply(p1, function(p2) {
      p2 <- cbind(p2, env_unique$unique_id)[,c("env_unique$unique_id", "presence", "pseudoabsence")]
      names(p2) <- c("unique_id", "presence", "pseudoabsence")
      env_long2 <- env_long |>
        dplyr::left_join(p2, by = "unique_id")
      pred_by_scenario <- env_long2 |>
        #dplyr::arrange(scenario, cell_id) |>
        dplyr::group_split(scenario)
      names(pred_by_scenario) <- sapply(pred_by_scenario, function(df) unique(df$scenario))
      pred_by_scenario <- sapply(pred_by_scenario, function(pbs) dplyr::select(pbs, -c("scenario", "unique_id")), simplify = FALSE)
      if ("current" %in% names(pred_by_scenario)) {
        pred_by_scenario <- pred_by_scenario[c("current", names(pred_by_scenario)[!names(pred_by_scenario) %in% c("current")])]
      }
      return(pred_by_scenario)
    })
    return(p0)
  })

  # get scenario names from the first element
  scenarios <- names(p[[1]][[1]])

  # rebuild the list
  p <- setNames(
    lapply(scenarios, function(scn) {
      lapply(p, function(sp) {
        lapply(sp, function(mod) mod[[scn]])
      })
    }),
    scenarios
  )

  p2 <- list(
    thresholds = list(values = th1, method = tm, criteria = th),
    predictions = p,
    file = file,
    grid = scen$grid
  )

  predic <- .predictions(p2)
  if (is_input_sdm(m)) {
    m$predictions <- predic
    predic <- m
  }
  return(predic)
}

#' @rdname predict_sdm
#' @export
get_predictions <- function(i) {
  if (is_input_sdm(i)) {
    return(i$predictions$predictions)
  }
  return(NULL)
}

#' @rdname predict_sdm
#' @export
add_predictions <- function(p1, p2) {
  assert_class_cli(p1, "predictions", null.ok = TRUE)
  assert_class_cli(p2, "predictions", null.ok = TRUE)
  if(is.null(p1)) {return(p2)}
  if(is.null(p2)) {return(p1)}
  grd <- rbind(p1$grid, p2$grid)
  grd$cell_id <- c(p1$grid$cell_id, max(p1$grid$cell_id)+p2$grid$cell_id)
  p <- list(thresholds = list(values = c(p1$thresholds$values, p2$thresholds$values),
                              method = unique(c(p1$thresholds$method, p2$thresholds$method)),
                              criteria = unique(c(p1$thresholds$criteria, p2$thresholds$criteria))),
            predictions = c(p1$predictions, p2$predictions),
            file = c(p1$file, p2$file),
            ensembles = rbind(p1$ensembles, p2$ensembles),
            grid = grd
  )
  pred <- .predictions(p)
  return(pred)
}

.predictions <- function(x) {
  predictions <- structure(
    list(
      thresholds = x$thresholds,
      predictions = x$predictions,
      file = x$file,
      ensembles = x$ensembles,
      grid = x$grid
    ),
    class = "predictions"
  )
  return(predictions)
}

#' Print method for predictions
#' @param x predictions object
#' @param ... passed to other methods
#' @returns Concatenate structured characters to showcase what is stored in the object.
#' @exportS3Method base::print
print.predictions <- function(x, ...) {
  cat("         caretSDM        \n")
  cat(".........................\n")
  cat("Class             : Predictions\n")
  cat(
    "Thresholds        :\n",
    "        Method   :", x$thresholds$method, "\n",
    "        Criteria :", x$thresholds$criteria, "\n",
    "        Metrics  :\n"
  )
  print(x$thresholds$values)
  invisible(x)
}
