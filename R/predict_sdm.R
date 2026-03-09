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
#'             ensembles = TRUE,
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
#' @param ensembles Boolean. Should ensembles be calculated? If \code{TRUE} a series of ensembles
#' are obtained. See details.
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
#' When \code{ensembles} is set to \code{TRUE}, three ensembles are currently implemented.
#' mean_occ_prob is the mean occurrence probability, which is a simple mean of predictions,
#' wmean_AUC is the same mean_occ_prob, but weighted by AUC, and committee_avg is the committee
#' average, as known as majority rule, where predictions are binarized and then a mean is obtained.
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
#'   i
#' }
#' @importFrom dplyr bind_cols select mutate all_of filter contains bind_rows across row_number distinct left_join group_split
#' @importFrom stringdist stringdist
#' @importFrom gtools mixedsort
#' @importFrom caret thresholder
#' @importFrom stars st_dimensions
#' @importFrom sf st_centroid st_as_sf st_coordinates
#' @importFrom stats na.omit weighted.mean
#' @import checkCLI
#'
#' @global algo .data Sensitivity Specificity scenario
#'
#' @export
predict_sdm <- function(m, scen = NULL, metric = "ROC", th = 0.9, tp = "prob", ensembles = TRUE, file = NULL, add.current = TRUE) {
  if (is_input_sdm(m) & is.null(scen)) {
    scen <- m$scenarios
  }
  UseMethod("predict_sdm", scen)
}

#' @export
predict_sdm.sdm_area <- function(m, scen, metric = "ROC", th = 0.9, tp = "prob", ensembles = TRUE, file = NULL, add.current = TRUE) {
  if (is_input_sdm(m)) {
    y <- m$models
    scen <- m$scenarios
  }
  assert_subset_cli(metric, colnames(dplyr::select(mean_validation_metrics(m)[[1]], -algo)))
  assert_subset_cli(tp, c("prob", "raw"))

  if (is.function(th)) {
    tm <- deparse(th)
    th1 <- sapply(names(y$models), function(sp) {
      th2 <- th(y$validation$metrics[[sp]][,metric])
      res1 <- dplyr::filter(mean_validation_metrics(m)[[sp]], .data[[metric]] > th2)
      if (nrow(res1)==0){
        cli_abort("Threshold is too high. No models passing the threshold.")
      }
      res <- dplyr::filter(get_validation_metrics(m)[[sp]], algo %in% res1$algo)

      return(res)
    }, simplify = FALSE, USE.NAMES = TRUE)
  } else {
    if (is.numeric(th)) {
      tm <- paste0("threshold")
      th1 <- sapply(names(y$models), function(sp) {
        res1 <- dplyr::filter(mean_validation_metrics(m)[[sp]], .data[[metric]] > th)
        if (nrow(res1)==0){
          cli_abort("Threshold is too high. No models passing the threshold.")
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


#  ####################### NEW REDUCED APPROACH #########################

  env_list <- scen$data

  env_long <- lapply(names(env_list), function(sc) {
    df <- env_list[[sc]]
    df$scenario  <- sc
    df
  }) |> dplyr::bind_rows()

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

#  ####################### OLD APPROACH  #######################
#
#  p <- list()
#
#
#  for (i in 1:length(scen$data)) {
#    print(paste0("Projecting: ", i, "/", length(scen$data)))
#    x <- scen$data[[i]] |>
#      as.data.frame() |>
#      stats::na.omit() |>
#      dplyr::select(dplyr::all_of(c(m$models$predictors, "cell_id")))
#    p[[i]] <- sapply(m1, function(m2) {
#      suppressWarnings(p2 <- predict(m2, newdata = x, type = tp))
#      cell_id <- scen$data[[i]] |>
#        stats::na.omit()
#      p2 <- sapply(p2, function(z) {
#        cbind(cell_id, stats::na.omit(z))
#      }, simplify = FALSE, USE.NAMES = TRUE)
#      return(p2)
#    }, simplify = FALSE, USE.NAMES = TRUE)
#  }
#
#  names(p) <- names(scen$data)
#
#  ########################## chatGPT Approach #########################
#
#  p <- vector("list", length(scen$data))
#  names(p) <- names(scen$data)
#
#  for (scn in names(scen$data)) {
#
#    df <- scen$data[[scn]]
#
#    # Drop geometry immediately
#    df_nogeo <- df[, !names(df) %in% c("geometry")]
#
#    pred_cols <- setdiff(names(df_nogeo), c("cell_id"))
#
#    # Deduplicate ONLY within scenario
#    df_unique <- df_nogeo[!duplicated(df_nogeo[, pred_cols]), pred_cols, drop = FALSE]
#
#    # Map original rows to unique rows
#    match_id <- match(
#      do.call(paste, df_nogeo[, pred_cols]),
#      do.call(paste, df_unique)
#    )
#
#    # Predict once per unique row
#    pred_sp <- lapply(m1, function(sp_models) {
#
#      lapply(sp_models, function(mod) {
#
#        pr <- predict(mod, newdata = df_unique, type = "prob")[, "presence"]
#
#        # Map back to full grid
#        pr_full <- pr[match_id]
#
#        data.frame(
#          cell_id = df$cell_id,
#          presence = pr_full
#        )
#      })
#    })
#
#    p[[scn]] <- pred_sp
#  }
#

  ####################### OLD ENSEMBLES APPROACH  #######################

  #if (ensembles) {
  #  print("Ensembling...")
  #  e <- sapply(names(p), function(y) {
  #    print(y)
  #    y <- p[[y]]
  #    e2 <- sapply(names(y), function(sp) {
  #      print(sp)
  #      x <- y[[sp]]
  #      if (length(x) > 0) {
  #        # Prepare data
  #        suppressMessages(df <- dplyr::bind_cols(x))
  #        df <- dplyr::select(df, dplyr::contains("presence"))
  #        # mean_occ_prob
  #        mean_occ_prob <- df |>
  #          as.data.frame() |>
  #          apply(2, function(x) {
  #            as.numeric(gsub(NaN, NA, x))
  #          }) |>
  #          rowMeans()
  #        # mean_occ_prob <- rowMeans(df)
  #        # wmean_AUC
  #        wmean_AUC <- apply(df, 1, function(x) {
  #          stats::weighted.mean(x, th1[[sp]]$ROC)
  #        })
  #        # Obtain Thresholds:
  #        suppressWarnings(th2 <- lapply(m1[[sp]], function(x) {
  #          caret::thresholder(x,
  #            threshold = seq(0, 1, by = 0.01),
  #            final = TRUE,
  #            statistics = "all"
  #          )
  #        }))
  #        th2 <- lapply(th2, function(x) {
  #          x <- x |> dplyr::mutate(th = Sensitivity + Specificity)
  #          th <- x[x$th == max(x$th), "prob_threshold"]
  #          if (length(th) > 1) {
  #            th <- mean(th)
  #          }
  #          return(th)
  #        })
  #        # binary
  #        for (i in 1:ncol(df)) {
  #          df[, i] <- ifelse(df[, i][] > th2[i], 1, 0)
  #        }
  #        committee_avg <- rowMeans(df)
  #
  #        # save everything
  #        df <- data.frame(cell_id = x[[1]]$cell_id, mean_occ_prob, wmean_AUC, committee_avg)
  #        return(df)
  #      }
  #    }, simplify = FALSE, USE.NAMES = TRUE)
  #  }, USE.NAMES = TRUE)
  #  if (length(names(p[[1]])) == 1 & !any(class(e) == "matrix")) {
  #    e <- t(as.matrix(e))
  #    rownames(e) <- names(p[[1]])
  #    colnames(e) <- gsub(paste0(".", names(p[[1]])), "", colnames(e))
  #  }
  #}

  ####################### NEW ENSEMBLES APPROACH  #######################

  if (ensembles) {

    message("Ensembling...")

    ## ------------------------------------------------------------
    ## 1. Precompute binary thresholds ONCE per species × model
    ## ------------------------------------------------------------

    bin_thresholds <- lapply(names(m1), function(sp) {
      vapply(m1[[sp]], function(mod) {
        th <- caret::thresholder(
          mod,
          threshold = seq(0, 1, by = 0.001),
          final = TRUE,
          statistics = c("Sensitivity", "Specificity")
        )
        th <- th$prob_threshold[which.max(th$Sensitivity + th$Specificity)] # max(sens+spec)
        if (length(th) > 1) mean(th) else th
      }, numeric(1))
    })
    names(bin_thresholds) <- names(m1)

    ## ------------------------------------------------------------
    ## 2. Ensemble per scenario × species (pure matrix ops)
    ## ------------------------------------------------------------

    e <- lapply(names(p), function(scn) {

      message("  ", scn)
      p_scn <- p[[scn]]

      e1 <- lapply(names(p_scn), function(sp) {

        x <- p_scn[[sp]]
        if (length(x) == 0) return(NULL)

        ## --------------------------------------------------------
        ## Build prediction matrix [cells × models]
        ## --------------------------------------------------------

        pred_mat <- do.call(
          cbind,
          lapply(x, `[[`, "presence")
        )

        pred_mat <- as.matrix(pred_mat)
        storage.mode(pred_mat) <- "double" # this accelerates calculations, since makes functions like
                                           # rowMeans() skip a cohersion step, by granting that
                                           # every value in pred_mat is a double.

        ## --------------------------------------------------------
        ## Mean occurrence probability
        ## --------------------------------------------------------

        mean_occ_prob <- rowMeans(pred_mat, na.rm = TRUE)

        ## --------------------------------------------------------
        ## Weighted mean (AUC)
        ## --------------------------------------------------------

        w <- th1[[sp]]$ROC
        wmean_AUC <- rowSums(pred_mat * w[col(pred_mat)], na.rm = TRUE) /
          sum(w, na.rm = TRUE)

        ## --------------------------------------------------------
        ## Committee average (binary, vectorized)
        ## --------------------------------------------------------

        th_bin <- bin_thresholds[[sp]]
        committee_avg <- rowMeans(pred_mat > th_bin[col(pred_mat)])

        ## --------------------------------------------------------
        ## Output
        ## --------------------------------------------------------

        data.frame(
          cell_id       = x[[1]]$cell_id,
          mean_occ_prob = mean_occ_prob,
          wmean_AUC     = wmean_AUC,
          committee_avg = committee_avg
        )
      })
      names(e1) <- names(p_scn)
      return(e1)
    })
    names(e) <- names(p)

    ## ------------------------------------------------------------
    ## 3. Shape ensembles as species × scenario list-matrix
    ## ------------------------------------------------------------

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

  }

  #####################################################################



  if(ensembles) {
    p2 <- list(
      thresholds = list(values = th1, method = tm, criteria = deparse(th)),
      predictions = p,
      #models = m,
      file = file,
      ensembles = e0,
      grid = scen$grid
    )
  } else {
    p2 <- list(
      thresholds = list(values = th1, method = tm, criteria = deparse(th)),
      predictions = p,
      #models = m,
      file = file,
      grid = scen$grid
    )
  }

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
get_ensembles <- function(i) {
  if (is_input_sdm(i)) {
    return(i$predictions$ensembles)
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
            #models = add_input_sdm(p1$models, p2$models),
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
      #models = x$models,
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
    "Ensembles         :\n",
    "        Methods  :", colnames(x$ensembles[1, 1][[1]])[-1], "\n"
  )
  cat(
    "Thresholds        :\n",
    "        Method   :", x$thresholds$method, "\n",
    "        Criteria :", x$thresholds$criteria, "\n",
    "        Metrics  :\n"
  )
  print(x$thresholds$values)
  invisible(x)
}
