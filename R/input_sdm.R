#' \code{input_sdm}
#'
#' This function creates a new \code{input_sdm} object.
#'
#' @param ... Data to be used in SDMs. Can be a \code{occurrences} and/or a \code{sdm_area} object.
#' @param i1 A \code{input_sdm} object.
#' @param i2 A \code{input_sdm} object.
#'
#' @returns A \code{input_sdm} object containing:
#'    \item{grid}{\code{sf} with POLYGON geometry representing the grid for the study area or
#'    LINESTRING if \code{sdm_area} was built with a LINESTRING \code{sf}.}
#'    \item{bbox}{Four corners for the bounding box (class \code{bbox}): minimum value of X, minimum
#'    value of Y, maximum value of X, maximum value of Y}
#'    \item{cell_size}{\code{numeric} information regarding the size of the cell used to rescale
#'    variables to the study area, representing also the cell size in the \code{grid}.}
#'    \item{epsg}{\code{character} information about the EPSG used in all slots from \code{sdm_area}.}
#'    \item{predictors}{\code{character} vector with predictors names included in \code{sdm_area}.}
#'
#' @details
#' If \code{sdm_area} is used, it can include predictors and scenarios. In this case,
#' \code{input_sdm} will detect and include as \code{scenarios} and \code{predictors} in the
#' \code{input_sdm} output. Objects can be included in any order, since the function will work by
#' detecting their classes.
#' The returned object is used throughout the whole workflow to apply functions.
#'
#' @seealso \code{\link{occurrences_sdm} \link{sdm_area}}
#'
#' @author Luiz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 50000, output_crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, occ_crs = 6933)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' @importFrom stats sd
#' @importFrom cli cli_abort
#' @importFrom utils packageVersion
#' @import checkCLI
#'
#' @export
input_sdm <- function(...) {
  x <- list(...)
  inp <- .input_sdm(x)
  return(inp)
}

.input_sdm <- function(x) {
  classes <- lapply(x, class)
  if (!length(unique(classes)) == length(classes)) {
    cli::cli_abort(c(
      "x" = "There are two objects or more with the same class.",
      "i" = "Provide only unique object classes."
    ))
  }
  l <- list()
  if ("occurrences" %in% classes) {
    if ("sdm_area" %in% classes &
      "cell_id" %in% colnames(x[classes %in% "occurrences"][[1]]$occurrences)) {
      l$occurrences <- x[classes %in% "occurrences"][[1]]
    } else {
      if ("sdm_area" %in% classes) {
        l$occurrences <- join_area(
          x[classes %in% "occurrences"][[1]],
          x[classes %in% "sdm_area"][[1]]
        )
      } else {
        l$occurrences <- x[classes %in% "occurrences"][[1]]
      }
    }
  }
  if ("sdm_area" %in% classes) {
    sa <- x[classes %in% "sdm_area"][[1]]
    if ("scenarios" %in% names(sa)) {
      l$predictors <- .sdm_area(sa[names(sa) != "scenarios"])
      l$scenarios <- sa$scenarios
    } else {
      l$predictors <- x[classes %in% "sdm_area"][[1]]
    }
  }

  inp <- structure(l,
    class = "input_sdm"
  )
  return(inp)
}

#' @rdname input_sdm
#' @export
add_input_sdm <- function(i1, i2) {
  assert_class_cli(i1, "input_sdm")
  assert_class_cli(i2, "input_sdm")

  l <- list(
    occurrences = add_occurrences(i1$occurrences, i2$occurrences),
    predictors = add_sdm_area(i1$predictors, i2$predictors),
    scenarios = add_sdm_area(i1$scenarios, i2$scenarios),
    models = add_models(i1$models, i2$models),
    predictions = add_predictions(i1$predictions, i2$predictions),
    predictions = add_ensembles(i1$ensembles, i2$ensembles)
  )
  i <- structure(l,
    class = "input_sdm"
  )
  return(i)
}

#' Print method for input_sdm
#' @param x input_sdm object
#' @param ... passed to other methods
#' @returns Concatenate structured characters to showcase what is stored in the object.
#' @exportS3Method base::print
print.input_sdm <- function(x, ...) {
  cat("             caretSDM           \n")
  cat("................................\n")
  cat("Class                          : input_sdm\n")
  cat("\n=========== Overview ===========\n")
  if ("occurrences" %in% names(x)) {
    cat("Focal Taxon                    :", paste(x$occurrences$spp_names, collapse = ", "), "\n")
  }
  if ("predictors" %in% names(x) && is_sdm_area(x$predictors)) {
    cat("Spatial extent                 :", paste(sf::st_bbox(x$predictors$grid), collapse = ", "), " (xmin,xmax,ymin,ymax)\n")
  }

  if ("scenarios" %in% names(x)) {
    sc_names <- names(x$scenarios$data)
    years <- as.integer(unlist(regmatches(sc_names, gregexpr("\\d{4}", sc_names))))
    if (length(years) > 0) {
      cat("Temporal extent (inferred)     :", min(years), "-", max(years), "\n")
    } else if (any(grepl("current|present", sc_names, ignore.case = TRUE))) {
      cat("Temporal extent                : Current\n")
    }
  }
  if ("occurrences" %in% names(x)) {
    obs_type <- "Presence-only"
    if (all(c("pseudoabsences", "background") %in% names(x$occurrences))) {
      obs_type <- "Presence-absence (pseudo-absence) and Presence-background"
    } else if (!is.null(x$occurrences$pseudoabsences)) {
      obs_type <- "Presence-absence (pseudo-absence)"
    } else if (!is.null(x$occurrences$background)) {
      obs_type <- "Presence-background"
    }
    cat("Observation type               :", obs_type, "\n")
  }
  if ("predictors" %in% names(x) && is_sdm_area(x$predictors)) {
    cat("Predictor names                :", paste(get_predictor_names(x$predictors), collapse = ", "), "\n")
  }
  if ("models" %in% names(x)) {
    if (is.list(x$models$algorithms)) {
      cat("Modelling techniques           : Stacked Ensemble\n")
      for (j in seq_along(x$models$algorithms)) {
        cat("  Layer", j, ":", x$models$algorithms[[j]], "\n")
      }
    } else {
      cat("Modelling techniques           :", paste(x$models$algorithms, collapse = ", "), "\n")
    }
    cat("Model complexity (tuneLength)  :", paste(x$models$algorithms, collapse = ", "), "\n")
  }
  if ("ensembles" %in% names(x)) {
    cat("Model averaging                :", x$ensembles$method, "\n")
  }
  cat("Software                       : caretSDM v", as.character(utils::packageVersion("caretSDM")),
      ", ", R.version$version.string, "\n", sep = "")
  cat("\n============= Data =============\n")
  if ("occurrences" %in% names(x)) {
    cat("-- Biodiversity data --\n")
    cat("Taxon names                    :", paste(x$occurrences$spp_names, collapse = ", "), "\n")
    cat("Sample size                    :", paste(x$occurrences$n_presences, collapse = ", "), "\n")

    if (!is.null(x$occurrences$pseudoabsences)) {
      cat("(Pseudo)Absence data method    :", x$occurrences$pseudoabsences$method, "\n")
      cat("Number of PA sets              :", paste(x$occurrences$pseudoabsences$n_set, collapse = ", "), "\n")
      cat("PAs per set                    :", paste(as.numeric(x$occurrences$pseudoabsences$n_pa), collapse = ", "), "\n")
      cat("PA-to-presence ratio           :", paste(round(as.numeric(x$occurrences$pseudoabsences$n_pa) /
                                                            x$occurrences$n_presences, 2), collapse = ", "), "\n")
    }

    if (!is.null(x$occurrences$background)) {
      cat("Background data method         :", x$occurrences$background$method, "\n")
      cat("Number of background sets      :", paste(x$occurrences$background$n_set, collapse = ", "), "\n")
      cat("BGs per set                    :", paste(as.numeric(x$occurrences$background$n_bg), collapse = ", "), "\n")
      cat("Background proportion          :", paste(as.numeric(x$occurrences$background$proportion), collapse = ", "), "\n")
    }

    if (!is.null(x$occurrences$data_cleaning)) {
      cat("Data cleaning                  :", paste(x$occurrences$data_cleaning, collapse = ", "), "\n")
    }

    if (!is.null(x$occurrences$esm)) {
      cat("Ensemble of Small Models (ESM) : TRUE\n")
      cat("ESM records per species        :", paste(x$occurrences$esm$n_records, collapse = ", "), "\n")
    }

    if (!is.null(x$occurrences$mem)) {
      cat("MacroEcological Models (ESM)   : TRUE\n")
    }
  }

  if ("models" %in% names(x) && !is.null(x$models$validation)) {
    cat("-- Data partitioning --\n")
    cat("Training/validation method     :", x$models$validation$method, "\n")
    cat("Number of folds/repeats        :", x$models$validation$number, "\n")
    if ("occurrences" %in% names(x) && "independent_test" %in% names(x$occurrences)) {
      cat("Independent test records       :", nrow(x$occurrences$independent_test), "\n")
    }
  }

  if ("predictors" %in% names(x) && is_sdm_area(x$predictors)) {
    cat("-- Predictor variables --\n")
    cat("Number of predictors           :", ncol(x$predictors$grid) - 2, "\n")
    cat("Predictor names                :", paste(get_predictor_names(x$predictors), collapse = ", "), "\n")
    cat("Spatial extent                 :", paste(sf::st_bbox(x$predictors$grid), collapse = ", "), " (xmin,xmax,ymin,ymax)\n")
    if (!is.null(x$predictors$cell_size)) {
      cat("Spatial resolution             :", paste0("(", x$predictors$cell_size, ", ", x$predictors$cell_size, ")"), "\n")
    }
    cat("Coordinate reference system    :", substr(sf::st_crs(x$predictors$grid)$input, 1, 20), "( EPSG:",sf::st_crs(x$predictors$grid)$epsg,")", "\n")
  }

  if ("scenarios" %in% names(x)) {
    cat("-- Transfer data --\n")
    cat("Number of scenarios            :", length(x$scenarios$data), "\n")
    cat("Scenario names                 :", paste(names(x$scenarios$data), collapse = ", "), "\n")
    if ("stationary" %in% names(x$scenarios)) {
      cat("Stationary variables           :", paste(x$scenarios$stationary, collapse = ", "), "\n")
    }
    sc_names <- names(x$scenarios$data)
    years <- as.integer(unlist(regmatches(sc_names, gregexpr("\\d{4}", sc_names))))
    if (length(years) > 0) {
      cat("Temporal extent (inferred)     :", min(years), "-", max(years), "\n")
    } else if (any(grepl("current|present", sc_names, ignore.case = TRUE))) {
      cat("Temporal extent                : Current\n")
    }
  }
  cat("\n============= Model ============\n")

  if ("predictors" %in% names(x) && is_sdm_area(x$predictors) && "variable_selection" %in% names(x$predictors)) {
    cat("-- Multicollinearity --\n")
    cat("Variable selection method      :", names(x$predictors$variable_selection)[1], "\n")
    if (!names(x$predictors$variable_selection)[1] %in% c("vifstep", "vifcor", "pca")) {
      cat("Selected variables             :", paste(x$predictors$variable_selection[[1]]$selected_variables, collapse = ", "), "\n")
    }
    if (names(x$predictors$variable_selection)[1] %in% c("vifstep", "vifcor")) {
      cat("VIF threshold                  :", x$predictors$variable_selection$vif$threshold, "\n")
      cat("Selected variables             :", paste(x$predictors$variable_selection$vif$selected_variables, collapse = ", "), "\n")
    }
    if (!is.null(x$predictors$variable_selection$pca)) {
      cat("PCA cumulative proportion th.  :", x$predictors$variable_selection$pca$cumulative_proportion_th, "\n")
      cat("PCA-selected components        :", paste(x$predictors$variable_selection$pca$selected_variables, collapse = ", "), "\n")
    }
  }

  if ("models" %in% names(x)) {
    cat("-- Model settings --\n")
    cat("Predictors used                :", paste(x$models$predictors, collapse = ", "), "\n")

    if (!is.null(x$models$models) && is.list(x$models$models)) {
      cat("Model hyperparameters          :\n")
      print(models_hyperparameters(x$models))
    }

    if (!is.null(x$models$models) && length(x$models$models) > 0) {
      first_fit <- x$models$models[[1]]
      if (inherits(first_fit, "train") && !is.null(first_fit$preProcess) &&
          length(first_fit$preProcess$method) > 0) {
        cat("Variable transformation        :", paste(first_fit$preProcess$method, collapse = ", "), "\n")
      }
    }
  }

  if ("predictions" %in% names(x) && !is.null(x$predictions$thresholds)) {
    cat("-- Threshold selection --\n")
    cat("Threshold method               :", x$predictions$thresholds$method, "\n")
    cat("Threshold criteria             :", x$predictions$thresholds$criteria, "\n")
  }

  cat("\n========== Assessment ==========\n")

  if ("models" %in% names(x)) {
    if (!is.null(x$models$validation)) {
      cat("-- Performance statistics --\n")
      cat("Cross-validation metrics       :\n")
      print(lapply(mean_validation_metrics(x), function(y) as.data.frame(y[, 1:5])))
    }
    if ("independent_validation" %in% names(x$models)) {
      cat("Independent test ROC           :", round(mean(unlist(x$models$independent_validation)), 3),
          "+/-", round(stats::sd(unlist(x$models$independent_validation)), 3), "\n")
    }
  }

  if ("predictions" %in% names(x) || "ensembles" %in% names(x)) {
    cat("\n========== Prediction ==========\n")

    if ("predictions" %in% names(x)) {
      pred_layers <- names(x$predictions$predictions)
      if (length(pred_layers) > 0) {
        cat("Prediction layers              :", paste(pred_layers, collapse = ", "), "\n")
        pred_type <- "Occurrence Probability"
        if (!is.null(x$predictions$thresholds) &&
            any(grepl("binary|pa|presence", pred_layers, ignore.case = TRUE))) {
          pred_type <- "Binary (thresholded)"
        }
        cat("Prediction unit                :", pred_type, "\n")
      } else {
        cat("No prediction layers computed yet.\n")
      }
    }

    if ("ensembles" %in% names(x)) {
      cat("Ensemble method                :", paste(x$ensembles$method, collapse = ", "), "\n")
      cat("Ensemble names                 :", paste(colnames(x$ensembles$data)[!colnames(x$ensembles$data) %in% names(x$predictions$predictions)], collapse = ", "), "\n")
    }
  }

  invisible(x)
}
