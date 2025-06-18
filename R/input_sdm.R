#' \code{input_sdm}
#'
#' This function creates a new \code{input_sdm} object.
#'
#' @param ... Data to be used in SDMs. Can be a \code{occurrences} and/or a \code{sdm_area} object.
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
#' @importFrom stats sd
#'
#' @export
input_sdm <- function(...) {
  x <- list(...)
  inp <- .input_sdm(x)
  return(inp)
}

#' @export
.input_sdm <- function(x) {
  classes <- lapply(x, class)
  l <- list()
  if ("occurrences" %in% classes) {
    l$occurrences <- x[classes %in% "occurrences"][[1]]
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

#' Print method for input_sdm
#' @param x input_sdm object
#' @param ... passed to other methods
#' @exportS3Method base::print
print.input_sdm <- function(x, ...) {
  cat("            caretSDM           \n")
  cat("...............................\n")
  cat("Class                         : input_sdm\n")
  if ("occurrences" %in% names(x)) {
    cat("--------  Occurrences  --------\n")
    cat("Species Names                 :", x$occurrences$spp_names, "\n")
    cat("Number of presences           :", x$occurrences$n_presences, "\n")
    if (!is.null(x$occurrences$pseudoabsences)) {
      cat(
        "Pseudoabsence methods         :\n",
        "   Method to obtain PAs      :", x$occurrences$pseudoabsences$method, "\n",
        "   Number of PA sets         :", x$occurrences$pseudoabsences$n_set, "\n",
        "   Number of PAs in each set :", as.numeric(x$occurrences$pseudoabsences$n_pa), "\n"
      )
    }
    if (!is.null(x$occurrences$background)) {
      cat("Background sets           :", length(x$occurrences$background), "\n")
    }
    if ("independent_test" %in% names(x$occurrences)) {
      cat("Independent Test              : TRUE (number of records = ", nrow(x$occurrences$independent_test), ")\n")
    }
    if (!is.null(x$occurrences$data_cleaning)) {
      cat(cat("Data Cleaning                 : "), cat(x$occurrences$data_cleaning, sep = ", "), "\n")
    }
  }
  if ("predictors" %in% names(x)) {
    if (is_sdm_area(x$predictors)) {
      cat("--------  Predictors  ---------\n")
      cat("Number of Predictors          :", ncol(x$predictors$grid)-2, "\n")
      cat(cat("Predictors Names              : "), cat(predictors(x$predictors), sep = ", "), "\n")
      if (!is.null(x$predictors$bbox)) {
        cat("Extent                        :", sf::st_bbox(x$predictors$grid), "(xmin, xmax, ymin, ymax)\n")
      }
      if (!is.null(x$predictors$epsg)) {
        cat("EPSG                          :", substr(sf::st_crs(x$predictors$grid)$input, 1, 20), "\n")
      }
      if (!is.null(x$predictors$resolution)) {
        cat("Resolution                    :", paste0("(", x$predictors$cell_size, ", ", x$cell_size, ")"), "(x, y)\n")
      }
      if (!is.null(x$predictors$variable_selection$vif)) {
        cat(
          cat("Area (VIF)                    : "), cat(x$predictors$variable_selection$vif$area), cat("\n"),
          cat("Threshold                     : "), cat(x$predictors$variable_selection$vif$threshold), cat("\n"),
          cat("Selected Variables (VIF)      : "), cat(x$predictors$variable_selection$vif$selected_variables, sep = ", "), "\n"
        )
      }
      if (!is.null(x$predictors$variable_selection$pca)) {
        cat(
          cat("PCA-transformed variables     : DONE \n"),
          cat("Cummulative proportion (",x$predictors$variable_selection$pca$cumulative_proportion_th,") : "), cat(x$predictors$variable_selection$pca$selected_variables, sep = ", "), "\n"
        )
      }
    }
  }
  if ("scenarios" %in% names(x)) {
    cat("---------  Scenarios  ---------\n")
    cat("Number of Scenarios           :", length(x$scenarios$data), "\n")
    cat("Scenarios Names               :", names(x$scenarios$data), "\n")
    if ("stationary" %in% names(x$scenarios)) {
      cat("Stationary Variables          :", x$scenarios$stationary, "\n")
    }
  }
  if ("models" %in% names(x)) {
    cat("-----------  Models  ----------\n")
    if (is.list(x$models$algorithms)) {
      cat("Algorithms Names              : Stacked Ensemble\n")
      for (j in 1:length(x$models$algorithms)) {
        cat("   Layer ", j, ": ", x$models$algorithms[[j]], "\n")
      }
    } else {
      cat("Algorithms Names              :", x$models$algorithms, "\n")
    }
    cat("Variables Names               :", x$models$predictors, "\n")
    cat(
      "Model Validation              :\n",
      "   Method                    :", x$models$validation$method, "\n",
      "   Number                    :", x$models$validation$number, "\n",
      "   Metrics                   :\n"
    )
    print(lapply(mean_validation_metrics(x), function(y){ as.data.frame(y[,1:5])}))
    if ("independent_validation" %in% names(x$models)) {
      cat(
        "Independent Validation        :\n",
        "   ROC (mean +- sd)            : ", round(mean(unlist(x$models$independent_validation)), 3),
        " +- ",
        round(stats::sd(unlist(x$models$independent_validation)), 3), "\n"
      )
    }
  }
  if ("predictions" %in% names(x)) {
    cat("--------  Predictions  --------\n")
    cat(
      "Ensembles                     :\n",
      "   Scenarios                 :", colnames(x$predictions$ensembles), "\n",
      "   Methods                   :", colnames(x$predictions$ensembles[1, 1][[1]])[-1], "\n"
    )
    cat(
      "Thresholds                    :\n",
      "   Method                    :", x$predictions$thresholds$method, "\n",
      "   Criteria                  :", x$predictions$thresholds$criteria, "\n"
    )
  }
  invisible(x)
}
