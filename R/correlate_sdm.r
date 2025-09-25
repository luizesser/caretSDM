#' Correlation between projections
#'
#' This function aims to unveil the correlation of different algorithms outputs.
#' For that, it uses the predictions on current scenario, but other scenarios can be tested.
#'
#' @usage correlate_sdm(i, scenario = "current")
#'
#' @param i A \code{input_sdm} object containing predictions.
#' @param scenario A \code{character} containing scenario to be tested. Standard is
#' \code{"current"}. Value must match \code{scenarios_names(i)}.
#'
#' @returns A \code{data.frame} with pearson correlation between projections.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' set.seed(1)
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
#' i <- pseudoabsences(i, method="random", n_set=2)
#'
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "boot",
#'                                 number = 1,
#'                                 repeats = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
#'   suppressWarnings()
#'
#' # Predict models:
#' i  <- predict_sdm(i, th = 0.8)
#'
#' # Check correlations:
#' correlate_sdm(i)
#'
#' @importFrom stats cor
#' @export
correlate_sdm <- function(i, scenario = "current") {
  if (!is_input_sdm(i)) {
    stop("i must be of class input_sdm")
  }
  if (!"predictions" %in% names(i)) {
    stop("i must have predictions. Run predict_sdm to check correlations.")
  }
  assert_choice_cli(scenario, scenarios_names(i))

  scen <- i$predictions$predictions[[scenario]]
  spp <- species_names(i)

  x <- sapply(spp, function(sp) {
    do.call(cbind, lapply(scen[[sp]], function(x){x$presence})) |> stats::cor()
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(x)
}
