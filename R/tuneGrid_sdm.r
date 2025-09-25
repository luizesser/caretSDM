#' Retrieve tuneGrid from models
#'
#' This function aims to unveil the correlation of different algorithms outputs.
#' For that, it uses the predictions on current scenario, but other scenarios can be tested.
#'
#' @usage tuneGrid_sdm(i, model_id = NULL)
#'
#' @param i A \code{input_sdm} object containing models.
#'
#' @returns A \code{list} with \code{data.frames} each one representing the table of a given model.
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
#' # Retrieve tuneGrid from model:
#' tuneGrid_sdm(i)
#'
#' @export
tuneGrid_sdm <- function(i) {
  if (!is_input_sdm(i)) {
    stop("i must be of class input_sdm")
  }
  m <- i$models$models
  spp <- species_names(i)

  x <- sapply(spp, function(sp) {
    lapply(m[[sp]], function(x){
      hip <- colnames(x$bestTune)
      x$results[,hip]
    })
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(x)
}
