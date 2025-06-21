#' Ensemble GCMs into one scenario
#'
#' An ensembling method to group different GCMs into one SSP scenario
#'
#' @usage
#' gcms_ensembles(i, gcms = NULL)
#'
#' @param i A \code{input_sdm} object.
#' @param gcms GCM codes in \code{scenarios_names(i)} to group scenarios.
#'
#' @returns A \code{input_sdm} object with grouped GCMs.
#'
#'
#' @seealso \code{\link{GBIF_data} \link{occurrences_sdm} \link{sdm_area} \link{input_sdm}
#' \link{predictors}}
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
#' sa <- add_predictors(sa, bioc)
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen) |> select_predictors(c("bio1", "bio12"))
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method="random", n_set = 2)
#'
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "boot",
#'                                 number = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i,
#'                algo = c("naive_bayes"),
#'                ctrl=ctrl_sdm,
#'                variables_selected = c("bio1", "bio12")) |>
#'   suppressWarnings()
#'
#' # Predict models:
#' i  <- predict_sdm(i, th=0.8)
#'
#' #' # Ensemble GCMs:
#' i <- gcms_ensembles(i, gcms = c("ca", "mi"))
#' i
#'
#' @importFrom dplyr bind_cols
#'
#' @export
gcms_ensembles <- function(i, gcms=NULL) {
  if (is_input_sdm(i)) {
    y <- i$predictions$ensembles
  }
  emet <- colnames(y[[1]])[-1]
  cols <- colnames(y)
  for (g in gcms) {
    cols <- gsub(g,"",cols)
  }

  scen_names <- names(table(cols)[table(cols)>1])

  out <- matrix(nrow=nrow(y), ncol = length(scen_names), dimnames = list(rownames(y), scen_names))
  l <- list()
  for(sp in rownames(y)){
    for(sc in scen_names){
      ysc <- y[sp, grep(sc, colnames(y))]
      ysc <- dplyr::bind_cols(ysc)
      l2 <- list()
      for (m in emet) {
        l2[[m]] <- rowMeans(ysc[,grep(m, colnames(ysc))])
      }
      l <- append(l,list(data.frame(cell_id=ysc[,1] , dplyr::bind_cols(l2))))
    }
  }

  m <- matrix(l, nrow=nrow(y), ncol = length(scen_names), dimnames = list(rownames(y), scen_names), byrow = TRUE)
  y2 <- cbind(y,m)

  i$predictions$ensembles <- y2
  return(i)
}

