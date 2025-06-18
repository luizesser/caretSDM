#' Model Response to Variables
#'
#' Obtain the Partial Dependence Plots (PDP) to each variable.
#'
#' @usage pdp_sdm(i, spp = NULL, algo = NULL, variables_selected = NULL, mean.only = FALSE)
#'
#' @param i A \code{input_sdm} object.
#' @param spp A \code{character} vector with species names to obtain the PDPs. If \code{NULL}
#' (standard), the first species in \code{species_names(i)} is used.
#' @param algo A \code{character} containing the algorithm to obtain the PDP. If \code{NULL}
#' (standard) all algorithms are mixed.
#' @param variables_selected A \code{character}. If there is a subset of predictors that should be ploted in this, it
#' can be informed using this parameter.
#' @param mean.only Boolean. Should only the mean curve be plotted or a curve to each run should be
#' included? Standard is FALSE.
#'
#' @return A plot (for \code{pdp_sdm}) or a data.frame (for \code{get_pdp_sdm}) with PDP values.
#'
#' @seealso \code{\link{varImp_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
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
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- vif_predictors(i)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method="bioclim", variables_selected = "vif")
#'
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "repeatedcv",
#'                                 number = 4,
#'                                 repeats = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("naive_bayes", "kknn"), variables_selected = "vif", ctrl=ctrl_sdm)
#'
#' # PDP plots:
#' pdp_sdm(i)
#' get_pdp_sdm(i)
#'
#' @importFrom dplyr bind_rows all_of filter group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom pdp partial
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_smooth facet_wrap labs theme_minimal
#'
#' @global value variable yhat id
#'
#' @export
pdp_sdm <- function(i, spp = NULL, algo = NULL, variables_selected = NULL, mean.only = FALSE) {
  assert_class_cli(i, "input_sdm")
  assert_subset_cli(algo, algorithms_used(i))
  assert_subset_cli(spp, species_names(i))
  assert_subset_cli(variables_selected, i$models$predictors)

  if(is.null(spp)){ spp <- species_names(i)[1] }
  if(is.null(algo)){ algo <- algorithms_used(i) }
  if(is.null(variables_selected)){ variables_selected <- i$models$predictors }

  l <- get_pdp_sdm(i, spp, algo, variables_selected)
  l2 <- dplyr::bind_rows(l)

  x <- ggplot2::ggplot(data = dplyr::group_by(l2,variable), ggplot2::aes(x = value, y = yhat, group = id)) +
    ggplot2::facet_wrap(~ variable, scales = "free_x")+
    ggplot2::geom_smooth(ggplot2::aes(x = value, y = yhat, group=variable), color = "blue",
                         linewidth = 1.2) +
    ggplot2::labs(title = "Partial Dependence Plot",
                  x = "",
                  y = "Probability of Occurrence") +
    ggplot2::theme_minimal()

  if(!mean.only) {
    x <- x + ggplot2::geom_line(alpha=0.2)
  }

  return(x)
}

#' @rdname pdp_sdm
#' @export
get_pdp_sdm <- function(i, spp = NULL, algo = NULL, variables_selected = NULL){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli(algo, algorithms_used(i))
  assert_subset_cli(spp, species_names(i))
  assert_subset_cli(variables_selected, i$models$predictors)

  m <- get_models(i)
  if(is.null(spp)){ spp <- species_names(i)[1] }
  if(is.null(algo)){ algo <- algorithms_used(i) }
  if(is.null(variables_selected)){ variables_selected <- i$models$predictors }

  n_algo <- match(algo, algorithms_used(i))

  l <- lapply(n_algo, function(y){
    n <- names(m[[spp]])[grep(paste0("\\.",y,"$"), names(m[[spp]]))]
    l <- list()
    for(v in variables_selected){
      suppressWarnings(l1 <- lapply(m[[spp]][n], function(x){pdp::partial(x, pred.var = v, plot = FALSE, prob = TRUE)}))
      l[[v]] <- dplyr::bind_rows(l1, .id="id")
    }
    df <- dplyr::bind_rows(l)
    df_long <- df |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables_selected), # Select columns that start with "var"
        names_to = "variable",     # New column for variable names
        values_to = "value"        # New column for variable values
      ) |>
      dplyr::filter(!is.na(value))
    return(df_long)
  })
  names(l) <- algorithms_used(i)[n_algo]
  return(l)
}
