#' Model Response to Variables
#'
#' Obtain the Partial Dependence Plots (PDP) to each variable.
#'
#' @usage pdp_sdm(i, spp = NULL, algo = NULL, variables_selected = NULL)
#'
#' @param i A \code{input_sdm} object.
#' @param spp A \code{character} vector with species names to obtain the PDPs. If \code{NULL}
#' (standard), the first species in \code{species_names(i)} is used.
#' @param algo A \code{character} containing the algorithm to obtain the PDP. If \code{NULL}
#' (standard) all algorithms are mixed.
#' @param variables_selected If there is a subset of predictors that should be ploted in this, it
#' can be informed using this parameter.
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
#' sa <- add_predictors(sa, bioc)
#'
#' # Create input_sdm:
#' i <- input_sdm(occurrences_sdm(occ), sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- vif_predictors(i)
#' i
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method="bioclim", variables_selected = "vif")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("nnet", "kknn"), variables_selected = "vif")
#' i
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
#' @export
pdp_sdm <- function(i, spp = NULL, algo = NULL, variables_selected = NULL) {
  assert_class_cli(i, "input_sdm")
  assert_subset_cli(algo, algorithms_used(i))
  assert_subset_cli(spp, species_names(i))
  assert_subset_cli(variables_selected, i$models$predictors)

  if(is.null(spp)){ spp <- species_names(i)[1] }
  if(is.null(algo)){ algo <- algorithms_used(i) }
  if(is.null(variables_selected)){ variables_selected <- i$models$predictors }

  l <- get_pdp_sdm(i, spp, algo, variables_selected)
  l2 <- bind_rows(l)

  summary_stats <- l2 |>
    dplyr::group_by(value, variable) |>
    dplyr::summarise(mean_value = mean(yhat, na.rm = TRUE),
                     min_value = min(yhat, na.rm = TRUE),
                     max_value = max(yhat, na.rm = TRUE))

  x <- ggplot2::ggplot(data = summary_stats, ggplot2::aes(x = value, y = mean_value, group = variable)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min_value, ymax = max_value), fill = "grey", alpha = 0.5) +  # Grey ribbon for range
    ggplot2::geom_smooth(ggplot2::aes(x = value, y = mean_value), color = "red",
                         linewidth = 1.2) +  # Add mean line
    ggplot2::facet_wrap(~ variable, scales = "free_x") +
    ggplot2::labs(title = "Partial Dependence Plot",
                  x = "",
                  y = "Probability of Occurrence") +
    ggplot2::theme_minimal()

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
      l1 <- lapply(m[[spp]][n], function(x){pdp::partial(x, pred.var = v, plot = F, prob = T)})
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
