#' Predictors as PCA-axes
#'
#' Transform predictors data into PCA-axes.
#'
#' @usage pca_predictors(i, cumulative_proportion = 0.99)
#'
#' @param i A \code{input_sdm} object.
#' @param cumulative_proportion A \code{numeric} with the threshold for cumulative proportion.
#' Standard is 0.99, meaning that axes returned as predictors sum up more than 99% of environmental
#' variance.
#'
#' @details
#' \code{pca_predictors} Transform predictors data into PCA-axes. If the user wants to use PCA-axes
#' as future scenarios, then scenarios should be added after the PCA transformation (see examples).
#' \code{pca_summary} Returns the summary of \code{prcomp} function. See ?stats::prcomp.
#' \code{get_pca_model} Returns the model built to calculate PCA-axes.
#'
#' @return \code{input_sdm} object with variables from both \code{predictors} and \code{scenarios}
#' transformed in PCA-axes.
#'
#' @seealso \code{\link{vif_predictors} \link{sdm_area} \link{add_scenarios} \link{add_predictors}}
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
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # PCA transformation:
#' i <- pca_predictors(i)
#'
#' # Include scenarios:
#' i <- add_scenarios(i, scen)
#'
#' @importFrom dplyr select
#' @importFrom stats prcomp
#'
#' @export
pca_predictors <- function(i, cumulative_proportion = 0.99) {
  assert_class_cli(i, "input_sdm")
  assert_numeric_cli(cumulative_proportion, lower = 0, upper = 1)
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_class_cli(i$predictors, "sdm_area")

  pred_df <- get_predictors(i) |>
    as.data.frame() |>
    dplyr::select(-c('cell_id', 'geometry'))

  pca_model <- stats::prcomp(pred_df)

  pred_pca <- get_predictors(i) |>
    cbind(pca_model$x)

  if(!"scenarios" %in% names(i)){
    i <- add_scenarios(i)
  }
  scen_df <- i$scenarios$data |>
    lapply(function(x){
      as.data.frame(x) |>
        dplyr::select(-c('cell_id', 'geometry'))
    })

  scen_pca <- names(scen_df) |>
    sapply(function(x){
      i$scenarios$data[[x]] |>
        cbind(predict(pca_model, newdata = scen_df[[x]]))
    }, simplify = FALSE, USE.NAMES = TRUE)

  i$predictors$variable_selection$pca$data <- pred_pca |> select(!get_predictor_names(i))
  i$predictors$variable_selection$pca$summary <- summary(pca_model)
  i$predictors$variable_selection$pca$model <- pca_model
  i$predictors$variable_selection$pca$selected_variables <- colnames(pca_summary(i)$importance)[1:(sum(pca_summary(i)$importance["Cumulative Proportion",]<cumulative_proportion)+1)]
  i$predictors$variable_selection$pca$cumulative_proportion_th <- cumulative_proportion

  i$predictors$grid <- pred_pca
  i$scenarios$data <- scen_pca

  return(i)
}

#' @rdname pca_predictors
#' @export
pca_summary <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok = FALSE)
  assert_subset_cli("pca", names(i$predictors$variable_selection), empty.ok = FALSE)
  assert_subset_cli("summary", names(i$predictors$variable_selection$pca), empty.ok = FALSE)
  assert_class_cli(i$predictors$variable_selection$pca$summary, "summary.prcomp")

  return(i$predictors$variable_selection$pca$summary)
}

#' @rdname pca_predictors
#' @export
get_pca_model <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok = FALSE)
  assert_subset_cli("pca", names(i$predictors$variable_selection), empty.ok = FALSE)
  assert_subset_cli("summary", names(i$predictors$variable_selection$pca), empty.ok = FALSE)
  assert_class_cli(i$predictors$variable_selection$pca$summary, "summary.prcomp")

  return(i$predictors$variable_selection$pca$model)
}
