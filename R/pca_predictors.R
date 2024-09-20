#' Predictors as PCA-axis
#'
#' Transform predictors data into PCA-axis.
#'
#' @usage pca_predictors(i)
#'
#' @param i A \code{input_sdm} object.
#'
#' @return \code{input_sdm} object with variables from both \code{predictors} and \code{scenarios}
#' transformed in PCA-axis.
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
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
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
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- pca_predictors(i)
#'
#' @importFrom dplyr select
#'
#' @export
pca_predictors <- function(i) {
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok=F)
  assert_class_cli(i$predictors, "sdm_area")

  pred_df <- get_predictors(i) |>
    as.data.frame() |>
    dplyr::select(-c('cell_id', 'geometry'))

  pca_model <- prcomp(pred_df)

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
  i$predictors$variable_selection$pca$selected_variables <- colnames(pca_summary(i)$importance)[1:(sum(pca_summary(i)$importance["Cumulative Proportion",]<0.99)+1)]

  i$predictors$grid <- pred_pca
  i$scenarios$data <- scen_pca

  return(i)
}

#' @rdname pca_predictors
#' @export
pca_summary <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok=F)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok=F)
  assert_subset_cli("pca", names(i$predictors$variable_selection), empty.ok=F)
  assert_subset_cli("summary", names(i$predictors$variable_selection$pca), empty.ok=F)
  assert_class_cli(i$predictors$variable_selection$pca$summary, "summary.prcomp")

  return(i$predictors$variable_selection$pca$summary)
}

#' @rdname pca_predictors
#' @export
get_pca_model <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok=F)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok=F)
  assert_subset_cli("pca", names(i$predictors$variable_selection), empty.ok=F)
  assert_subset_cli("summary", names(i$predictors$variable_selection$pca), empty.ok=F)
  assert_class_cli(i$predictors$variable_selection$pca$summary, "summary.prcomp")

  return(i$predictors$variable_selection$pca$model)
}
