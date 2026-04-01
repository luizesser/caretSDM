#' Multicollinearity Analysis
#'
#' Apply multicollinearity calculation on predictors.
#'
#' @usage multicollinearity_sdm(pred,
#'                              method = NULL,
#'                              variables_selected = NULL,
#'                              cumulative_proportion = 0.99,
#'                              th = 0.5,
#'                              ...)
#'
#' @param pred A \code{input_sdm} or \code{predictors} object.
#' @param method Which method should be used to detect multicollinearity. Can be a \code{character}
#' or a custom \code{function}.
#' @param variables_selected A vector with pre-selected variables names to filter variables.
#' @param cumulative_proportion A \code{numeric} with the threshold for cumulative proportion in
#' PCA. Standard is 0.99, meaning that axes returned as predictors sum up more than 99% of
#' environmental variance.
#' @param th Threshold to be applied in VIF routine. See ?usdm::vifcor.
#' @param ... Further arguments to be passed to the applied method.
#'
#' @param i A \code{input_sdm} object.
#'
#'
#' @details multicollinearity_sdm is a wrapper function to run usdm::vifcor, usdm::vifstep or a pca
#' in caretSDM, but also provides a way to implement custom functions to reduce multicollinearity.
#' If user provides a custom function, it must have the arguments \code{env_sf} and \code{occ_sf},
#' which will consist of two \code{sf}s. The first has the predictor values for the whole study
#' area, while the second has the presence records for the species. The function must return a
#' vector with selected variables.
#'
#' @return A \code{input_sdm} or \code{predictors} object with VIF data.
#'
#' @seealso \code{\link{vif_predictors} \link{pca_predictors} \link{get_predictor_names}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # VIF calculation:
#' i <- multicollinearity_sdm(i, method = "vifcor", th = 0.5)
#' i
#'
#' # Retrieve information about vif:
#' vif_summary(i)
#' selected_variables(i)
#'
#' # Example of custom function:
#' custom_function <- function(env_sf, occ_sf) {
#'   env_df <- dplyr::select(sf::st_drop_geometry(env_sf), -"cell_id")
#'   correlations <- cor(env_df)
#'   col <- caret::findCorrelation(correlations, cutoff = 0.7)
#'   selected <- colnames(correlations)[-col]
#'   return(selected)
#' }
#'
#' @importFrom usdm vifcor vifstep
#' @importFrom dplyr filter select mutate_if all_of
#' @importFrom sf st_centroid st_as_sf
#' @importFrom stars st_extract
#' @importFrom stats prcomp
#' @import checkCLI
#'
#' @export
multicollinearity_sdm <- function(pred,
                                  method = NULL,
                                  variables_selected = NULL,
                                  cumulative_proportion = 0.99,
                                  th = 0.5,
                                   ...) {

  assert_class_cli(pred, "input_sdm")
  assert_subset_cli("predictors", names(pred), empty.ok = FALSE)
  assert_class_cli(pred$predictors, "sdm_area")
  assert_subset_cli(variables_selected, c(get_predictor_names(pred), "vif", "pca"), empty.ok = TRUE)
  if (is_input_sdm(pred)) {
    x <- pred$predictors
    occ <- pred$occurrences$occurrences
    epsg <- pred$occurrences$crs
  }
  if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(x)
  }
  facnum <- function(x) {
    return(as.numeric(as.factor(x)))
  }
  if (is.character(method)) {

    assert_subset_cli(method, c("vifcor", "vifstep", "pca"))

    if (method == "vifcor") {
      assert_numeric_cli(th, len = 1, null.ok = FALSE, upper = 1, lower = 0, any.missing = FALSE)
      v <- x$grid |>
        as.data.frame() |>
        dplyr::select(-c("geometry", "cell_id")) |>
        dplyr::select(dplyr::all_of(variables_selected)) |>
        dplyr::mutate_if(is.character, facnum) |>
        usdm::vifcor(th=th, ...)
      x$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
      x$variable_selection$vif$threshold <- th
      x$variable_selection$vif$vifcor <- v
    }

    if (method == "vifstep") {
      assert_numeric_cli(th, len = 1, null.ok = FALSE, upper = 1000, lower = 0, any.missing = FALSE)
      v <- x$grid |>
        as.data.frame() |>
        dplyr::select(-c("geometry", "cell_id")) |>
        dplyr::select(dplyr::all_of(variables_selected)) |>
        dplyr::mutate_if(is.character, facnum) |>
        usdm::vifstep(th=th, ...)
      x$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
      x$variable_selection$vif$threshold <- th
      x$variable_selection$vif$vifstep <- v
    }

    if (method == "pca") {
      assert_numeric_cli(cumulative_proportion, lower = 0, upper = 1)
      pca_model <- x$grid |>
        as.data.frame() |>
        dplyr::select(-c("geometry", "cell_id")) |>
        dplyr::select(dplyr::all_of(variables_selected)) |>
        dplyr::mutate_if(is.character, facnum) |>
        stats::prcomp()

      pred_pca <- get_predictors(x) |>
          cbind(pca_model$x)

      if(!"scenarios" %in% names(pred)){
        pred <- add_scenarios(pred)
      }
      scen_df <- pred$scenarios$data |>
        lapply(function(x){
          as.data.frame(x) |>
            dplyr::select(-c('cell_id', 'geometry'))
        })

      x$variable_selection$pca$data <- pred_pca |> dplyr::select(!get_predictor_names(pred))
      x$variable_selection$pca$summary <- summary(pca_model)
      x$variable_selection$pca$model <- pca_model
      x$variable_selection$pca$selected_variables <- colnames(summary(pca_model)$importance)[1:(sum(summary(pca_model)$importance["Cumulative Proportion",]<cumulative_proportion)+1)]
      x$variable_selection$pca$cumulative_proportion_th <- cumulative_proportion

      x$grid <- pred_pca

      scen_pca <- names(scen_df) |>
        sapply(function(x){
          pred$scenarios$data[[x]] |>
            cbind(predict(pca_model, newdata = scen_df[[x]]))
        }, simplify = FALSE, USE.NAMES = TRUE)

      pred$scenarios$data <- scen_pca
    }
  }

  if (is.function(method)) {
    assert_function_cli(method, args = c("env_sf", "occ_sf"))
    method_name <- deparse(substitute(method))
    v <- method(env_sf = x$grid, occ_sf = occ, ...)
    assert_subset_cli(v, get_predictor_names(pred))
    x$variable_selection[[method_name]]$selected_variables <- v
  }

  if (is_input_sdm(pred)) {
    pred$predictors <- x
    x <- pred
  }

  return(x)
}

#' @rdname multicollinearity_sdm
#' @export
selected_variables <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok = FALSE)
  return(i$predictors$variable_selection[[1]]$selected_variables)
}
