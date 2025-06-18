#' Calculate VIF
#'
#' Apply Variance Inflation Factor (VIF) calculation.
#'
#' @usage vif_predictors(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected =
#' NULL)
#'
#' @param pred A \code{input_sdm} or \code{predictors} object.
#' @param area Character. Which area should be used in vif selection? Standard is \code{"all"}.
#' @param th Threshold to be applied in VIF routine. See ?usdm::vifcor.
#' @param maxobservations Max observations to use to calculate the VIF.
#' @param variables_selected If there is a subset of predictors that should be used in this
#' function, it can be informed using this parameter. If set to \code{NULL} (standard) all variables
#' are used.
#' @param i A \code{input_sdm} to retrieve information from.
#'
#' @details vif_predictors is a wrapper function to run usdm::vifcor in caretSDM.
#'
#' @return A \code{input_sdm} or \code{predictors} object with VIF data.
#'
#' @seealso \code{\link{get_predictor_names}}
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
#' i
#'
#' # Retrieve information about vif:
#' vif_summary(i)
#' selected_variables(i)
#'
#' @importFrom usdm vifcor
#' @importFrom dplyr filter select mutate_if all_of
#' @importFrom sf st_centroid st_as_sf
#' @importFrom stars st_extract
#'
#' @export
vif_predictors <- function(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected = NULL) {
  assert_class_cli(pred, "input_sdm")
  assert_subset_cli("predictors", names(pred), empty.ok = FALSE)
  assert_class_cli(pred$predictors, "sdm_area")
  assert_class_cli(pred, "input_sdm")
  assert_numeric_cli(th, len = 1, null.ok = FALSE, upper = 1, lower = 0, any.missing = FALSE)
  assert_numeric_cli(maxobservations, len = 1, null.ok = FALSE, any.missing = FALSE)
  assert_subset_cli(variables_selected, c(get_predictor_names(pred), "vif", "pca"), empty.ok = TRUE)
  assert_choice_cli(area, c("all", "occurrences"))

  if (is_input_sdm(pred)) {
    x <- pred$predictors
    occ <- pred$occurrences$occurrences
    epsg <- pred$occurrences$crs
  }

  if (is_sdm_area(x)) {
    if (is.null(variables_selected)) {
      variables_selected <- get_predictor_names(x)
    }
    facnum <- function(x) {
      return(as.numeric(as.factor(x)))
    }
    if (area == "all") {
      v <- x$grid |>
        as.data.frame() |>
        dplyr::select(-c("geometry", "cell_id")) |>
        dplyr::select(dplyr::all_of(variables_selected)) |>
        dplyr::mutate_if(is.character, facnum) |>
        usdm::vifcor(th = th, size = maxobservations)
    }
  }

  x$variable_selection$vif$area <- area
  x$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
  x$variable_selection$vif$threshold <- th
  x$variable_selection$vif$vifcor <- v
  if (is_input_sdm(pred)) {
    pred$predictors <- x
    x <- pred
  }
  return(x)
}

#' @rdname vif_predictors
#' @export
vif_summary <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok = FALSE)
  assert_subset_cli("vif", names(i$predictors$variable_selection), empty.ok = FALSE)
  return(i$predictors$variable_selection$vif$vifcor)
}

#' @rdname vif_predictors
#' @export
selected_variables <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok = FALSE)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok = FALSE)
  assert_subset_cli("vif", names(i$predictors$variable_selection), empty.ok = FALSE)
  return(i$predictors$variable_selection$vif$selected_variables)
}
