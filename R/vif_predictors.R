#' Calculate VIF
#'
#' Apply Variance Inflation Factor (VIF) calculation.
#'
#' @usage vif_predictors(pred, th = 0.5, maxobservations = 5000, variables_selected = NULL)
#'
#' @param pred A \code{input_sdm} or \code{predictors} object.
#' @param th Threshold to be applied in VIF routine.
#' @param maxobservations Max observations to use to calculate the VIF.
#' @param variables_selected If there is a subset of predictors that should be used in this
#' function, it can be informed using this parameter.
#'
#' @return A \code{input_sdm} or \code{predictors} object with VIF data.
#'
#' @seealso \code{\link{usdm::vifcor}\link{predictors_names}}
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
#' i <- vif_predictors(i)
#'
#' @importFrom usdm vifcor
#' @importFrom dplyr filter select mutate_if all_of
#' @importFrom sf st_centroid st_as_sf
#' @importFrom stars st_extract
#'
#' @export
vif_predictors <- function(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected = NULL) {
  assert_class_cli(pred, "input_sdm")
  assert_subset_cli("predictors", names(pred), empty.ok=F)
  assert_class_cli(pred$predictors, "sdm_area")
  assert_class_cli(pred, "input_sdm")
  assert_numeric_cli(th, len=1, null.ok=FALSE, upper=1, lower=0, any.missing=FALSE)
  assert_numeric_cli(maxobservations, len=1, null.ok=FALSE, any.missing=FALSE)
  assert_subset_cli(variables_selected, c(get_predictor_names(pred), "vif", "pca"), empty.ok=T)
  assert_choice_cli(area, c("all", "occurrences"))

  if (is_input_sdm(pred)) {
    x <- pred$predictors
    occ <- pred$occurrences$occurrences
    epsg <- pred$occurrences$crs
  }

  #if (is_predictors(x)) {
  #  if (is.null(variables_selected)) {
  #    selected_vars <- x$predictors_names
  #    cat(cat("Using all variables available: "), cat(selected_vars, sep = ", "))
  #  } else if (any(variables_selected %in% x$predictors_names)) {
  #    selected_vars <- x$predictors_names[x$predictors_names %in% variables_selected]
  #    cat(cat("Using given variables: "), cat(selected_vars, sep = ", "))
  #  }
  #  if (area == "all") {
  #    suppressWarnings(sf_x <- sf::st_centroid(sf::st_as_sf(dplyr::filter(x$data, band %in% selected_vars))))
  #    p <- select(as.data.frame(sf_x), -"geometry")
  #  }
  #  if (area == "occurrences") {
  #    if (!is_input_sdm(pred)) {
  #      stop("Method only available with input_sdm class.")
  #    }
  #    #cols <- find_columns(occ$occurrences)
  #    #coordinates(occ$occurrences) <- cols[2:3]
  #    #st_crs(occ) <- as.character(st_crs(epsg))[1]
  #    p <- sf::st_extract(as.data.frame(x$data[selected_vars, ])[, selected_vars], sf::st_as_sf(occ))
  #  }
  #  v <- usdm::vifcor(p, th = th, size = maxobservations)
  #} else
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
  assert_subset_cli("predictors", names(i), empty.ok=F)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok=F)
  assert_subset_cli("vif", names(i$predictors$variable_selection), empty.ok=F)
  return(i$predictors$variable_selection$vif$vifcor)
}

#' @rdname vif_predictors
#' @export
selected_variables <- function(i){
  assert_class_cli(i, "input_sdm")
  assert_subset_cli("predictors", names(i), empty.ok=F)
  assert_subset_cli("variable_selection", names(i$predictors), empty.ok=F)
  assert_subset_cli("vif", names(i$predictors$variable_selection), empty.ok=F)
  return(i$predictors$variable_selection$vif$selected_variables)
}
