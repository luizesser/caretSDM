#' Prediction Change Analysis
#'
#' Provides an automate way for the visualization of projections gain, loss, and stability between different scenarios.
#'
#' @usage
#' prediction_change_sdm(i, scenario = NULL, ensemble_type = NULL, species = NULL, th = 0.5)
#'
#' @param i A \code{input_sdm} object with projections.
#' @param scenario Character. One of the scenarios that were projected. Can be ensembles as well.
#' @param ensemble_type Character. Type of ensemble to be used. Standard is NULL, but will return the mean_occ_prob
#' @param species Character. Species to be analyzed. Standard is NULL.
#' @param th Numeric. Threshold to binarize the ensemble.
#'
#' @returns A plot with comparison between current and other scenario.
#'
#'
#' @seealso \code{\link{species_names} \link{scenarios_names}}
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
#' # Ensemble GCMs:
#' i <- gcms_ensembles(i, gcms = c("ca", "mi"))
#' i
#'
#' # Change Analysis
#' prediction_change_sdm(i, scenario = "_ssp585_2090", ensemble_type = "mean_occ_prob")
#'
#' @importFrom tidyr replace_na
#' @importFrom dplyr full_join mutate select filter
#' @importFrom sf st_join
#' @importFrom mapview mapview
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_d xlab ylab ggtitle theme_minimal unit
#' @importFrom ggspatial north_arrow_fancy_orienteering annotation_north_arrow
#'
#' @global result_current result_future
#'
#' @export
prediction_change_sdm <- function(i, scenario = NULL, ensemble_type = NULL, species = NULL, th = 0.5) {
  assert_class_cli(i, "input_sdm")
  assert_choice_cli(scenario, colnames(get_ensembles(i)), null.ok = TRUE)
  if(is.null(scenario)){
    scenario <- sample(colnames(get_ensembles(i))[!colnames(get_ensembles(i)) %in%"current"], 1)
  }
  assert_choice_cli(ensemble_type, colnames(get_ensembles(i)[1,1][[1]]), null.ok = TRUE)
  if(is.null(ensemble_type)){
    ensemble_type <- "mean_occ_prob"
  }
  assert_choice_cli(species, colnames(get_ensembles(i)[1,1][[1]]), null.ok = TRUE)
  if(is.null(species)){
    species <- species_names(i)[1]
  }
  assert_numeric_cli(th, len = 1)
  cell_id <- get_ensembles(i)[[species, "current"]][, "cell_id"]
  v <- ifelse(get_ensembles(i)[[species, "current"]][, ensemble_type] > th, 1, 0)
  grd_current <- i$predictions$predictions[[grep("current", names(i$predictions$predictions))[1]]][[species]][[1]]
  grd_current <- dplyr::filter(grd_current, grd_current$cell_id == cell_id)
  grd_current[grd_current$cell_id %in% cell_id, "result"] <- v

  cell_id <- get_ensembles(i)[[species, scenario]][, "cell_id"]
  v <- ifelse(get_ensembles(i)[[species, scenario]][, ensemble_type] > th, 1, 0)
  grd_future <- i$predictions$predictions[[grep(scenario, names(i$predictions$predictions))[1]]][[species]][[1]]
  grd_future <- dplyr::filter(grd_future, grd_future$cell_id == cell_id)
  grd_future[grd_future$cell_id %in% cell_id, "result"] <- v

  grd <- sf::st_join(grd_current, grd_future, suffix = c("_current", "_future")) |>
    dplyr::select(c("result_current", "result_future")) |>
    dplyr::mutate(
      result_current = tidyr::replace_na(result_current, 0),
      result_future = tidyr::replace_na(result_future, 0),
      value = result_current + 2 * result_future
    ) |>
    dplyr::select(value) |>
    dplyr::mutate(value = factor(value,
                                 levels = c(0, 1, 2, 3),
                                 labels = c("Both absent",
                                            "Current only",
                                            "Projection only",
                                            "Both present")))

  p <- ggplot2::ggplot()
  if(!unique(sf::st_geometry_type(grd)) == "LINESTRING"){
    p <- p + ggplot2::geom_sf(data = grd, ggplot2::aes(fill = value), linewidth = 0.001) +
      ggplot2::scale_fill_viridis_d(name = paste0("Scenarios\n Changes"))
  } else {
    p <- p + ggplot2::geom_sf(data = grd, ggplot2::aes(color = value)) +
      ggplot2::scale_colour_viridis_c(name = paste0("Occurrence\n Probability"))
  }
  p <- p +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::ggtitle(paste0(species, " expected change in the ", scenario, " scenario")) +
    ggplot2::theme_minimal() +
    ggspatial::annotation_north_arrow(
      height = ggplot2::unit(1, "cm"),
      width = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering,
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.7, "cm")
    )

  return(p)
}

