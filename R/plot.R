#' S3 Methods for plot and mapview
#'
#' This function creates different plots depending on the input.
#'
#' @usage
#' plot_occurrences(i, spp_name = NULL, pa = TRUE)
#'
#' @param i Object to be plotted. Can be a \code{input_sdm}, but also \code{occurrences} or
#' \code{sdm_area}.
#' @param spp_name A character with species to be plotted. If NULL, the first species is plotted.
#' @param pa Boolean. Should pseudoabsences be plotted together? (not implemented yet.)
#' @param variables_selected A character vector with names of variables to be plotted.
#' @param scenario description
#' @param id The id of models to be plotted (only used when \code{ensemble = FALSE}). Possible
#' values are row names of get_validation_metrics(i).
#' @param ensemble Boolean. Should the ensemble be plotted (TRUE)? Otherwise a prediction will be
#' plotted
#' @param ensemble_type Character of the type of ensemble to be plotted. One of: "mean_occ_prob",
#' "wmean_AUC" or "committee_avg"
#' @param raster Should the niche be extrapolated to a raster covering all possibe values in the
#' environmental space?
#'
#' @details We implemented a bestiary of plots to help visualizing the process and results. If you
#' are not familiar with mapview, consider using it to better visualize maps.
#'
#' @return The plot or mapview desired.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom mapview mapview
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c xlab ylab ggtitle theme_minimal unit
#' scale_color_viridis_c geom_point scale_y_continuous scale_x_continuous stat_density_2d_filled
#' after_stat stat_summary_2d
#' @importFrom ggspatial annotation_scale north_arrow_fancy_orienteering annotation_north_arrow
#' @importFrom dplyr filter select all_of
#' @importFrom gtools mixedsort
#' @importFrom data.table rbindlist
#' @importFrom stringdist stringdist
#' @importFrom sf st_as_sf st_geometry_type
#' @importFrom tidyr pivot_longer
#'
#' @global species result value var1 var2 density
#'
#' @export
plot_occurrences <- function(i, spp_name = NULL, pa = TRUE) {
  assert_subset_cli(class(i), c("occurrences", "input_sdm"))
  assert_subset_cli(spp_name, species_names(i))
  assert_logical_cli(pa)
  assert_subset_cli("occurrences", names(i))
  if(is_input_sdm(i)){
      return(plot(i$occurrences, spp_name, pa))
  } else if (is_occurrences(i)){
    return(plot(i, spp_name, pa))
  }
}

#' @exportS3Method base::plot
plot.occurrences <- function(x, spp_name = NULL, pa = TRUE, ...) {
  grd <- x$occurrences
  valid_spp <- species_names(x)
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }
  grd <- dplyr::filter(grd, species %in% spp_name) |> dplyr::select(species)

  tmp <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = grd, ggplot2::aes(fill = species)) +
    ggplot2::scale_fill_viridis_d(name = paste0("Species")) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Species occurrence records") +
    ggplot2::theme_minimal() +
    ggspatial::annotation_north_arrow(
      height = ggplot2::unit(1, "cm"),
      width = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering,
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.7, "cm")
    ) +
    ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"))
  return(tmp)
}

#' @rdname plot_occurrences
#' @export
plot_grid <- function(i) {
  assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  if(is_input_sdm(i)){
    i <- i$predictors
  }
  return(plot(i, scenario="grid"))
}

#' @rdname plot_occurrences
#' @export
plot_predictors <- function(i, variables_selected = NULL) {
  assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  if (is_input_sdm(i)){
    return(plot(i$predictors, variables_selected, scenario="predictors"))
  } else if (is_sdm_area(i)) {
    return(plot(i, variables_selected, scenario="predictors"))
  }
}

#' @rdname plot_occurrences
#' @export
plot_scenarios <- function(i, variables_selected = NULL, scenario = NULL) {
  assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  assert_subset_cli(scenario, scenarios_names(i))
  assert_subset_cli("predictors", names(i))
  return(plot(i$scenarios, variables_selected, scenario))
}

#' @exportS3Method base::plot
plot.sdm_area <- function(x, variables_selected = NULL, scenario = NULL, ...) {
  if(is.null(scenario)){
    scenario <- scenarios_names(x)[1]
  }
  if(scenario=="grid"){
    tmp <- ggplot2::ggplot(x$grid)
    if(!unique(sf::st_geometry_type(x$grid)) == "LINESTRING"){
      tmp <- tmp + ggplot2::geom_sf(data=x$grid, ggplot2::aes(fill = cell_id)) +
        ggplot2::scale_fill_viridis_c()
    } else {
      tmp <- tmp + ggplot2::geom_sf(data=x$grid, ggplot2::aes(color = cell_id)) +
        ggplot2::scale_colour_viridis_c()
    }
    tmp <- tmp +
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Latitude") +
      ggplot2::ggtitle("Grid built using sdm_area()") +
      ggplot2::theme_minimal() +
      ggspatial::annotation_north_arrow(
        height = ggplot2::unit(1, "cm"),
        width = ggplot2::unit(1, "cm"),
        style = ggspatial::north_arrow_fancy_orienteering,
        pad_x = ggplot2::unit(0.2, "cm"),
        pad_y = ggplot2::unit(0.7, "cm")
      ) +
      ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"))
    return(tmp)
  }

  if ("vif" %in% variables_selected) {
    variables_selected <- x$variable_selection$vif$selected_variables
  } else if ("pca" %in% variables_selected) {
    variables_selected <- x$variable_selection$pca$selected_variables
  } else if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(x)
  }

 if(scenario=="predictors"){
    st <- x$grid |> dplyr::select(dplyr::all_of(variables_selected))
    teste <- tidyr::pivot_longer(st, dplyr::all_of(variables_selected))

    tmp <- ggplot2::ggplot(teste) +
      ggplot2::facet_grid(. ~ name) +
      ggplot2::geom_sf(data = teste, ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Latitude") +
      ggplot2::ggtitle("Predictor variables") +
      ggplot2::theme_minimal() +
      ggspatial::annotation_north_arrow(
        height = ggplot2::unit(1, "cm"),
        width = ggplot2::unit(1, "cm"),
        style = ggspatial::north_arrow_fancy_orienteering,
        pad_x = ggplot2::unit(0.2, "cm"),
        pad_y = ggplot2::unit(0.7, "cm")
      ) +
      ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"))
  } else {
    st <- x$data[[scenario]] |> dplyr::select(dplyr::all_of(variables_selected))
    teste <- tidyr::pivot_longer(st, dplyr::all_of(variables_selected))

    tmp <- ggplot2::ggplot(teste) +
      ggplot2::facet_grid(. ~ name) +
      ggplot2::geom_sf(data = teste, ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Latitude") +
      ggplot2::ggtitle(paste0("Predictor variables for ", scenario," scenario")) +
      ggplot2::theme_minimal() +
      ggspatial::annotation_north_arrow(
        height = ggplot2::unit(1, "cm"),
        width = ggplot2::unit(1, "cm"),
        style = ggspatial::north_arrow_fancy_orienteering,
        pad_x = ggplot2::unit(0.2, "cm"),
        pad_y = ggplot2::unit(0.7, "cm")
      ) +
      ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"))
  }

  return(tmp)
}

#' @rdname plot_occurrences
#' @export
plot_predictions <- function(i, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE,
                             ensemble_type = "mean_occ_prob") {
  assert_class_cli(i, "input_sdm")
  assert_subset_cli(spp_name, species_names(i))
  assert_subset_cli("predictions", names(i))
  return(plot(i$predictions, spp_name, scenario, id, ensemble, ensemble_type))
}

#' @exportS3Method base::plot
plot.predictions <- function(x, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE,
                             ensemble_type = "mean_occ_prob", ...) {
  ens <- ifelse(ensemble, "ensembles", "predictions")
  valid_spp <- names(x$predictions[[1]])
  if (ensemble) {
    valid_scen <- x[[ens]] |> colnames()
  } else {
    valid_scen <- names(x$predictions)
  }

  if (!is.null(scenario)) {
    scenario <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]
  } else {
    scenario <- valid_scen[1]
  }
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }

  grd <- x$predictions[[scenario]][[spp_name]][[1]]
  while (is.null(grd)) {
    valid_scen <- valid_scen[! valid_scen %in% scenario]
    scenario2 <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]
    grd <- x$predictions[[scenario2]][[spp_name]][[1]]
  }

  if (ensemble) {
    cell_id <- x[[ens]][[spp_name, scenario]][, "cell_id"]
    v <- x[[ens]][[spp_name, scenario]][, ensemble_type]
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
    if (ensemble_type == "mean_occ_prob") {
      et <- "Mean Occurrence Probability"
    }
    if (ensemble_type == "wmean_AUC") {
      et <- "Mean Occurrence Probability Weighted by ROC/AUC"
    }
    if (ensemble_type == "committee_avg") {
      et <- "Committee Average"
    }
    subtitle <- paste0("Ensemble type: ", et)
  } else {
    cell_id <- x[["predictions"]][[scenario]][[spp_name]][[1]]$cell_id
    v <- x[[ens]][[scenario]][[spp_name]][[id]]$presence
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
    subtitle <- NULL
  }
  p <- ggplot2::ggplot()
  if(!unique(sf::st_geometry_type(grd)) == "LINESTRING"){
    p <- p + ggplot2::geom_sf(data = grd, ggplot2::aes(fill = result), linewidth = 0.001) +
      ggplot2::scale_fill_viridis_c(name = paste0("Occurrence\n Probability"), limits = c(0, 1))
  } else {
    p <- p + ggplot2::geom_sf(data = grd, ggplot2::aes(color = result)) +
      ggplot2::scale_colour_viridis_c(name = paste0("Occurrence\n Probability"), limits = c(0, 1))
  }
  p <- p +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::ggtitle(paste0(spp_name, " distribution in the ", scenario, " scenario"), subtitle = subtitle) +
    ggplot2::theme_minimal() +
    ggspatial::annotation_north_arrow(
      height = ggplot2::unit(1, "cm"),
      width = ggplot2::unit(1, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering,
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.7, "cm")
    ) #+
    #ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"), )

  return(p)
}

#' @rdname plot_occurrences
#' @export
mapview_grid <- function(i) {
  assert_subset_cli(class(i), c("sdm_area", "input_sdm"))

  if(is_input_sdm(i)) {
    x <- i$predictors
  } else if(is_sdm_area(i)) {
    x <- i
  }

  grd <- x$grid |> dplyr::select("cell_id")

  mapview::mapview(grd, zcol = "cell_id", layer.name = "cell_id")
}

#' @rdname plot_occurrences
#' @export
mapview_occurrences <- function(i, spp_name = NULL, pa = TRUE) {
  assert_subset_cli(class(i), c("occurrences", "input_sdm"))
  assert_subset_cli(spp_name, species_names(i))
  assert_logical_cli(pa)
  assert_subset_cli("occurrences", names(i))

  if(is_input_sdm(i)) {
    x <- i$occurrences
  } else if(is_occurrences(i)) {
    x <- i
  }
  valid_spp <- species_names(x)
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }

  grd <- x$occurrences
  grd <- dplyr::filter(grd, species %in% spp_name) |> dplyr::select(species)

  mapview::mapview(grd, zcol = "species", layer.name = "Species")
}

#' @rdname plot_occurrences
#' @export
mapview_predictors <- function(i, variables_selected = NULL) {
  assert_subset_cli(class(i), c("input_sdm", "sdm_area"))
  assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))

  if(is_input_sdm(i)){
    x <- i$predictors
  } else if(is_sdm_area(i)){
    x <- i
  }

  if ("vif" %in% variables_selected) {
    variables_selected <- x$variable_selection$vif$selected_variables
  } else if ("pca" %in% variables_selected) {
    variables_selected <- x$variable_selection$pca$selected_variables
  } else if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(x)[1]
  }

  st <- x$grid |> dplyr::select(dplyr::all_of(variables_selected))
  mapview::mapview(st, layer.name=variables_selected)
}

#' @rdname plot_occurrences
#' @export
mapview_scenarios <- function(i, variables_selected = NULL, scenario = NULL) {
  assert_subset_cli(class(i), c("input_sdm", "sdm_area"))
  assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  assert_subset_cli(scenario, scenarios_names(i))

  x <- i$scenarios

  if ("vif" %in% variables_selected) {
    variables_selected <- x$variable_selection$vif$selected_variables
  } else if ("pca" %in% variables_selected) {
    variables_selected <- x$variable_selection$pca$selected_variables
  } else if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(x)[1]
  }

  if(is.null(scenario)){
    scenario <- scenarios_names(x)[1]
  }
  st <- x$data[[scenario]] |> dplyr::select(dplyr::all_of(variables_selected))
  mapview::mapview(st, layer.name = paste0(scenario," ",variables_selected))
}

#' @rdname plot_occurrences
#' @export
mapview_predictions <- function(i, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE, ensemble_type = "mean_occ_prob") {
  assert_class_cli(i, "input_sdm")

  x <- i$predictions
  valid_spp <- names(x$predictions[[1]])
  valid_scen <- names(x$predictions)
  ens <- ifelse(ensemble, "ensembles", "predictions")
  grd <- x$grid
  if (!is.null(scenario)) {
    scenario <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]
  } else {
    scenario <- valid_scen[1]
  }
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }
  cell_id <- x[["predictions"]][[scenario]][[spp_name]][[1]]$cell_id
  if (ensemble) {
    v <- x[[ens]][[spp_name, scenario]][, ensemble_type]
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
  } else {
    v <- x[[ens]][[scenario]][[spp_name]][[id]]$presence
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
  }
  mapview::mapview(grd, zcol = "result", layer.name = paste0(spp_name))
}

#' @exportS3Method base::plot
plot.input_sdm <- function(x, ...) {
  i <- x
  assert_class_cli(i, "input_sdm")
  if ("predictions" %in% names(i)) {
    return(plot_predictions(i))
  }
  #if ("scenarios" %in% names(i)) {
  #  return(plot_scenarios(i))
  #}
  if ("predictors" %in% names(i)) {
    return(plot_predictors(i))
  }
  if ("occurrences" %in% names(i)) {
    return(plot_occurrences(i))
  }
}

#' @rdname plot_occurrences
#' @export
plot_background <- function(i, variables_selected = NULL) {
  assert_class_cli(i, "input_sdm")
  pred <- i$predictors$grid

  if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(i)
  } else if ("vif" %in% variables_selected) {
    variables_selected <- selected_variables(i)
  } else if ("pca" %in% variables_selected) {
    variables_selected <- selected_variables(i)
  } else {
    assert_choice_cli(variables_selected, get_predictor_names(i))
  }

  pred_df <- as.data.frame(pred)[,variables_selected]
  colnames(pred_df) <- c("var1", "var2")
  background <- ggplot2::ggplot(pred_df, ggplot2::aes(x = var1,
                                                      y = var2,
                                                      fill = ggplot2::after_stat(density))) +
    ggplot2::stat_density_2d_filled(bins=50, show.legend = FALSE, contour = FALSE, geom="raster") +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::ggtitle("Background Data for the Study Area") +
    ggplot2::xlab(variables_selected[1]) +
    ggplot2::ylab(variables_selected[2]) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal()
  return(background)
}

#' @rdname plot_occurrences
#' @export
plot_niche <- function(i, spp_name = NULL, variables_selected = NULL, scenario = NULL, id = NULL,
                       ensemble = TRUE, ensemble_type = "mean_occ_prob", raster = FALSE) {
  assert_logical_cli(ensemble)
  assert_logical_cli(raster)
  ens <- ifelse(ensemble, "ensembles", "predictions")
  assert_class_cli(i$predictions, "predictions")

  if (ensemble) {
    valid_scen <- i$predictions[[ens]] |> colnames()
  } else {
    valid_scen <- scenarios_names(i)
  }

  if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(i)
  } else if (any(c("vif", "pca") %in% variables_selected)) {
    variables_selected <- selected_variables(i)
  } else {
    assert_subset_cli(variables_selected, get_predictor_names(i))
  }
  if (!is.null(scenario)) {
    scenario <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]
  } else {
    scenario <- valid_scen[1]
  }
  valid_spp <- species_names(i)
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }

  grd <- i$predictions$predictions[[scenario]][[spp_name]][[1]]
  while (is.null(grd)) {
    valid_scen <- valid_scen[! valid_scen %in% scenario]
    scenario2 <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]
    grd <- i$predictions$predictions[[scenario2]][[spp_name]][[1]]
  }

  if (ensemble) {
    cell_id <- i$predictions[[ens]][[spp_name, scenario]][, "cell_id"]
    v <- i$predictions[[ens]][[spp_name, scenario]][, ensemble_type]
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
    if (ensemble_type == "mean_occ_prob") {
      et <- "Mean Occurrence Probability"
    }
    if (ensemble_type == "wmean_AUC") {
      et <- "Mean Occurrence Probability Weighted by ROC/AUC"
    }
    if (ensemble_type == "committee_avg") {
      et <- "Committee Average"
    }
    subtitle <- paste0("Ensemble type: ", et)
  } else {
    cell_id <- i$predictions[["predictions"]][[scenario]][[spp_name]][[1]]$cell_id
    v <- i$predictions[[ens]][[scenario]][[spp_name]][[id]]$presence
    grd <- dplyr::filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
    subtitle <- NULL
  }
  grd2 <- as.data.frame(grd)[,c(variables_selected, "result")]
  colnames(grd2) <- c("var1", "var2", "result")
  # As a raster:
  if(raster) {
    p <- ggplot2::ggplot() +
      ggplot2::stat_summary_2d(
        data = grd2,
        ggplot2::aes(x = var1, y = var2, z = result),
        fun = mean,  # Averages presence probability
        geom = "raster"
      ) +
      ggplot2::scale_fill_viridis_c(name = "Presence Probability") +
      ggplot2::xlab(variables_selected[1]) +
      ggplot2::ylab(variables_selected[2]) +
      ggplot2::ggtitle(paste0(spp_name, " Niche for the ", scenario, " scenario"), subtitle = subtitle) +
      ggplot2::theme_minimal()
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = grd2,
                          ggplot2::aes(x = var1, y = var2, color = result),
                          size = 3,
                          alpha = 0.4,
                          stroke = 1) +
      ggplot2::scale_color_viridis_c(name = "Presence Probability") +
      ggplot2::xlab(variables_selected[1]) +
      ggplot2::ylab(variables_selected[2]) +
      ggplot2::ggtitle(paste0(spp_name, " Niche for the ", scenario, " scenario"), subtitle = subtitle) +
      ggplot2::theme_minimal()
  }
  return(p)
}
