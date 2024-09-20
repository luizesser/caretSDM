#' S3 Methods for plot
#'
#' This function creates different plots depending on the input.
#'
#' @usage
#' plot_occurrences(i, spp_name = NULL, pa = TRUE)
#'
#' @param i Object to be plotted.
#'
#' @return A predictors object.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#' @importFrom mapview mapview
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c xlab ylab ggtitle theme_minimal unit
#' @importFrom ggspatial annotation_scale north_arrow_fancy_orienteering annotation_north_arrow
#' @importFrom dplyr filter
#' @importFrom gtools mixedsort
#' @importFrom data.table rbindlist
#' @importFrom stringdist stringdist
#' @importFrom sf st_as_sf
#' @importFrom tidyr pivot_longer
#'
#' @export
plot_occurrences <- function(i, spp_name = NULL, pa = TRUE) {
  caretSDM:::assert_subset_cli(class(i), c("occurrences", "input_sdm"))
  caretSDM:::assert_subset_cli(spp_name, species_names(i))
  caretSDM:::assert_logical_cli(pa)
  caretSDM:::assert_subset_cli("occurrences", names(i))
  if(is_input_sdm(i)){
      return(plot(i$occurrences, spp_name, pa))
  } else if (is_occurrences(i)){
    return(plot(i, spp_name, pa))
  }
}

#' @exportS3Method base::plot
plot.occurrences <- function(x, spp_name = NULL, pa = TRUE) {
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
  caretSDM:::assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  if(is_input_sdm(i)){
    i <- i$predictors
  }
  return(plot(i, scenario="grid"))
}

#' @rdname plot_occurrences
#' @export
plot_predictors <- function(i, variables_selected = NULL) {
  caretSDM:::assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  caretSDM:::assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  if (is_input_sdm(i)){
    return(plot(i$predictors, variables_selected, scenario="predictors"))
  } else if (is_sdm_area(i)) {
    return(plot(i, variables_selected, scenario="predictors"))
  }
}

#' @rdname plot_occurrences
#' @export
plot_scenarios <- function(i, variables_selected = NULL, scenario = NULL) {
  caretSDM:::assert_subset_cli(class(i), c("sdm_area", "input_sdm"))
  caretSDM:::assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  caretSDM:::assert_subset_cli(scenario, scenarios_names(i))
  caretSDM:::assert_subset_cli("predictors", names(i))
  return(plot(i$scenarios, variables_selected, scenario))
}

#' @exportS3Method base::plot
plot.sdm_area <- function(x, variables_selected = NULL, scenario = NULL) {
  if(scenario=="grid"){
    tmp <- ggplot2::ggplot(x$grid) +
      ggplot2::geom_sf(data=x$grid, ggplot2::aes(fill = cell_id)) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Latitude") +
      ggplot2::ggtitle("Grid built using study_area()") +
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
    st <- x$grid |> dplyr::select(all_of(variables_selected))
    teste <- tidyr::pivot_longer(st, all_of(variables_selected))

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
    if(is.null(scenario)){
      scenario <- scenarios_names(x)[1]
    }
    st <- x$data[[scenario]] |> dplyr::select(all_of(variables_selected))
    teste <- tidyr::pivot_longer(st, all_of(variables_selected))

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
  caretSDM:::assert_class_cli(i, "input_sdm")
  caretSDM:::assert_subset_cli(spp_name, species_names(i))
  #caretSDM:::assert_subset_cli(scenario, scenarios_names(i))
  caretSDM:::assert_subset_cli("predictions", names(i))
  return(plot(i$predictions, spp_name, scenario, id, ensemble, ensemble_type))
}

#' @exportS3Method base::plot
plot.predictions <- function(x, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE,
                             ensemble_type = "mean_occ_prob") {
  ens <- ifelse(ensemble, "ensembles", "predictions")
  valid_spp <- names(x$predictions[[1]])
  if (ensemble) {
    valid_scen <- x[[ens]] |> colnames()
  } else {
    valid_scen <- names(x$predictions)
  }

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
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = grd, ggplot2::aes(fill = result)) +
    ggplot2::scale_fill_viridis_c(name = paste0("Occurrence\n Probability"), limits = c(0, 1)) +
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
    ) +
    ggspatial::annotation_scale(height = ggplot2::unit(0.2, "cm"))

  return(p)
}

#' @rdname plot_occurrences
#' @export
mapview_grid <- function(i) {
  caretSDM:::assert_subset_cli(class(i), c("sdm_area", "input_sdm"))

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
  caretSDM:::assert_subset_cli(class(i), c("occurrences", "input_sdm"))
  caretSDM:::assert_subset_cli(spp_name, species_names(i))
  caretSDM:::assert_logical_cli(pa)
  caretSDM:::assert_subset_cli("occurrences", names(i))

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
  caretSDM:::assert_subset_cli(class(i), c("input_sdm", "sdm_area"))
  caretSDM:::assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))

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

  st <- x$grid |> dplyr::select(all_of(variables_selected))
  mapview::mapview(st, layer.name=variables_selected)
}

#' @rdname plot_occurrences
#' @export
mapview_scenarios <- function(i, variables_selected = NULL, scenario = NULL) {
  caretSDM:::assert_subset_cli(class(i), c("input_sdm", "sdm_area"))
  caretSDM:::assert_subset_cli(variables_selected, c(get_predictor_names(i), "vif", "pca"))
  caretSDM:::assert_subset_cli(scenario, scenarios_names(i))

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
    variables_selected <- get_predictor_names(x)
  }

  if(is.null(scenario)){
    scenario <- scenarios_names(x)[1]
  }
  st <- x$data[[scenario]] |> dplyr::select(all_of(variables_selected))
  mapview::mapview(st, layer.name = paste0(scenario,"\n",variables_selected))
}

#' @rdname plot_occurrences
#' @export
mapview_predictions <- function(i, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE, ensemble_type = "mean_occ_prob") {
  caretSDM:::assert_class_cli(i, "input_sdm")

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
plot.input_sdm <- function(i) {
  caretSDM:::assert_class_cli(i, "input_sdm")
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
