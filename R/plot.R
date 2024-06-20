#' S3 Methods for plot
#'
#' This function creates different plots depending on the input.
#'
#' @usage fun(arg1, arg2 = default, ...)
#'
#' @param x Object to be plotted.
#'
#' @return A predictors object.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#' @import mapview
#' @import viridis
#' @import ggplot2
#' @import ggspatial
#' @import dplyr
#' @importFrom gtools mixedsort
#' @importFrom data.table rbindlist
#' @importFrom stringdist stringdist
#'

#' @exportS3Method base::plot
plot.occurrences <- function(x, spp_name = NULL, pa = TRUE) {
  df <- x$occurrences
  valid_spp <- unique(df$species)
  if (!is.null(spp_name)) {
    spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]
  } else {
    spp_name <- valid_spp[1]
  }
  df <- filter(df, species == spp_name)
  tmp <- plot(df["species"])
  return(tmp)
}

#' @rdname plot
#' @export
plot_occurrences <- function(i, spp_name = NULL, pa = TRUE) {
  return(plot(i$occurrences, spp_name, pa))
}

#' @exportS3Method base::plot
plot.predictors <- function(x, variables_selected = NULL) {
  st <- x$data
  if (variables_selected == "vif") {
    st <- st[, , x$variable_selection$vif$selected_variables]
  } else {
    st <- st[[variables_selected]]
  }
  tmp <- plot(st, col = viridis(1000))
  return(tmp)
}

#' @rdname plot
#' @export
plot_predictors <- function(i, variables_selected = NULL) {
  return(plot(i$predictors, variables_selected))
}

#' @exportS3Method base::plot
plot.models <- function(x) {
  ids <- x$validation$metrics
  alg <- x$algorithms
  ids <- as.data.frame(sapply(alg, function(x) {
    rownames(ids[ids$algo == x, ])
  }, USE.NAMES = T))
  models <- sapply(alg, function(a) {
    x$models[names(x$models) %in% ids$a]
  }, USE.NAMES = T)
  hyp <- as.vector(unlist(sapply(names(models), function(m) {
    colnames(models[[m]][[1]]$bestTune)
  }, USE.NAMES = T)))
  sa <- sapply(models, function(m) {
    sapply(m, function(m2) {
      cols <- colnames(m2$results) %in% c(hyp, "ROC")
      df <- m2$results[, cols]
      return(df)
    }, USE.NAMES = T)
  }, USE.NAMES = T)
  df <- lapply(sa, function(x) {
    as.data.frame(rbindlist(apply(x, 2, as.data.frame)))
  })
  tmp <- lapply(df, function(x) {
    if (ncol(x) > 2) {
      sd <- apply(x, 2, sd)
      sd["ROC"] <- 0
      for (n in names(which(sd != 0))) {
        tmp2 <- plot(x[, c(n, "ROC")])
      }
    } else {
      tmp2 <- plot(x)
    }
    return(tmp2)
  })
  return(tmp)
}

#' @exportS3Method base::plot
plot.predictions <- function(x, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE, ensemble_type = "mean_occ_prob") {
  valid_spp <- names(x$predictions[[1]])
  valid_scen <- names(x$predictions)
  ens <- ifelse(ensemble, "ensembles", "predictions")
  # valid_id
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
    v <- x[[ens]][[scenario]][[spp_name]][[id]]$presence
    grd <- filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
    subtitle <- NULL
  }
  p2 <- ggplot() +
    geom_sf(data = grd, aes(fill = result)) +
    scale_fill_viridis_c(name = paste0("Occurrence\n Probability"), limits = c(0, 1)) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(paste0(spp_name, " distribution in the ", scenario, " scenario"), subtitle = subtitle) +
    theme_minimal() +
    annotation_north_arrow(
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      style = north_arrow_fancy_orienteering,
      pad_x = unit(0.2, "cm"),
      pad_y = unit(0.7, "cm")
    ) +
    annotation_scale(height = unit(0.2, "cm"))

  return(p)
}

#' @rdname plot
#' @export
plot_predictions <- function(i, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE, ensemble_type = "mean_occ_prob") {
  return(plot(i$predictions, spp_name, scenario, id, ensemble, ensemble_type))
}

#' @exportS3Method mapview::mapview
mapview.occurrences <- function(x, spp_name, pa) {
  df <- x$occurrences
  if (!is.null(spp_name)) {
    df <- filter(df, species == spp_name)
  }
  if (pa == TRUE) {

  } else {

  }
  tmp <- mapview::mapview(df, zcol = "species", layer.name = "Species")
  return(tmp)
}

#' @exportS3Method mapview::mapview
mapview.predictors <- function(x) {
  st <- x$grid
  if ("variable_selection" %in% names(x)) {
    st <- st[[x$variable_selection$vif$selected_variables]]
  }
  tmp <- mapview(st_as_sf(x$data), burst = T, legend = F, hide = T)
  return(tmp)
}

#' @exportS3Method mapview::mapview
mapview.predictions <- function(x, spp_name = NULL, scenario = NULL, id = NULL, ensemble = TRUE, ensemble_type = "mean_occ_prob") {
  valid_spp <- names(x$predictions[[1]])
  valid_scen <- names(x$predictions)
  ens <- ifelse(ensemble, "ensembles", "predictions")
  # valid_id
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
    grd <- filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
  } else {
    v <- x[[ens]][[scenario]][[spp_name]][[id]]$presence
    grd <- filter(grd, grd$cell_id == cell_id)
    grd[grd$cell_id %in% cell_id, "result"] <- v
  }
  tmp <- mapview::mapview(grd, zcol = "result", layer.name = paste0(spp_name))
  return(tmp)
}
