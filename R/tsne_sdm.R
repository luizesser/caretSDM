#' tSNE
#'
#' This function calculates tSNE with presences and pseudoabsences data and returns a list of plots.
#'
#' @usage tsne_sdm(occ, pred = NULL, variables_selected = NULL)
#'
#' @param occ A \code{occurrences} or \code{input_sdm} object.
#' @param pred A \code{predictors} object. If \code{occ} is of class \code{input_sdm}, then
#' \code{pred} is retrieved from it.
#' @param variables_selected Variable to be used in t-SNE. It can also be 'vif', if previously calculated.
#'
#' @return A list of plots, where each plot is a tSNE for a given pseudoabsence dataset.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom ggplot2 ggplot aes xlab ylab ggtitle geom_point scale_color_manual
#' @importFrom raster extract
#' @importFrom Rtsne Rtsne
#' @importFrom dplyr select filter all_of
#' @importFrom sf st_as_sf
#'
#' @global V1 V2
#'
#' @export
tsne_sdm <- function(occ, pred = NULL, variables_selected = NULL) {
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    pred <- occ$predictors
  } else {
    y <- occ
    if (is.null(pred)) {
      stop(warning("pred not found"))
    }
  }
  if (is.null(variables_selected)) {
    variables_selected <- get_predictor_names(occ)
  } else if ("vif" %in% variables_selected) {
    variables_selected <- pred$variable_selection$vif$selected_variables
  } else if ("pca" %in% variables_selected) {
    variables_selected <- pred$variable_selection$pca$selected_variables
  }

  tsne_sp <- sapply(species_names(occ), function(sp) {
    pa_id <- lapply(y$pseudoabsences$data[[sp]], function(x) {
      x$cell_id
    })
    p <- y$occurrences[y$occurrences$species == sp, ]$cell_id
    env <- pred$grid
    p <- dplyr::filter(env, env$cell_id %in% p)
    Presence <- c(rep("Presence", nrow(p)), rep("Pseudoabsence", length(pa_id[[1]])))

    df_tsne <- lapply(pa_id, function(id) {
      pa <- dplyr::filter(env, env$cell_id %in% id)
      df <- rbind(p, pa)
      df <- cbind(Presence, df)
    })

    perp <- round((nrow(df_tsne[[1]])^(1 / 2)), digits = 0)
    plot_list <- lapply(df_tsne, function(ts) {
      ts2 <- dplyr::select(as.data.frame(ts), dplyr::all_of(variables_selected))
      ts2 <- as.matrix(ts2[, variables_selected])
      tsne_bg <- Rtsne::Rtsne(ts2, perplexity = perp)
      df <- as.data.frame(tsne_bg$Y)
      tsne_result <- ggplot2::ggplot(df, ggplot2::aes(x = V1, y = V2)) +
        ggplot2::xlab("tSNE Dim 1") +
        ggplot2::ylab("tSNE Dim 2") +
        ggplot2::ggtitle("Presences and Pseudoabsences's t-SNE") +
        ggplot2::geom_point(ggplot2::aes(col = Presence)) +
        ggplot2::scale_color_manual(values = c("gold", "darkblue"))
    })
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(tsne_sp)
}
