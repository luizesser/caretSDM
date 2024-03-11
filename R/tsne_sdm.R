#' tSNE
#'
#' This function calculates tSNE with presences and pseudoabsences data and returns a list of plots.
#'
#' @param occ A occurrences object or an input_sdm object.
#' @param pred A predictors object. If occ is an input_sdm object, then pred is retrieved from it.
#' @param selected_vars Variable to be used in t-SNE. It can also be 'vif', if previously calculated.
#'
#' @return A plot to each pseudoabsence dataset in form of a list.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import ggplot2
#' @importFrom raster extract
#' @importFrom Rtsne Rtsne
#'
#' @export
tsne_sdm <- function(occ, pred=NULL, variables_selected=NULL){
  if(class(occ)=='input_sdm'){
    y <- occ$occurrences
    pred <- occ$predictors
  } else {
    y <- occ
    if(is.null(pred)){
      stop(warning('pred not found'))
    }
  }
  if(is.null(variables_selected)){variables_selected <- names(pred$grid)} else {
    if(variables_selected=='vif'){variables_selected <- pred$variable_selection$vif$selected_variables}
  }
  tsne_sp <- sapply(y$spp_names,function(sp){
    pa_id <- lapply(y$pseudoabsences$data[[sp]], function(x){x$cell_id})
    p <- y$occurrences[y$occurrences$species==sp,]$cell_id
    env <- select(cbind(pred$grid,st_as_sf(pred$data)),-'geometry.1')
    p <- filter(env, env$cell_id %in% p)
    Presence <- c(rep('Presence', nrow(p)),rep('Pseudoabsence', length(pa_id[[1]])))

    df_tsne <- lapply(pa_id, function(id){
      pa <- filter(env, env$cell_id %in% id)
      df <- rbind(p, pa)
      df <- cbind(Presence,df)})

    perp = round((nrow(df_tsne[[1]]) ^ (1/2)), digits=0)
    plot_list <- lapply(df_tsne, function(ts){
      ts2 <- select(as.data.frame(ts),all_of(variables_selected))
      ts2 <- as.matrix(ts2[,variables_selected])
      tsne_bg <- Rtsne(ts2, perplexity = perp)
      df <- as.data.frame(tsne_bg$Y)
      tsne_result <- ggplot(df, aes(x=V1,y=V2)) +
        xlab("tSNE Dim 1") +
        ylab("tSNE Dim 2") +
        ggtitle("Presences and Pseudoabsences's t-SNE") +
        geom_point(aes(col=Presence)) +
        scale_color_manual(values=c("gold", "darkblue"))
    })
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(tsne_sp)
}
