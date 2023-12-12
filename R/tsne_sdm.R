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
tsne_sdm <- function(occ, pred, selected_vars=NULL){
  if(class(occ)=='input_sdm'){
    y <- occ$occurrences
    pred <- occ$predictors
  } else {
    y <- occ
  }
  if(is.null(selected_vars)){selected_vars <- names(pred$grid)} else {
    if(selected_vars=='vif'){selected_vars <- pred$variable_selection$vif$selected_variables}
  }

  pa <- lapply(y$pseudoabsences$data, function(x){select(x, all_of(selected_vars))})
  p <- i$occurrences$occurrences
  cols <- find_columns(p)
  coordinates(p) <- cols[2:3]
  p <- extract(pred$grid[[selected_vars]], p)
  df_tsne <- lapply(pa, function(x){
    df <- rbind(p, x)
    Presence <- c(rep(1, nrow(p)),rep(0, nrow(x)))
    df <- cbind(Presence,df)})

  perp = round((nrow(df_tsne[[1]]) ^ (1/2)), digits=0)

  Groups <- c(rep("Presence", nrow(p)),rep("Pseudoabsence", nrow(pa[[1]])))

  plot_list <- lapply(df_tsne, function(ts){
    tsne_bg <- Rtsne(as.matrix(ts), perplexity = perp)
    df <- as.data.frame(tsne_bg$Y)
    tsne_result <- ggplot(df, aes(x=V1,y=V2)) +
      xlab("tSNE Dim 1") +
      ylab("tSNE Dim 2") +
      ggtitle("Presences and Pseudoabsences's t-SNE") +
      geom_point(aes(col=Groups)) +
      scale_color_manual(values=c("gold", "darkblue"))
  })

  return(plot_list)
}
