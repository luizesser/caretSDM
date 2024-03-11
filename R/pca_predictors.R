#' Calculate PCA
#'
#' Apply PCA calculation in a predictors object.
#'
#' @param pred A predictors object
#' @param variables_selected Subset of variables to be included in the PCA
#'
#' @return A predictors object with PCA data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import cubelyr
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @importFrom usdm vifcor
#'
#' @export
pca_predictors <- function(pred, variables_selected=NULL){
  if(class(pred)=='input_sdm'){
    x <- pred$predictors
    occ <- pred$occurrences$occurrences
  } else {
    x <- pred
  }
  if(is.null(variables_selected)){
    selected_vars <- x$predictors_names
    print(cat('Using all variables available: '), cat(selected_vars, sep=', '))
  }
  if(any(variables_selected %in% x$predictors_names)){
    selected_vars <- x$predictors_names[x$predictors_names %in% variables_selected]
    print(cat('Using given variables: '), cat(selected_vars, sep=', '))
  }




  x$variable_selection$pca$model <- pc_model
  x$variable_selection$pca$data <- pc_data
  if(class(pred)=='input_sdm'){
    pred$predictors <- x
    x <- pred
  }
  return(x)
}
