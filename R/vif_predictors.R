#' Calculate VIF
#'
#' Apply VIF calculation in a predictors object.
#'
#' @param pred A predictors object
#' @param th Threshold
#' @param maxobservations Number of sets of pseudoabsence to create
#'
#' @return A predictors object with VIF data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom usdm vifcor
#' @export
vif_predictors <- function(pred, th=0.5, maxobservations=5000, variables_selected=NULL){
  if(is.null(variables_selected)){
    selected_vars <- pred$predictors_names
    print(cat('Using all variables available: ', selected_vars))
  }
  if(any(variables_selected %in% pred$predictors_names) ){
    selected_vars <- pred$predictors_names[pred$predictors_names %in% c("wc2.1_10m_bio_1"  ,"wc2.1_10m_bio_10", "wc2.1_10m_bio_11", "wc2.1_10m_bio_12")]
    print(cat('Using given variables: ', selected_vars))
  }
  p <- pred$grid[[selected_vars]]
  v <- vifcor(p, th=th, maxobservations=maxobservations)
  pred$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
  pred$variable_selection$vif$threshold <- th
  pred$variable_selection$vif$vifcor <- v
  return(pred)
}
