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
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @importFrom usdm vifcor
#' @export
vif_predictors <- function(pred, area='all', th=0.5, maxobservations=5000, variables_selected=NULL){
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
  if(any(variables_selected %in% x$predictors_names) ){
    selected_vars <- x$predictors_names[x$predictors_names %in% variables_selected]
    print(cat('Using given variables: '), cat(selected_vars, sep=', '))
  }
  if(area=='all'){p <- as.data.frame(x$grid[[selected_vars]])}
  if(area=='occurrences'){
    if(!class(pred)=='input_sdm'){stop('Method only available with input_sdm class.')}
    cols <- find_columns(i$occurrences$occurrences)
    df <- i$occurrences$occurrences
    coordinates(df) <- cols[2:3]
    p <- extract(x$grid[[selected_vars]], df)
  }
  v <- vifcor(p, th=th, size=maxobservations)
  x$variable_selection$vif$area <- area
  x$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
  x$variable_selection$vif$threshold <- th
  x$variable_selection$vif$vifcor <- v
  if(class(pred)=='input_sdm'){
    i$predictors <- x
    x <- i
  }
  return(x)
}
