#' Write Predictions
#'
#' This function writes predictions in the given directory.
#'
#' @param m A models object
#' @param scen A scenarios object
#' @param th Thresholds for metrics to be used
#'
#' @return A predictions object
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#'
#' @export
write_predictions <- function(x,l){
  if(class(x)=='input_data'){
    x <- x$predictions
  }
  if(class(x)=='predictions'){
    x <- x
  }

}
