#' is_sdm functions to check caretSDM data classes.
#'
#' This functions returns a boolean to check caretSDM object classes.
#'
#' @param x caretSDM object
#'
#' @returns The output is a boolean
#'
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
is_input_sdm <- function(x) {
  return(class(x) == "input_sdm")
}

#' @export
is_occurrences <- function(x) {
  return(class(x) == "occurrences")
}

#' @export
is_predictors <- function(x) {
  return(class(x) == "predictors")
}

#' @export
is_scenarios <- function(x) {
  return(class(x) == "scenarios")
}

#' @export
is_models <- function(x) {
  return(class(x) == "models")
}

#' @export
is_predictions <- function(x) {
  return(class(x) == "predictions")
}
