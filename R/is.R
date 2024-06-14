#' \code{is_class} functions to check caretSDM data classes.
#'
#' This functions returns a boolean to check caretSDM object classes.
#'
#' @usage is_input_sdm(x)
#'
#' @param x Object to be tested.
#'
#' @returns Boolean.
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#' is_sdm_area(sa)
#' is_input_sdm(sa)
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
is_input_sdm <- function(x) {
  return(class(x) == "input_sdm")
}

#' @rdname is_input_sdm
#' @export
is_sdm_area <- function(x) {
  return(class(x) == "sdm_area")
}

#' @rdname is_input_sdm
#' @export
is_occurrences <- function(x) {
  return(class(x) == "occurrences")
}

#' @rdname is_input_sdm
#' @export
is_predictors <- function(x) {
  return(class(x) == "predictors")
}

#' @rdname is_input_sdm
#' @export
is_scenarios <- function(x) {
  return(class(x) == "scenarios")
}

#' @rdname is_input_sdm
#' @export
is_models <- function(x) {
  return(class(x) == "models")
}

#' @rdname is_input_sdm
#' @export
is_predictions <- function(x) {
  return(class(x) == "predictions")
}
