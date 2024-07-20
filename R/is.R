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
  return(checkmate::test_class(x, classes = "input_sdm", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_sdm_area <- function(x) {
  return(checkmate::test_class(x, classes = "sdm_area", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_occurrences <- function(x) {
  return(checkmate::test_class(x, classes = "occurrences", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_predictors <- function(x) {
  return(checkmate::test_class(x, classes = "predictors", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_scenarios <- function(x) {
  return(checkmate::test_class(x, classes = "scenarios", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_models <- function(x) {
  return(checkmate::test_class(x, classes = "models", null.ok = FALSE))
}

#' @rdname is_input_sdm
#' @export
is_predictions <- function(x) {
  return(checkmate::test_class(x, classes = "predictions", null.ok = FALSE))
}
