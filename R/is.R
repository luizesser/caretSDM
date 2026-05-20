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
#' sa <- sdm_area(parana, cell_size = 25000, output_crs = 6933)
#'
#' is_sdm_area(sa)
#'
#' is_input_sdm(sa)
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import checkCLI
#'
#' @export
is_input_sdm <- function(x) {
  return(checkCLI::check_class_cli(x, classes = "input_sdm", null.ok = FALSE) == TRUE)
}

#' @rdname is_input_sdm
#' @export
is_sdm_area <- function(x) {
  return(checkCLI::check_class_cli(x, classes = "sdm_area", null.ok = FALSE) == TRUE)
}

#' @rdname is_input_sdm
#' @export
is_occurrences <- function(x) {
  return(checkCLI::check_class_cli(x, classes = "occurrences", null.ok = FALSE) == TRUE)
}

#' @rdname is_input_sdm
#' @export
is_models <- function(x) {
  return(checkCLI::check_class_cli(x, classes = "models", null.ok = FALSE) == TRUE)
}

#' @rdname is_input_sdm
#' @export
is_predictions <- function(x) {
  return(checkCLI::check_class_cli(x, classes = "predictions", null.ok = FALSE) == TRUE)
}
