

#' @param x
#'
#' @export
predictors <- function(x) {
  UseMethod("predictors")
}

#' @export
predictors.sdm_area <- function(x, ...){
  predictors <- x$grid |>
    colnames() |>
    purrr::discard(\(x) x %in% c("geometry", "cell_id"))
  return(predictors)
}
