#' @rdname predictor_names
#' @export
predictors <- function(x) {
  UseMethod("predictors")
}

#' @rdname predictor_names
#' @export
predictors.sdm_area <- function(x, ...){
  predictors <- x$grid |>
    names() |>
    purrr::discard(\(x) x %in% c("geometry", "cell_id"))
  return(predictors)
}

#' @rdname predictor_names
#' @export
set_predictor_names <- function(x, new_names) {
  UseMethod("set_predictor_names")
}

#' @rdname predictor_names
#' @export
set_predictor_names.sdm_area <- function(x, new_names) {
  .check_sdm_area(x)
  assert_character_cli(
    new_names,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok =  FALSE,
    len = (x$grid |> names() |> length()) - 2,
    unique = TRUE
  )

  grd_spatial <- x$grid |>
    select(c(cell_id, geometry))
  grd_data <- x$grid |>
    as.data.frame() |>
    dplyr::select(-c(cell_id, geometry))

  names(grd_data) <- new_names
  x$grid <- grd_spatial |>
    dplyr::bind_cols(grd_data) |>
    dplyr::relocate(geometry, .after = last_col())

  return(x)
}

#' @rdname predictor_names
#' @export
get_predictor_names <- function(x) {
  UseMethod("get_predictor_names")
}

#' @rdname predictor_names
#' @export
get_predictor_names.sdm_area <- function(x) {
  return(predictors(x))
}




