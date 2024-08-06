#' Predictors Names Managing
#'
#' This function manage predictors names in \code{sdm_area} objects.
#'
#' @usage
#' predictors(x)
#'
#' @param x A \code{sdm_area} or \code{input_sdm} object to get/set predictors names.
#' @param new_names \code{character} vector from size \code{length(get_predictor_names(x))}
#'
#' @details
#' This functions is available so users can modify predictors names to better represent them. Use
#' carefully to avoid giving wrong names to the predictors. Useful to make sure the predictors names
#' are equal the names in scenarios.
#'
#' @return A \code{character} vector with predictors names.
#'
#' @seealso \code{\link{parana} \link{sdm_area}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#' predictors(sa)
#'
#' @importFrom dplyr select bind_cols relocate
#' @importFrom purrr discard
#'
#' @export
#' @rdname predictor_names
#' @export
predictors <- function(x) {
  assert_cli(
    check_class_cli(x, c('input_sdm')),
    check_class_cli(x, c('sdm_area'))
  )
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
predictors.input_sdm <- function(x, ...){
  x <- x$predictors
  predictors <- x$grid |>
    names() |>
    purrr::discard(\(x) x %in% c("geometry", "cell_id"))
  return(predictors)
}

#' @rdname predictor_names
#' @export
set_predictor_names <- function(x, new_names) {
  assert_cli(
    check_class_cli(x, c('input_sdm')),
    check_class_cli(x, c('sdm_area'))
  )
  UseMethod("set_predictor_names")
}

#' @rdname predictor_names
#' @export
set_predictor_names.input_sdm <- function(x, new_names) {
  i <- x
  x <- x$predictors
  .check_sdm_area(x)
  assert_character_cli(
    new_names,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok =  FALSE,
    len = (x$grid |> names() |> length()) - 2,
    unique = TRUE
  )
  assert_names_cli(
    new_names,
    disjunct.from = c("cell_id"),
    .var.name = "new_names"
  )
  assert_names_cli(
    new_names,
    disjunct.from = c("geometry"),
    .var.name = "new_names"
  )
  grd_spatial <- x$grid |>
    dplyr::select(c(cell_id, geometry))
  grd_data <- x$grid |>
    as.data.frame() |>
    dplyr::select(-c(cell_id, geometry))

  names(grd_data) <- new_names
  x$grid <- grd_spatial |>
    dplyr::bind_cols(grd_data) |>
    dplyr::relocate(geometry, .after = last_col())

  i$predictors <- x
  return(i)
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
  assert_names_cli(
    new_names,
    disjunct.from = c("cell_id"),
    .var.name = "new_names"
  )
  assert_names_cli(
    new_names,
    disjunct.from = c("geometry"),
    .var.name = "new_names"
  )
  grd_spatial <- x$grid |>
    dplyr::select(c(cell_id, geometry))
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
  assert_cli(
    check_class_cli(x, c('input_sdm')),
    check_class_cli(x, c('sdm_area'))
  )
  if(is_input_sdm(x)){
    x <- x$predictors
  }
  UseMethod("get_predictor_names")
}

#' @rdname predictor_names
#' @export
get_predictor_names.sdm_area <- function(x) {
  return(predictors(x))
}

#' @rdname predictor_names
#' @export
get_predictor_names.input_sdm <- function(x) {
  return(predictors(x$predictors))
}
