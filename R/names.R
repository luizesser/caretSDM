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
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
#'
#' # Check predictors' names:
#' get_predictor_names(sa)
#'
#' @importFrom dplyr select bind_cols relocate
#' @importFrom purrr discard
#' @importFrom stars st_set_dimensions st_get_dimension_values
#' @importFrom stringdist stringdist
#' @importFrom cli cli_abort
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

  if("scenarios" %in% names(x)){
    i$scenarios$data <- sapply(x$scenarios$data, function(y) {
      grd_spatial <- y |>
        dplyr::select(c(cell_id, geometry))
      grd_data <- y |>
        as.data.frame() |>
        dplyr::select(-c(cell_id, geometry))

      names(grd_data) <- new_names
      y <- grd_spatial |>
        dplyr::bind_cols(grd_data) |>
        dplyr::relocate(geometry, .after = last_col())
    }, simplify = FALSE, USE.NAMES = TRUE)
  }

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

  if("scenarios" %in% names(x)){
    x$scenarios$data <- sapply(x$scenarios$data, function(y) {
      grd_spatial <- y |>
        dplyr::select(c(cell_id, geometry))
      grd_data <- y |>
        as.data.frame() |>
        dplyr::select(-c(cell_id, geometry))

      names(grd_data) <- new_names
      y <- grd_spatial |>
        dplyr::bind_cols(grd_data) |>
        dplyr::relocate(geometry, .after = last_col())
    }, simplify = FALSE, USE.NAMES = TRUE)
    x$scenarios$grid <- x$scenarios$data[[1]]
  }
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

#' @rdname predictor_names
#' @export
test_variables_names <- function(sa, scen){
  assert_class_cli(sa, "sdm_area")
  assert_class_cli(scen, "stars")
  sa_names <- get_predictor_names(sa)
  scen_names <- sort(stars::st_get_dimension_values(scen, "band"))
  return(all( scen_names %in% sa_names ))
}

#' @rdname predictor_names
#' @export
find_closest_matches <- function(inputs, valid_inputs) {
  closest_matches <- character(length(inputs))
  for (i in seq_along(inputs)) {
    distances <- stringdist::stringdist(inputs[i], valid_inputs)
    closest_index <- which.min(distances)
    closest_matches[i] <- valid_inputs[closest_index]
    valid_inputs <- valid_inputs[-closest_index]
  }
  df <- data.frame(s1_names=inputs, s2_names=closest_matches)
  return(df)
}
#find_closest_matches <- function(inputs, valid_inputs) {
#  # Initialize results
#  min_len <- min(length(inputs), length(valid_inputs))
#  matched_inputs <- character(length(min_len))
#  matched_values <- character(length(min_len))
#  distances <- numeric(length(min_len))
#
#  # Track which valid inputs have been matched
#  matched_valid <- logical(length(valid_inputs))
#  names(matched_valid) <- valid_inputs
#
#  shorter_index <- which.min(c(length(inputs), length(valid_inputs)))
#  shorter <- get(ifelse(shorter_index == 1, "inputs", "valid_inputs"))
#  longer <- get(ifelse(shorter_index != 1, "inputs", "valid_inputs"))
#
#  for (i in seq_along(shorter)) {
#    input <- shorter[i]
#
#    # Calculate distances to all valid inputs (excluding already matched ones)
#    available_valid <- longer[!matched_valid]
#    if (length(available_valid) == 0) {
#      # No valid inputs left → assign NA
#      matched_inputs[i] <- input
#      matched_values[i] <- NA
#      distances[i] <- NA
#      next
#    }
#
#    current_distances <- stringdist::stringdist(input, available_valid, method = "jw")
#    closest_index <- which.min(current_distances)
#    best_match <- available_valid[closest_index]
#    best_distance <- current_distances[closest_index]
#    stop_test <- length(current_distances[current_distances == best_distance])
#    if (stop_test > 1) {
#      warning(paste0("Not able to match variables from different sources"))
#      stop()
#    }
#    # Record the match
#    matched_inputs[i] <- input
#    matched_values[i] <- best_match
#    distances[i] <- best_distance
#
#    # Mark this valid input as matched
#    matched_valid[best_match] <- TRUE
#  }
#
#  # Create the result dataframe
#  result_df <- data.frame(
#    s1_names = matched_inputs,
#    s2_names = matched_values,
#    distance = distances,
#    stringsAsFactors = FALSE
#  )
#
#  return(result_df)
#}
#
#inputs <- c( "bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10",
#             "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
#valid_inputs <- c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15")


#' @rdname predictor_names
#' @export
set_variables_names <- function(s1 = NULL, s2 = NULL, new_names = NULL) {
  assert_class_cli(s1, "stars")
  if(is.null(new_names)) {
    assert_subset_cli(class(s2), c("stars", "sdm_area"))
    if(is(s2, "stars")) {
      assert_class_cli(s2, "stars")
      len_s2 <- length(stars::st_get_dimension_values(s2, "band"))
      len_s1 <- length(stars::st_get_dimension_values(s1, "band"))
      if (!len_s1 == len_s2) {
        cli::cli_abort(c("i" = "{.var s1} has {len_s1} variable{?s},
                  while {.var s2} has {len_s2} variable{?s}.",
                  "x" = "{.var s1} and {.var s2} should have the same number of variables."))
      }
      if(!all(sort(stars::st_get_dimension_values(s2, "band")) == stars::st_get_dimension_values(s1, "band"))){
        closest_match <- find_closest_matches(stars::st_get_dimension_values(s1, "band"),
                                              stars::st_get_dimension_values(s2, "band"))
        print(closest_match)
        s1 <- stars::st_set_dimensions(s1, "band", values = closest_match$s2_names)
      }
    }
    if(is_sdm_area(s2)) {
      len_s2 <- length(get_predictor_names(s2))
      len_s1 <- length(stars::st_get_dimension_values(s1, "band"))
      if (!len_s1 == len_s2) {
        cli::cli_abort(c("i" = "{.var s1} has {len_s1} variable{?s},
                  while {.var s2} has {len_s2} variable{?s}.",
                  "x" = "{.var s1} and {.var s2} should have the same number of variables."))
      }
      if(!all(sort(get_predictor_names(s2)) == stars::st_get_dimension_values(s1, "band"))){
        closest_match <- find_closest_matches(stars::st_get_dimension_values(s1, "band"),
                                              get_predictor_names(s2))
        print(closest_match)
        s1 <- stars::st_set_dimensions(s1, "band", values = closest_match$s2_names)
      }
    }
  } else {
    len_s1 <- length(stars::st_get_dimension_values(s1, "band"))
    assert_character_cli(new_names, len = len_s1)
    s1 <- stars::st_set_dimensions(s1, "band", values = new_names)
  }
  return(s1)
}
