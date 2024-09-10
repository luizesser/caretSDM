#' Tidyverse methods for caretSDM objects
#'
#' Set of functions to facilitate the use of caretSDM through tidyverse grammatics.
#'
#' @usage select(x, ...)
#'
#' @param x \code{sdm_area} object
#' @param ... \code{character} vector with predictors to be selected.
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
#'
#' @importFrom dplyr select relocate mutate
#'
#' @rdname tidyverse-methods
#' @export
select.sdm_area <- function(x, ...){
  .check_sdm_area(x)
  grd <- dplyr::select(x$grid, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd <- grd |> dplyr::relocate(cell_id, ...)
  x$grid <- grd

  if("scenarios" %in% names(x)){
    .check_sdm_area(x$scenarios)
    x$scenarios$data <- sapply(x$scenarios$data, function(y) {
      grd <- dplyr::select(y, ...)
      grd_col_names <- colnames(grd)
      if (!("cell_id" %in% grd_col_names)) {
        grd[["cell_id"]] <- y[["cell_id"]]
      }
      grd <- grd |> dplyr::relocate(cell_id, ...)
      return(grd)
    }, simplify = FALSE, USE.NAMES = TRUE)
    x$scenarios$grid <- x$scenarios$data[[1]]
  }

  return(x)
}

#' @rdname tidyverse-methods
#' @export
select.input_sdm <- function(x, ...){
  i <- x
  x <- x$predictors
  .check_sdm_area(x)
  grd <- dplyr::select(x$grid, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd <- grd |> dplyr::relocate(cell_id, ...)
  x$grid <- grd
  i$predictors <- x

  if("scenarios" %in% names(x)){
    .check_sdm_area(x$scenarios)
    x$scenarios$data <- sapply(x$scenarios$data, function(y) {
      grd <- dplyr::select(y, ...)
      grd_col_names <- colnames(grd)
      if (!("cell_id" %in% grd_col_names)) {
        grd[["cell_id"]] <- y[["cell_id"]]
      }
      grd <- grd |> dplyr::relocate(cell_id, ...)
      return(grd)
    }, simplify = FALSE, USE.NAMES = TRUE)
    x$scenarios$grid <- x$scenarios$data[[1]]
  }

  return(i)
}

#' @rdname tidyverse-methods
#' @export
mutate.sdm_area <- function(x, ...){
  .check_sdm_area(x)
  grd <- dplyr::mutate(x$grid, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
  grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)
  x$grid <- grd
  return(x)
}

#' @rdname tidyverse-methods
#' @export
mutate.input_sdm <- function(x, ...){
  i <- x
  x <- x$predictors
  .check_sdm_area(x)
  grd <- dplyr::mutate(x$grid, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
  grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)
  x$grid <- grd
  i$predictors <- x
  return(i)
}

#' @rdname tidyverse-methods
#' @export
filter.sdm_area <- function(x, ...){
  .check_sdm_area(x)
  grd <- dplyr::filter(x$grid, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
  grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)
  x$grid <- grd
  return(x)
}

#' @rdname tidyverse-methods
#' @export
filter.input_sdm <- function(x, ...){
  i <- x
  x <- x$occurences
  grd <- dplyr::filter(x$occurrences, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$occurrences[["cell_id"]]
  }
  grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
  grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)
  x$occurrences <- grd
  i$occurrences <- x
  return(i)
}

#' @rdname tidyverse-methods
#' @export
filter.occurrences <- function(x, ...){
  oc <- x
  x <- x$occurrences
  grd <- dplyr::filter(x, ...)
  grd_col_names <- colnames(grd)
  if (!("cell_id" %in% grd_col_names)) {
    grd[["cell_id"]] <- x$grid[["cell_id"]]
  }
  grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
  grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)
  x$grid <- grd
  return(i)
}
