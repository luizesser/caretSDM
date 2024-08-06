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
#' # Create sdm_area object
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors
#' sa <- add_predictors(sa, bioc)
#'
#' # Select predictors
#' predictors(sa)
#' sa <- select(sa, c("bio01", "bio12"))
#'
#' @importFrom dplyr select relocate
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
  return(i)
}
