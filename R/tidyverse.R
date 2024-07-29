#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
