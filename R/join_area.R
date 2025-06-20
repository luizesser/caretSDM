#' Join Area
#'
#' Join cell_id data from sdm_area to a occurrences
#'
#' @usage join_area(occ, pred)
#'
#' @param occ A \code{occurrences} object or \code{input_sdm}.
#' @param pred A \code{sdm_area} object to retrieve cell_id from.
#'
#' @returns A \code{occurrences} object with \code{cell_id} to each record.
#'
#' @details
#' This function is key in this SDM workflow. It attaches cell_id values to \code{occ}, deletes
#' records outside \code{pred} and allows the use of pseudoabsences. This function also tests if
#' CRS from both \code{occ} and \code{pred} are equal, otherwise the CRS of \code{pred} is used to
#' convert \code{occ}.
#'
#' @seealso \code{\link{occurrences_sdm} \link{sdm_area} \link{input_sdm}
#' \link{pseudoabsences}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa, scen)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' @importFrom sf st_crs st_transform st_join st_geometry_type st_nearest_feature
#' @importFrom dplyr select relocate
#' @importFrom cli cli_abort cli_warn
#' @importFrom stats na.omit
#'
#' @export
join_area <- function(occ, pred) {
  assert_class_cli(occ, "occurrences")
  assert_class_cli(pred, "sdm_area")
  oc <- occ$occurrences
  pd <- pred$grid

  if("cell_id" %in% names(oc)){
    cli::cli_warn(c(
      "occurrence data already has a 'cell_id' column.",
      "i" = "Deleting current cell_id and applying a new cell_id."
    ))
    oc <- oc |> select(-cell_id)
  }

  if(sf::st_crs(oc) != sf::st_crs(pd)){
    oc <- sf::st_transform(oc, sf::st_crs(pd))
  }

  v1 <- nrow(oc)

  if(unique(sf::st_geometry_type(pd)) == "LINESTRING") {
    # Find nearest features
    nearest <- sf::st_nearest_feature(oc, dplyr::select(pd, "cell_id"))
    cell_id <- pd[nearest, "cell_id"]
    oc <- cbind(oc, cell_id)|>
      dplyr::relocate("cell_id") |>
      dplyr::select(-"geometry.1")

  } else {
    oc <- oc |>
      sf::st_join(dplyr::select(pd, "cell_id")) |>
      dplyr::relocate("cell_id") |>
      stats::na.omit()
  }

  v2 <- v1-nrow(oc)

  if(v2 > 0){
    cli::cli_warn(c("Some records from {.var occ} do not fall in {.var pred}.",
               "i" = "{v2} elements from {.var occ} were excluded.",
               "i" = "If this seems too much, check how {.var occ} and {.var pred} intersect."
               ))
  }

  len <- length(unique(oc$cell_id))
  if(len <= 1) {
    cli::cli_abort(c(
      "occurrence data has {len} cell_id value{?s}.",
      "x" = "{.var occ} and {.var pred} probably do not overlap."
    ))
  }

  occ$occurrences <- oc
  occ$n_presences <- table(oc$species)
  occ$crs <- sf::st_crs(oc)$epsg
  return(occ)
}
