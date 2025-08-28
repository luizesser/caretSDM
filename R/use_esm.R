#' Ensemble of Small Models (ESM) in caretSDM
#'
#' This functions set parameters to run a ESM when running \code{train_sdm}.
#'
#' @usage use_esm(x, spp = NULL, n_records = 20)
#'
#' @param x A \code{occurrences} or \code{input_sdm} object containing occurrences.
#' @param spp A vector of species names containing the species which the ESM must be applied.
#' Standard is NULL.
#' @param n_records Numeric. Number of species records to apply the ESM. Standard is 20.
#'
#' @details
#' We supply two different ways to apply the ESM. If species names are provided, then ESM will be
#' applied only in given species. If a number of species records is provided, then ESM will be
#' applied in every species with number of records bellow the given threshold. As standard,
#' \code{use_esm} will be apply to every species with less then 20 records.
#'
#' @returns A \code{input_sdm} or \code{occurrences} object with ESM parameters.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples

#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Use MEM:
#' i <- use_esm(i)
#'
#' @importFrom cli cli_abort
#' @export
use_esm <- function(x, spp = NULL, n_records = 20) {
  caretSDM:::assert_subset_cli(class(x), c("input_sdm", "occurrences"))
  caretSDM:::assert_subset_cli(spp, species_names(x))
  caretSDM:::assert_numeric_cli(n_records, lower = 0, len = 1, any.missing = FALSE)

  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else if (is_occurrences(x)) {
    y <- x
  }

  if(is.null(spp)) {
    spp <- names(n_records(y))[n_records(y) <= n_records]
  }
  if(length(spp) == 0) {
    cli::cli_abort(c("x" = "No species with number of records =< {n_records}."))
  }
  y$esm$spp <- spp
  y$esm$n_records <- n_records

  if (is_input_sdm(x)) {
    x$occurrences <- y
  } else if (is_occurrences(x)) {
    x <- y
  }
  return(x)
}
