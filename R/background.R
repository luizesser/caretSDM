#' Obtain Background data
#'
#' This function obtains background data given a set of predictors.
#'
#' @usage
#' background(occ,
#'            pred = NULL,
#'            n = 10000,
#'            n_set = 1,
#'            proportion = NULL)
#'
#' @param occ A \code{occurrences_sdm} or \code{input_sdm} object.
#' @param pred A \code{sdm_area} object. If \code{NULL} and \code{occ} is a \code{input_sdm},
#' \code{pred} will be retrieved from \code{occ}.
#' @param n \code{numeric}. Number of background records to be generated in each dataset created.
#' If \code{NULL} then the function prevents imbalance by using the same number of presence records
#' (\code{n_records(occ)}). If you want to address different sizes to each species, you must provide
#' a named vector (as in \code{n_records(occ)}).
#' @param n_set \code{numeric}. Number of datasets of background data to create.
#' @param proportion \code{numeric}. A number between 0 and 1 representing a proportion of the area
#' to be mapped as background. E.g.: if the whole area has 5,000 cells and proportion is 0.1, then
#' \code{n} is set to 500. Standard is NULL. This argument overwrites \code{n}.
#'
#' @param i A \code{input_sdm} object.
#'
#' @returns A \code{occurrences_sdm} or \code{input_sdm} object with background data.
#'
#' @details
#' \code{background} is used in the SDM workflow to obtain background data, a step necessary for
#' MaxEnt algorithm to run. This function helps avoid the use of pseudoabsence data in background
#' algorithms and the use of background data in pseudoabsence algorithms, a very common mistake.
#'
#' \code{n_background} returns the number of background records obtained per species.
#'
#' \code{background_data} returns a \code{list} of species names. Each species name will have a
#' \code{list}s with background data from class \code{sf}.
#'
#' @seealso \code{link{input_sdm} \link{pseudoabsences} \link{occurrences_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
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
#' # Pseudoabsence generation:
#' i <- background(i, proportion = 1) # All available data is obtained as background data.
#'
#' @importFrom sf st_as_sf st_crs st_transform st_intersection st_geometry_type st_difference
#' @importFrom dplyr select all_of filter
#' @importFrom stars st_extract
#' @importFrom dismo bioclim predict
#' @importFrom cli cli_abort cli_warn cli_alert
#' @importFrom caret train getModelInfo
#' @importFrom stats pchisq cov mahalanobis
#'
#' @export
background <- function(occ, pred = NULL, n = 10000, n_set = 1, proportion = NULL) {
  assert_class_cli(occ, "input_sdm")
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    pred <- occ$predictors
  }
  assert_class_cli(pred, "sdm_area")
  assert_numeric_cli(n)
  assert_int_cli(n_set)
  assert_numeric_cli(proportion, null.ok = TRUE, lower = 0, upper = 1)
  if(!is.null(proportion)){
    n1 <- round(nrow(pred$grid)*proportion, 0)
    cli::cli_alert(c("Proportion is {proportion} Setting all species to have {n1} background records."))
    n <- n_records(y)
    n[] <- n1
  }

  if(is.null(names(n))){
    if(length(n) == length(species_names(y))) {
      names(n) <- species_names(y)
    } else {
      n1 <- species_names(y)
      n <- rep(n, length(n1))
      names(n) <- n1
    }
  }

  assert_names_cli(names(n), identical.to = species_names(y), type = "unique")

  if (!is.null(y$background)) {
    cli::cli_alert("Previous background element on Occurrences object will be overwritten.")
  }

  df <- pred$grid
  l <- sapply(y$spp_names, function(sp) {
    l <- list()
    for (j in 1:n_set) {
      if (n[sp] < nrow(df)) {
        samp <- sample(df$cell_id, n[sp])
      } else {
        samp <- sample(df$cell_id, n[sp], replace = TRUE)
        n[sp] <<- nrow(df)
        cli::cli_alert("Background number is higher than the total data available.
                        Setting the number of background data to be {n[sp]}.")
      }
      l[[j]] <- df[df$cell_id %in% samp, ]
    }
    return(l)
  }, simplify = FALSE, USE.NAMES = TRUE)

  if (is.null(proportion)) {
    proportion <- round(n/nrow(df), 3)
  }

  bg <- .background(y, l, n, n_set, proportion)

  if (is_input_sdm(occ)) {
    occ$occurrences <- bg
    bg <- occ
  }
  return(bg)
}

#' @rdname background
#' @export
n_background <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(i$occurrences$background$n)
}

#' @rdname background
#' @export
background_data <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$background$data)
}

.background <- function(occ, l, n, n_set, proportion) {
  occ$background$data <- l
  occ$background$n <- as.numeric(n)
  occ$background$n_set <- n_set
  occ$background$proportion <- proportion
  return(occ)
}
