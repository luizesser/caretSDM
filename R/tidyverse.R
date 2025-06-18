#' Tidyverse methods for caretSDM objects
#'
#' Set of functions to facilitate the use of caretSDM through tidyverse grammatics.
#'
#' @usage select_predictors(x, ...)
#'
#' @param x \code{sdm_area} or \code{input_sdm} object.
#' @param spp Species to be filtered.
#' @param ... \code{character} arguments to pass to the given function.
#' @param .data Data to pass to tidyr function.
#' @param .by See ?dplyr::filter.
#' @param .preserve See ?dplyr::filter.
#'
#'
#' @examples

#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio1", "bio4", "bio12"))
#'
#' @importFrom dplyr select relocate mutate filter
#'
#' @global species
#'
#' @rdname tidyverse-methods
#' @export
select_predictors <- function(x, ...) {
  return(dplyr::select(x, ...))
}

#' @rdname tidyverse-methods
#' @export
select.sdm_area <- function(.data, ...){
  x <- .data
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
select.input_sdm <- function(.data, ...){
  x <- .data
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
mutate.sdm_area <- function(.data, ...){
  x <- .data
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
mutate.input_sdm <- function(.data, ...){
  x <- .data
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
filter.sdm_area <- function(.data, ..., .by, .preserve){
  x <- .data
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
filter.input_sdm <- function(.data, ..., .by, .preserve){
  x <- .data
  oc <- filter(x$occurrences, ...)
  x$occurrences <- oc
  return(x)
}

#' @rdname tidyverse-methods
#' @export
filter.occurrences <- function(.data, ..., .by, .preserve){
  x <- .data

  oc <- x
  x <- x$occurrences
  cd <- FALSE
  if("cell_id" %in% names(x)){
    cd <- TRUE
  }
  grd <- dplyr::filter(x, ...)
  grd_col_names <- colnames(grd)
  if(cd){
    if (!("cell_id" %in% grd_col_names)) {
      grd[["cell_id"]] <- oc$grid[["cell_id"]]
    }
    grd_col_names2 <- setdiff(grd_col_names, c('cell_id','geometry'))
    grd <- grd |> dplyr::relocate(cell_id, all_of(grd_col_names2), geometry)

  } else {
    grd_col_names2 <- setdiff(grd_col_names, c('geometry'))
    grd <- grd |> dplyr::relocate(all_of(grd_col_names2), geometry)
  }

  oc$occurrences <- grd
  oc$spp_names <- table(grd$species) |> names()
  oc$n_presences <- table(grd$species)
  return(oc)
}

#' @rdname tidyverse-methods
#' @export
filter_species <- function(x, spp = NULL, ...) {
  if (is.null(spp)) {
    cli::cli_abort(c("No species selected."))
  }

  if(is_input_sdm(x)){
    if ("occurrences" %in% names(x)) {
      x$occurrences$occurrences <- dplyr::filter(x$occurrences$occurrences, species %in% spp, ...)
      x$occurrences$spp_names <- spp
      x$occurrences$n_presences <- x$occurrences$n_presences[spp]
      if ("pseudoabsences" %in% names(x$occurrences)) {
        x$occurrences$pseudoabsences$n_pa <- x$occurrences$pseudoabsences$n_pa[spp]
        x$occurrences$pseudoabsences$data <- x$occurrences$pseudoabsences$data[spp]
      }
    }

    if ("models" %in% names(x)) {
      x$models$validation$metrics <- x$models$validation$metrics[spp]
      x$models$models <- x$models$models[spp]
    }

    if ("predictions" %in% names(x)) {
      x$predictions$thresholds$values <- x$predictions$thresholds$values[spp]
      x$predictions$ensembles <- subset(x$predictions$ensembles, rownames(x$predictions$ensembles) %in% spp)
      x$predictions$models$validation$metrics <- x$predictions$models$validation$metrics[spp]
      x$predictions$models$models <- x$predictions$models$models[spp]
      for (j in 1:length(x$predictions$predictions)){
        x$predictions$predictions[[j]] <- x$predictions$predictions[[j]][spp]
      }
    }
  }
  if(is_occurrences(x)){
    if ("occurrences" %in% names(x)) {
      x$occurrences <- dplyr::filter(x$occurrences, species %in% spp)
      x$spp_names <- spp
      x$n_presences <- x$n_presences[spp]
    }
  }

  return(x)
}
