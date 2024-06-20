#' Occurrences Managing
#'
#' This function creates and manage \code{occurrences} objects.
#'
#' @usage
#' occurrences_sdm(x,
#'                 independent_test = NULL,
#'                 p = 0.1,
#'                 epsg = NULL,
#'                 ...)
#'
#' @param x A \code{data.frame} with species records.
#' @param independent_test Boolean. If \code{independet_test} is \code{TRUE}, a fraction of the data
#' is kept for independent testing. Otherwise, the whole dataset \code{x} is used.
#' @param p Numeric. Fraction of data to be used as independent test. Standard is 0.1.
#' @param epsg Numeric. EPSG of \code{x}.
#' @param ... A vector with column names addressing the columns with species names, longitude and
#' latitude, respectively, in \code{x}.
#' @param i \code{input_sdm} object.
#'
#' @return A \code{occurrences} object.
#'
#' @details
#' \code{x} must have three columns: species, decimalLongitude and decimalLatitude.
#'
#' \code{n_records} return the number of presence records to each species.
#'
#' \code{species_names} return the species names.
#'
#' \code{get_coords} return a \code{data.frame} with coordinates of species records.
#'
#' @seealso \code{\link{input_sdm} \link{GBIF_data} \link{occ}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' occ <- occurrences_sdm(occ, epsg = 6933)
#'
#' @import tibble
#' @import stars
#' @importFrom here here
#'
#' @export
occurrences_sdm <- function(x, independent_test = NULL, p = 0.1, epsg = NULL, ...) {
  UseMethod("occurrences_sdm")
}

#' @export
occurrences_sdm.data.frame <- function(x, independent_test = NULL, p = 0.1, epsg = NULL, ...) {
  occ <- .occurrences(x, independent_test, p, epsg, ...)
  return(occ)
}

#' @export
occurrences_sdm.tibble <- function(x, independent_test = NULL, p = 0.1, epsg = NULL, ...) {
  x <- as.data.frame(x)
  occ <- .occurrences(x, independent_test, p, epsg, ...)
  return(occ)
}

#' @export
occurrences_sdm.sf <- function(x, independent_test = NULL, p = 0.1, epsg = NULL, ...) {
  x <- cbind(select(as.data.frame(x), -"geometry"), st_coordinates(x))
  occ <- .occurrences(x, independent_test, p, epsg, ...)
  return(occ)
}

#' @rdname occurrences_sdm
#' @export
n_records <- function(i) {
  x=i
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$n_presences)
}

#' @rdname occurrences_sdm
#' @export
species_names <- function(i) {
  x=i
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$spp_names)
}

#' @rdname occurrences_sdm
#' @export
get_coords <- function(i) {
  x=i
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  res <- y$occurrences |>
    st_coordinates() |>
    as.data.frame() |>
    select(X, Y)
  return(res)
}

#' @export
.occurrences <- function(x, independent_test = NULL, p = 0.1, epsg = NULL, ...) {
  col_names <- find_columns(x, ...)
  x <- x[, col_names]
  if (length(col_names) == 2) {
    x <- cbind(rep("Sp_unknown", nrow(x)), x)
  }
  spp_names <- unique(x[, 1])
  if (is.null(epsg)) {
    epsg <- 4326
  }
  if (!is.null(independent_test)) {
    if (isTRUE(independent_test)) {
      occ_data <- st_as_sf(x, coords = col_names[c(2, 3)])
      n <- as.vector(caret::createDataPartition(x[, col_names[1]], list = F, p = p))
      x <- occ_data[-n, ]
      indep_test_data <- occ_data[n, ]
      occ <- structure(list(
        occurrences = x,
        spp_names = spp_names,
        n_presences = table(x[, 1]),
        independent_test = indep_test_data,
        epsg = epsg
      ), class = "occurrences")
    } else {
      independent_test <- as.data.frame(independent_test)
      col_names <- find_columns(independent_test, ...)
      independent_test <- independent_test[, col_names]
      if (length(col_names) == 2) {
        independent_test <- cbind(rep("Sp_unknown", nrow(independent_test)), independent_test)
      }
      # spp_names2 <- unique(independent_test[,1]) ### Multiple Species?
      occ <- structure(list(
        occurrences = x,
        spp_names = spp_names,
        n_presences = table(x[, 1]),
        independent_test = independent_test,
        epsg = epsg
      ), class = "occurrences")
    }
  } else {
    occ <- structure(list(
      occurrences = x,
      spp_names = spp_names,
      n_presences = table(x[, 1]),
      epsg = epsg
    ), class = "occurrences")
  }

  return(occ)
}

#' Print method for occurrences
#' @exportS3Method base::print
print.occurrences <- function(x) {
  cat("        caretSDM       \n")
  cat(".......................\n")
  cat("Class                 : Occurrences\n")
  cat("Species Names         :", x$spp_names, "\n")
  cat("Number of presences   :", x$n_presences, "\n")
  if (!is.null(x$pseudoabsences)) {
    cat(
      "Pseudoabsence methods :\n",
      "        Method to obtain PAs       :", x$pseudoabsences$method, "\n",
      "        Number of PA sets          :", x$pseudoabsences$n_set, "\n",
      "        Number of PAs in each set  :", x$pseudoabsences$n_pa, "\n"
    )
  }
  if (!is.null(x$background)) {
    cat("Background sets       :", length(x$background), "\n")
  }
  if (!is.null(x$independent_test)) {
    cat("Independent Test      : TRUE (number of records = ", nrow(x$independent_test), ")\n")
  }
  if (!is.null(x$data_cleaning)) {
    cat(cat("Data Cleaning         : "), cat(x$data_cleaning, sep = ", "), "\n")
  }
  n <- max(nchar(colnames(x$occurrences)[1]), nchar(x$occurrences[, 1])[1])
  n <- n + sum(nchar(colnames(x$occurrences)[-1])) + ncol(x$occurrences) + 1
  cat(rep("=", n), sep = "")
  cat("\nData:\n")
  print(head(x$occurrences))
}
