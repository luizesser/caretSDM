#' Occurrences Managing
#'
#' This function creates and manage \code{occurrences} objects.
#'
#' @usage
#' occurrences_sdm(x,
#'                 independent_test = NULL,
#'                 p = 0.1,
#'                 crs = NULL,
#'                 independent_test_crs = NULL,
#'                 ...)
#'
#' @param x A \code{data.frame}, \code{tibble} or \code{sf} with species records.
#' @param independent_test Boolean. If \code{independet_test} is \code{TRUE}, a fraction of the data
#' is kept for independent testing. Otherwise, the whole dataset \code{x} is used. It can also be a
#' \code{data.frame} or a \code{sf}, with species records to be used as independent test. Structure
#' and names should be identical to those in \code{x}.
#' @param p Numeric. Fraction of data to be used as independent test. Standard is 0.1.
#' @param crs Numeric. CRS of \code{x}.
#' @param independent_test_crs Numeric. CRS of \code{independent_test} if it is a
#' \code{data.frame}.
#' @param ... A vector with column names addressing the columns with species names, longitude and
#' latitude, respectively, in \code{x}.
#' @param i \code{input_sdm} or \code{occurrences} object.
#' @param oc1 A \code{occurrences} object to be summed with.
#' @param oc2 A \code{occurrences} object to be summed with.
#'
#' @return A \code{occurrences} object.
#'
#' @details
#' \code{x} must have three columns: species, decimalLongitude and decimalLatitude. When \code{sf}
#' it is only necessary a species column.
#' \code{n_records} return the number of presence records to each species.
#' \code{species_names} return the species names.
#' \code{get_coords} return a \code{data.frame} with coordinates of species records.
#' \code{add_occurrences} return a \code{occurrences}. This function sums two \code{occurrences} objects.
#' It can also sum a \code{occurrences} object with a \code{data.frame} object.
#' \code{occurrences_as_df} returns a \code{data.frame} with species names and coordinates.
#'
#' @seealso \code{\link{input_sdm} \link{GBIF_data} \link{occ}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933)
#'
#' @importFrom caret createDataPartition
#' @importFrom dplyr select
#' @importFrom sf st_coordinates st_as_sf st_transform st_crs
#' @importFrom stats na.omit
#' @importFrom cli cli_alert_warning cli_abort
#' @importFrom utils read.csv head
#'
#' @global X Y
#'
#' @export
occurrences_sdm <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                            independent_test_crs = NULL, ...) {
  UseMethod("occurrences_sdm")
}

#' @export
occurrences_sdm.data.frame <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                                       independent_test_crs = NULL, ...) {
  occ <- .occurrences(x, independent_test, p, crs, independent_test_crs, ...)
  return(occ)
}

#' @export
occurrences_sdm.tbl_df <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                                   independent_test_crs = NULL, ...) {
  x <- as.data.frame(x)
  occ <- .occurrences(x, independent_test, p, crs, independent_test_crs, ...)
  return(occ)
}

#' @export
occurrences_sdm.sf <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                               independent_test_crs = NULL, ...) {
  x <- cbind(dplyr::select(as.data.frame(x), -"geometry"), sf::st_coordinates(x))
  occ <- .occurrences(x, independent_test, p, crs, independent_test_crs, ...)
  return(occ)
}

#' @export
occurrences_sdm.character <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                               independent_test_crs = NULL, ...) {
  x <- utils::read.csv(x)
  occ <- .occurrences(x, independent_test, p, crs, independent_test_crs, ...)
  return(occ)
}

#' @rdname occurrences_sdm
#' @export
n_records <- function(i) {
  x=i
  if (is_input_sdm(x)) {
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
  if (is_input_sdm(x)) {
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
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  res <- y$occurrences |>
    sf::st_coordinates() |>
    as.data.frame() |>
    dplyr::select(X, Y)
  return(res)
}

#' @rdname occurrences_sdm
#' @export
occurrences_as_df <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  res <- y$occurrences |>
    .sf_to_df_sdm()
  return(res)
}

#' @rdname occurrences_sdm
#' @export
add_occurrences <- function(oc1, oc2) {
  assert_class_cli(oc1, "occurrences")
  if(is.data.frame(oc2)){
    oc2 <- .occurrences(oc2)
  }
  assert_class_cli(oc2, "occurrences")
  oc1_st <- oc1$occurrences
  oc2_st <- oc2$occurrences
  x <- rbind(oc1_st, oc2_st)
  res <- occurrences_sdm(x)
  return(res)
}

#' @export
.occurrences <- function(x, independent_test = NULL, p = 0.1, crs = NULL,
                         independent_test_crs = NULL, ...) {
  assert_int_cli(crs, lower = 1024, upper = 32766, null.ok = TRUE)
  assert_int_cli(independent_test_crs, lower = 1024, upper = 32766, null.ok = TRUE)
  if(isTRUE(independent_test)){
    assert_numeric_cli(p, lower = 0.0001, upper = 0.9999, len=1)
  }
  col_names <- .find_columns(x, ...)
  x <- x[, col_names] |> stats::na.omit()
  if (length(col_names) == 2) {
    cli::cli_alert_warning("Species column not found. Addressing all records to a unknown species.")
    colnames(x) <- c("decimalLongitude", "decimalLatitude")
    x <- cbind(species=rep("Sp_unknown", nrow(x)), x)
  } else if (length(col_names) == 3) {
    colnames(x) <- c("species", "decimalLongitude", "decimalLatitude")
  }
  spp_names <- unique(x[,1])
  if (is.null(crs)) {
    crs <- 4326
  }
  x <- sf::st_as_sf(x, coords = colnames(x)[c(2, 3)])
  sf::st_crs(x) <- crs
  if (!is.null(independent_test)) {
    if (isTRUE(independent_test)) {
      n <- as.vector(caret::createDataPartition(x$species, list = FALSE, p = p))
      x2 <- x[-n, ]
      indep_test_data <- x[n, ]
      occ <- structure(list(
        occurrences = x2,
        spp_names = spp_names,
        n_presences = table(x2$species),
        independent_test = indep_test_data,
        crs = crs
      ), class = "occurrences")
    } else if(any(class(independent_test) %in% c("data.frame"))) {
      if(any(class(independent_test) %in% c("sf"))) {
        if(is.na(sf::st_crs(independent_test))) {
          if(is.null(independent_test_crs)) {
            cli::cli_abort(c(
              "x" = "{.var independent_test_crs} is {.cls {class(independent_test_crs)}}, must be
              numeric",
              "i" = "To avoid geographical errors, it is important to know your CRSs. To solve this
              issue inform the CRS using the {.var independent_test_crs} parameter. Usually, GPS
              systems use EPSG:4326."
            ))
          } else {
            sf::st_crs(independent_test) <- sf::st_crs(independent_test_crs)
          }
        } else {
          independent_test_crs <- sf::st_crs(independent_test)
        }
        if("species" %in% colnames(independent_test)) {
          if(!all(spp_names %in% unique(independent_test$species))) {
            cli::cli_abort(c(
              "x" = "Species from {.var x} are missing in {.var independent_test data}.",
              "i" = "Make sure that the species column in {.var independent_test} have all species
              from {.var x} and all of them are written identically."
            ))
          }
        } else {
          if(length(spp_names) == 1) {
            independent_test <- cbind(species=rep(spp_names, nrow(independent_test)),
                                      independent_test)
          } else {
            cli::cli_abort(c(
              "x" = "Not able to infer to which species {.var independent_test} is refering to.",
              "i" = "There are more than one species in {.var x}, and no species information in
              {.var independent_test}. Make sure that a species column is added in
              {.var independent_test} with all species from {.var x} and all of them are written
              identically."
            ))
          }
        }
        if(sf::st_crs(independent_test) != sf::st_crs(x)){
          independent_test <- sf::st_transform(independent_test, sf::st_crs(x))
        }
        occ <- structure(list(
          occurrences = x,
          spp_names = spp_names,
          n_presences = table(x$species),
          independent_test = independent_test,
          crs = crs
        ), class = "occurrences")
      } else {
        if(is.null(independent_test_crs)){
          cli::cli_abort(c(
            "x" = "{.var independent_test_crs} is {.cls {class(independent_test_crs)}}, must be
            numeric",
            "i" = "To avoid geographical errors, it is important to know your CRSs. To solve this
              issue inform the CRS using the {.var independent_test_crs} parameter. Usually, GPS
              systems use EPSG:4326."
          ))
        }
        independent_test <- as.data.frame(independent_test)
        col_names <- .find_columns(independent_test, ...)
        independent_test <- independent_test[, col_names]
        if (length(col_names) == 2) {
          if(length(spp_names) == 1){
            colnames(independent_test) <- c("decimalLongitude", "decimalLatitude")
            independent_test <- cbind(species=rep(spp_names, nrow(independent_test)),
                                      independent_test)
          } else {
            cli::cli_abort(c(
              "x" = "Not able to infer to which species {.var independent_test} is refering to.",
              "i" = "There are more than one species in {.var x}, and no species information in
              {.var independent_test}. Make sure that a species column is added in
              {.var independent_test} with all species from {.var x} and all of them are written
              identically."
            ))
          }
        } else if (length(col_names) == 3) {
          colnames(independent_test) <- c("species", "decimalLongitude", "decimalLatitude")
        }
        independent_test <- sf::st_as_sf(independent_test,
                                         coords = colnames(independent_test)[c(2, 3)])
        sf::st_crs(independent_test) <- independent_test_crs
        if(independent_test_crs != crs){
          independent_test <- sf::st_transform(independent_test, crs)
        }
        occ <- structure(list(
          occurrences = x,
          spp_names = spp_names,
          n_presences = table(x$species),
          independent_test = independent_test,
          crs = crs
        ), class = "occurrences")
      }
    } else {
      cli::cli_abort(c(
        "x" = "{.var independent_test} must be either TRUE, a data.frame, a tibble or a sf with
        independent test data.",
        "i" = "{.var independent_test} is currently from class {.cls {class(independent_test)}}"
      ))
    }
  } else {
    occ <- structure(list(
      occurrences = x,
      spp_names = spp_names,
      n_presences = table(x$species),
      crs = crs
    ), class = "occurrences")
  }

  return(occ)
}

#' Print method for occurrences
#' @param x occurrences object
#' @param ... passed to other methods
#' @exportS3Method base::print
print.occurrences <- function(x, ...) {
  cat("        caretSDM       \n")
  cat(".......................\n")
  cat("Class                 : occurrences\n")
  cat("Species Names         :", sort(x$spp_names), "\n")
  cat("Number of presences   :", x$n_presences[sort(x$spp_names)], "\n")
  if (!is.null(x$independent_test)) {
    cat("Independent Test      : TRUE (number of records = ", nrow(x$independent_test), ")\n")
  }

  n <- max(nchar(colnames(x$occurrences)[1]), nchar(x$occurrences$species)[1])
  n <- n + sum(nchar(colnames(x$occurrences)[-1])) + ncol(x$occurrences) + 1
  cat(rep("=", n), sep = "")
  cat("\nData:\n")
  print(utils::head(x$occurrences))
  invisible(x)
}
