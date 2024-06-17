#' Band management for \code{stars} objects
#'
#' Functions to retrieve names and change them from \code{stars} objects.
#'
#' @usage set_band_names(x, new_names)
#'
#' @param x A \code{stars} object.
#' @param new_names \code{character} vector of new names to be addressed to the bands.
#'
#' @returns A \code{stars} object with updated band names.
#'
#' @seealso \code{\link{input_sdm} \link{sdm_area} \link{scen}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' scen
#' get_band_names(scen)
#' scen2 <- set_band_names(scen, paste0("bioclimatics_", 1:19))
#' get_band_names(scen2)
#'
#' @rdname band_names
#' @export
set_band_names <- function(x, new_names) {
  if (class(x) == "stars") {
    st_dimensions(x)$band$values <- new_names
  } else {
    stop("x must be a stars object")
  }
  return(x)
}

#' @rdname band_names
#' @export
get_band_names <- function(x) {
  if (class(x) == "stars") {
    return(st_dimensions(x)$band$values)
  } else {
    stop("x must be a stars object")
  }
}
