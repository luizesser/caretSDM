#' MacroEcological Models (MEM) in caretSDM
#'
#' This functions sums all species records into one.
#' Should be used before the data cleaning routine.
#'
#' @usage use_mem(x, add = TRUE, name = "MEM")
#'
#' @param x A \code{occurrences} or \code{input_sdm} object containing occurrences.
#' @param add Logical. Should the new MEM records be added to the pool (\code{TRUE}) of species or
#' the output should have only the summed records (\code{FALSE})? Standard is \code{TRUE}.
#' @param name How should the new records be named? Standard is "MEM".
#'
#' @returns A \code{input_sdm} or \code{occurrences} object with MEM data.
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
use_mem <- function(x, add = TRUE, name = "MEM") {
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else if (is_occurrences(x)) {
    y <- x
  } else {
    stop("x must be of class input_sdm or occurrences")
  }
  if (!add) {
    y$occurrences$species <- rep(name, length(y$occurrences$species))
    y$spp_names <- name
    y$n_presences <- table(y$occurrences$species)
  } else {
    df <- y$occurrences
    df2 <- df
    df2$species <- rep(name, length(df2$species))
    y$occurrences <- rbind(df, df2)
    y$spp_names <- c(y$spp_names, name)
    y$n_presences <- table(y$occurrences$species)
  }

  if (is_input_sdm(x)) {
    x$occurrences <- y
  } else if (is_occurrences(x)) {
    x <- y
  }
  return(x)
}
