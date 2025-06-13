#' Hydrologic Variables
#'
#' A \code{sf} LINESTRING object with hydrologic variables (LENGTH_KM and DIST_DN_KM) for the Parana state in Brazil.
#' Data obtained from HydroSHEDS for rivers from order < 3.
#'
#' @format ## `bioc`
#' A \code{sf} with 3748 attributes and 2 fiels:
#' \describe{
#'   \item{LENGTH_KM}{Length of the river reach segment, in kilometers.}
#'   \item{DIST_DN_KM}{Distance from the reach outlet, i.e., the most downstream pixel of the reach, to the final downstream location along the river network, in kilometers. This downstream location is either the pour point into the ocean or an endorheic sink.}
#' }
#' @source <https://www.hydrosheds.org/>
"rivs"
