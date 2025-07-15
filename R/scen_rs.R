#' Bioclimatic Variables
#'
#' A \code{stars} object with bioclimatic variables (bio1, bio4 and bio12) and four future scenarios for
#' the Rio Grande do Sul state in Brazil. Data from MIROC6 GCM from WorldClim 2.1 at 10 arc-min
#' resolution.
#'
#' @format ## `scen_rs`
#' A \code{stars} with 5 attribute and 3 bands:
#' \describe{
#'   \item{current}{Current scenario with the average values for the years 1970-2000}
#'   \item{ca_ssp245_2090}{Intermediate scenario for the year 2090 and GCM CanESM5}
#'   \item{ca_ssp585_2090}{Extreme  scenario for the year 2090 and GCM CanESM5}
#'   \item{mi_ssp245_2090}{Intermediate scenario for the year 2090 and GCM MIROC6}
#'   \item{mi_ssp585_2090}{Extreme  scenario for the year 2090 and GCM MIROC6}
#'   \item{bio1}{Annual Mean Temperature}
#'   \item{bio4}{Temperature Seasonality}
#'   \item{bio12}{Annual Precipitation}
#' }
#' @source <https://www.worldclim.org/>
"scen_rs"
