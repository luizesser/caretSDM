#' Calculate VIF
#'
#' Apply Variance Inflation Factor (VIF) calculation.
#'
#' @usage vif_predictors(pred, th = 0.5, maxobservations = 5000, variables_selected = NULL)
#'
#' @param pred A \code{input_sdm} or \code{predictors} object.
#' @param th Threshold to be applied in VIF routine.
#' @param maxobservations Max observations to use to calculate the VIF.
#' @param variables_selected If there is a subset of predictors that should be used in this
#' function, it can be informed using this parameter.
#'
#' @return A \code{input_sdm} or \code{predictors} object with VIF data.
#'
#' @seealso \code{\link{usdm::vifcor}\link{predictors_names}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Create input_sdm:
#' i <- input_sdm(occurrences(occ), sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- vif_predictors(i)
#' i
#'
#' @import cubelyr
#' @import tibble
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @importFrom usdm vifcor
#'
#' @export
vif_predictors <- function(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected = NULL) {
  if (class(pred) == "input_sdm") {
    x <- pred$predictors
    occ <- pred$occurrences$occurrences
    epsg <- pred$occurrences$epsg
  } else {
    x <- pred
  }

  if (is_predictors(x)) {
    if (is.null(variables_selected)) {
      selected_vars <- x$predictors_names
      cat(cat("Using all variables available: "), cat(selected_vars, sep = ", "))
    } else if (any(variables_selected %in% x$predictors_names)) {
      selected_vars <- x$predictors_names[x$predictors_names %in% variables_selected]
      cat(cat("Using given variables: "), cat(selected_vars, sep = ", "))
    }
    if (area == "all") {
      suppressWarnings(sf_x <- st_centroid(st_as_sf(filter(x$data, band %in% selected_vars))))
      p <- select(as.data.frame(sf_x), -"geometry")
    }
    if (area == "occurrences") {
      if (!class(pred) == "input_sdm") {
        stop("Method only available with input_sdm class.")
      }
      cols <- find_columns(occ$occurrences)
      coordinates(occ$occurrences) <- cols[2:3]
      st_crs(occ) <- as.character(st_crs(epsg))[1]
      p <- st_extract(as.data.frame(x$data[selected_vars, ])[, selected_vars], st_as_sf(occ))
    }
    v <- vifcor(p, th = th, size = maxobservations)
  } else if (is_sdm_area(x)) {
    if (is.null(variables_selected)) {
      selected_variables <- x$predictors
    }
    facnum <- function(x) {
      return(as.numeric(as.factor(x)))
    }
    if (area == "all") {
      v <- x$grid |>
        as.data.frame() |>
        select(-c("geometry", "cell_id")) |>
        select(all_of(selected_variables)) |>
        mutate_if(is.character, facnum) |>
        vifcor(th = th, size = maxobservations)
    }
  }

  x$variable_selection$vif$area <- area
  x$variable_selection$vif$selected_variables <- v@variables[!v@variables %in% v@excluded]
  x$variable_selection$vif$threshold <- th
  x$variable_selection$vif$vifcor <- v
  if (class(pred) == "input_sdm") {
    pred$predictors <- x
    x <- pred
  }
  return(x)
}
