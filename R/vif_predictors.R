#' Calculate VIF
#'
#' Apply VIF calculation in a predictors object.
#'
#' @param pred A predictors object
#' @param th Threshold
#' @param maxobservations Max observations to use to calculate the VIF
#'
#' @return A predictors object with VIF data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import cubelyr
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
  if (is.null(variables_selected)) {
    selected_vars <- x$predictors_names
    cat(cat("Using all variables available: "), cat(selected_vars, sep = ", "))
  }
  if (any(variables_selected %in% x$predictors_names)) {
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
