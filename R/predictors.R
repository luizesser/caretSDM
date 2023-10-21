#' Create a predictors object
#'
#' This function creates a new predictors object
#'
#' @param x A string with the path to GCM files.
#' @param predictors_names A vector with names to be addressed to each predictor.
#'
#' @return A predictors object.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#' @import raster
#' @importFrom gtools mixedsort
#'
#' @export
predictors <- function(x, ...){
  UseMethod("predictors", x)
}

#' @export
predictors.RasterStack <- function(x, predictors_names=NULL){
  if(is.null(predictors_names)){predictors_names <- names(x)}
  coords <- coordinates(x)
  bbox <- bbox(x)[c(1,3,2,4)]
  resolution <- res(x)
  epsg <- as.character(x@crs)
  df <- cbind(coords,as.data.frame(x))
  cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  x <- list(predictors_names=predictors_names,
                        coords=coords,
                        bbox=bbox,
                        resolution=resolution,
                        epsg=epsg,
                        cell_id=cell_id,
                        data=df,
                        grid=x)
  occ <- .predictors(x)
  return(occ)
}

#' @export
predictors.data.frame <- function(x, ...){ # pode entrar tanto uma tabela com coord e spp quanto sem.
  resolution <- NULL
  epsg <- NULL
  col_names <- find_columns(x, ...) # colocar um try
  if(!length(col_names) == 0){
    if(any(col_names %in% colnames(x))){predictors_names <- colnames(x)[!colnames(x) %in% col_names]}
    coords <- x[,col_names[-1]]
    bbox <- c(min(coords[,1]), max(coords[,1]),
              min(coords[,2]), max(coords[,2]))
    df <- x[,predictors_names]
  } else {
    coords <- NULL
    bbox <- NULL
    predictors_names <- colnames(x)
    df <- x
  }
  x <- list(predictors_names=predictors_names,
            coords=coords,
            bbox=bbox,
            resolution=resolution,
            epsg=epsg,
            data=df)
  occ <- .predictors(x)
  return(occ)
}

#' @export
.predictors <- function(x){
  occ <- structure(list(predictors_names=x$predictors_names,
                        coords=x$coords,
                        bbox=x$bbox,
                        resolution=x$resolution,
                        epsg=x$epsg,
                        grid=x$grid,
                        data=x$data),
                   class = "predictors")
  return(occ)
}

#' Print method for predictors
#' @export
print.predictors <- function(x) {
  cat("Predictors Object:\n")
  cat("Predictors Names:", x$predictors_names, "\n")
  cat("Number of Predictors:", length(x$predictors_names), "\n")
  if(!is.null(x$bbox)){cat("Bounding Box:", x$bbox, "\n")}
  if(!is.null(x$epsg)){cat("EPSG:", x$epsg, "\n")}
  if(!is.null(x$resolution)){cat("Resolution:", x$resolution, "\n")}
  if(!is.null(x$variable_selection$vif)){cat("Selected Variables (VIF):", x$variable_selection$vif$selected_variables, "\n")}
  cat("\nData:\n")
  print(head(x$df))
}
