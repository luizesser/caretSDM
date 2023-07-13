#' Create a predictors object
#'
#' This function creates a new predictors object
#'
#' @param x A string with the path to GCM files.
#' @param extension Extension of stack files. Standard is ".tif", which is the extension from WorldClim 2.1.
#' @param recursive Logical. Should the function import stacks recursively (i.e. search files from folders within folders)? Standard is TRUE.
#' @param gcm_names A vector with names to be addressed to each GCM. S
#'
#' @return A list of stacks, with each element of the list corresponding to a GCM from given path.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' s <- import_gcms(path='input_data/WorldClim_data_future', extension=".tif", recursive=TRUE, gcm_names=NULL)
#' study_area <- extent(c(-57, -22, -48, -33))
#' var_names <- c("bio_1", "bio_12")
#' t <- transform_gcms(s, var_names, study_area)
#'
#' @importFrom here here
#'
#' @export
predictors <- function(x, ...){
  UseMethod("predictors", x)
}

predictors.RasterStack <- function(x){
  predictors_names <- names(x)
  coords <- coordinates(x)
  bbox <- as.numeric(bbox(x))
  resolution <- res(r)
  epsg <- as.character(x@crs)
  df <- cbind(coords,as.data.frame(x))
  cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  occ <- structure(list(predictors_names=predictors_names,
                        coords=coords,
                        bbox=bbox,
                        resolution=resolution,
                        epsg=epsg,
                        cell_id=cell_id,
                        data.frame=x),
                   class = "occurrences")
  return(occ)
}

predictors.data.frame <- function(x, ...){
  col_names <- find_columns(x, spp=FALSE, ...)
  predictor_names <- colnames(df)[,-c(col_names)]
  coords <- x[,col_names]
  bbox <- c(min(x[,col_names[1]]),
            min(x[,col_names[2]]),
            max(x[,col_names[1]]),
            max(x[,col_names[2]]))
}

.predictors <- function(x){
  occ <- structure(list(predictors_names=predictors_names,
                        coords=coords,
                        bbox=bbox,
                        resolution=resolution,
                        epsg=epsg,
                        data.frame=x),
                   class = "occurrences")
  return(occ)
}

