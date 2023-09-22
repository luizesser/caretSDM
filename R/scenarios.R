#' Create a scenarios object
#'
#' This function creates a new scenarios object, which are used to project models in space and time.
#'
#' @param x A string with the path to GCM files.
#' @param extension Extension of stack files. Standard is ".tif", which is the extension from WorldClim 2.1.
#' @param recursive Logical. Should the function import stacks recursively (i.e. search files from folders within folders)? Standard is TRUE.
#' @param scenarios_names A vector with names to be addressed to each scenario.
#' @param variables_names A vector with names to be addressed to each variable. Alternatively, a vector from names on training data/predictors.
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
#' @import raster
#' @importFrom here here
#' @importFrom gtools mixedsort
#'
#' @export
scenarios <- function(x, ...){
  UseMethod("scenarios", x)
}

#' @export
scenarios.RasterStack <- function(x){
  coords <- coordinates(x)
  bbox <- bbox(x)[c(1,3,2,4)]
  resolution <- res(x)
  epsg <- as.character(x@crs)
  cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  grid <- raster(x)
  x <- list(coords=coords,
            bbox=bbox,
            resolution=resolution,
            epsg=epsg,
            cell_id=cell_id,
            paths=NULL,
            grid=grid,
            data=x)
  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios.data.frame <- function(x, ...){ # pode entrar tanto uma tabela com coord e spp quanto sem.
  resolution <- NULL
  epsg <- NULL
  #col_names <- find_columns(x, ...) # colocar um try
  #if(!length(col_names) == 0){
  #  if(any(col_names %in% colnames(x))){scenarios_names <- colnames(x)[!colnames(x) %in% col_names]}
  #  coords <- x[,col_names[-1]]
  #  bbox <- c(min(coords[,1]), max(coords[,1]),
  #            min(coords[,2]), max(coords[,2]))
  #  df <- x[,scenarios_names]
  #} else {
  #  coords <- NULL
  #  bbox <- NULL
  #  scenarios_names <- colnames(x)
  #  df <- x
  #}
  #x <- list(scenarios_names=scenarios_names,
  #          coords=coords,
  #          bbox=bbox,
  #          resolution=resolution,
  #          epsg=epsg,
  #          df=df)
  #occ <- .scenarios(x)
  #return(occ)
}

#' @export
scenarios.character <- function(x, extension='.tif$', recursive=T, scenarios_names=NULL, variables_names=NULL, var_selected=NULL){ # pode entrar tanto uma tabela com coord e spp quanto sem.
  l <- list.files(x, pattern=extension, rec=recursive, full.names = T)
  s <- lapply(l, function(x){s <- stack(x)})
  resolution <- res(s[[1]])
  epsg <- as.character(crs(s[[1]]))
  coords <- coordinates(s[[1]])
  bbox <- bbox(s[[1]])[c(1,3,2,4)]

  if(is.null(scenarios_names)){
    if(length(unique(l)) == length(l)){
      scenarios_names <- basename(l)
    } else {
      paste0('scenario_',1:length(l))
    }
  }
  names(s) <- scenarios_names

  if(is.null(variables_names)){
    s <- lapply(s, function(x){x <- x[[mixedsort(names(x))]]
                               variables_names <- paste0('bio_', 1:19)
                               names(x) <- variables_names
                               return(x)})
  } else {
    s <- lapply(s, function(x){names(x) <- variables_names
                               return(x)})
  }

  if(is.null(var_selected)){
    var_selected <- variables_names
  }

  s <- lapply(s, function(x){x <- x[[var_selected]]
                             return(x)})
  paths <- l
  cell_id <- na.omit(data.frame(cell_id=1:ncell(x), as.data.frame(s[[1]][[1]])))[,'cell_id']

  #df <- lapply(s,function(x){na.omit(as.data.frame(x))})
  grid <- raster(s[[1]])

  x <- list(predictors_names=var_selected,
            coords=coords,
            bbox=bbox,
            resolution=resolution,
            epsg=epsg,
            cell_id=cell_id,
            paths=paths,
            grid=grid,
            data=s)
  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios.list <- function(x, ...){
  if(class(s[[1]]) == 'RasterStack'){
    s <- x
    resolution <- res(s[[1]])
    epsg <- as.character(crs(s[[1]]))
    coords <- coordinates(s[[1]])
    bbox <- bbox(s[[1]])[c(1,3,2,4)]

    if(length(unique(l)) == length(l)){scenarios_names <- basename(l)} else {paste0('scenario_',1:length(l))}
    names(s) <- scenarios_names
    paths <- l
    cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']

    grid <- raster(s[[1]])
  }

  if(class(s[[1]]) == 'data.frame'){

  }

  x <- list(predictors_names=x$predictors_names,
            coords=coords,
            bbox=bbox,
            resolution=resolution,
            epsg=epsg,
            cell_id=cell_id,
            paths=paths,
            grid=grid,
            data=s)
  scen <- .scenarios(x)
  return(scen)
}

#' @export
.scenarios <- function(x){
  occ <- structure(list(predictors_names=x$predictors_names,
                        coords=x$coords,
                        bbox=x$bbox,
                        resolution=x$resolution,
                        epsg=x$epsg,
                        cell_id=x$cell_id,
                        paths=x$paths,
                        grid=x$grid,
                        data=x$data),
                   class = "scenarios")
  return(occ)
}

#' Print method for scenarios
#' @export
print.scenarios <- function(x) {
  cat("Scenarios Object:\n")
  cat("Scenarios Names:", names(x$data), "\n")
  cat("Number of Scenarios:", length(x$data), "\n")
  if(!is.null(x$bbox)){cat("Bounding Box:", x$bbox, "\n")}
  if(!is.null(x$epsg)){cat("EPSG:", x$epsg, "\n")}
  if(!is.null(x$resolution)){cat("Resolution:", x$resolution, "\n")}
  if(!is.null(x$variable_selection$vif)){cat("Selected Variables (VIF):", x$variable_selection$vif$selected_variables, "\n")}
  cat("\nData:\n")
  print(head(x$df))
}
