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
#' @import stars
#' @import raster
#' @importFrom here here
#' @importFrom gtools mixedsort
#'
#' @export
scenarios <- function(x, ...){
  UseMethod("scenarios")
}

#' @export
scenarios.RasterStack <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL, scenarios_names=NULL){
  if(is.null(predictors_names)){predictors_names <- names(x)}
  if(!is.null(study_area) & is.null(rescaling)){x <- mask(crop(x, study_area), study_area)}
  xb <- x
  x <- st_as_stars(cbind(coordinates(x),as.data.frame(x)))
  st_crs(x) <- as.character(crs(xb))
  if(is.null(scenarios_names)){
    if(length(unique(l)) == length(l)){
      scenarios_names <- basename(l)
    } else {
      paste0('scenario_',1:length(l))
    }
  }
  names(x) <- scenarios_names
  grd <- NULL
  if(!is.null(study_area) & !is.null(rescaling)){
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if(is.null(rescaling$epsg)){crs2 <- as.character(crs(xb))[1]} else {crs2 <- as.character(st_crs(rescaling$epsg))[1]}
    study_area <- st_transform(study_area, crs2)
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs=crs2)
    grd <- st_as_sf(data.frame(cell_id=seq(1,length(grd)),as.data.frame(grd)))
    grd <- grd[study_area,] # save grd in output? makek write_functions to save things.
    # rescaling
    if(is.na(st_crs(grd))){st_crs(grd)<-st_crs(x)}
    x <- aggregate(x, grd, FUN = function(y){mean(na.omit(y))})
    #x %>% split("band") %>% as.data.frame() %>% head()
    resolution <- c(x=cellsize,y=cellsize)
    coords <- as.data.frame(st_coordinates(st_centroid(grd)))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  }
  bbox <- st_bbox(x)[c(1,3,2,4)]
  epsg <- as.character(st_crs(x))[1]

  if(is.null(grd)){
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              paths=NULL,
              data=x)
  } else {
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              data=x,
              paths=NULL,
              grid=grd)
  }

  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios.data.frame <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL, scenarios_names=NULL, epsg=NULL){ # pode entrar tanto uma tabela com coord e spp quanto sem.
  x <- st_as_stars(x)
  if(is.null(scenarios_names)){
    if(length(unique(x)) == length(x)){
      scenarios_names <- basename(x)
    } else {
      paste0('scenario_',1:length(x))
    }
  }
  names(x) <- scenarios_names
  grd <- NULL
  if(!is.null(study_area) & !is.null(rescaling)){
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if(is.null(rescaling$epsg)){crs2 <- as.character(st_crs(epsg))[1]} else {crs2 <- as.character(st_crs(rescaling$epsg))[1]}
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs=crs2)
    grd <- st_as_sf(data.frame(cell_id=seq(1,length(grd)),as.data.frame(grd)))
    grd <- grd[study_area,] # save grd in output?
    # rescaling
    if(is.na(st_crs(grd))){st_crs(grd)<-st_crs(x)}
    x <- aggregate(x, grd, FUN = function(y){mean(na.omit(y))})
    #x %>% split("band") %>% as.data.frame() %>% head()
    resolution <- c(x=cellsize,y=cellsize)
    coords <- as.data.frame(st_coordinates(st_centroid(grd)))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  }
  bbox <- st_bbox(x)[c(1,3,2,4)] # st_bbox
  epsg <- as.character(st_crs(x))[1]

  if(is.null(grd)){
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              paths=NULL,
              data=x)
  } else {
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              data=x,
              paths=NULL,
              grid=grd)
  }

  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios.SpatRaster <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL){
  x <- st_as_stars(x)
  names(st_dimensions(x)) <- c('x', 'y', 'band')
  occ <- scenarios(x, study_area, predictors_names, rescaling)
  return(occ)
}

#' @export
scenarios.stars <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL, scenarios_names=NULL){
  if(is.null(predictors_names)){predictors_names <- names(x)}
  if(!is.null(study_area) & is.null(rescaling)){
    if(!all(st_is_valid(study_area))){study_area <- st_make_valid(study_area)}
    suppressWarnings(x <- st_crop(x,st_as_sf(st_union(study_area))))
    resolution <- st_res(x)
    coords <- st_coordinates(x)[,c('x','y')]
    df <- as.data.frame(split(x,'band'))
    cell_id <- na.omit(data.frame(cell_id=1:nrow(df), df))[,'cell_id']
    grd <- st_make_grid(x, n=c(ncol(x),nrow(x)))
  } else if(!is.null(study_area) & !is.null(rescaling)){
    # criar grid a partir de study_area
    cellsize <- rescaling$cellsize
    if(is.null(rescaling$epsg)){crs2 <- as.character(st_crs(x))[1]} else {crs2 <- as.character(st_crs(rescaling$epsg))[1]}
    study_area <- st_transform(study_area, crs2)
    grd <- st_make_grid(study_area, cellsize = cellsize)
    grd <- st_transform(grd, crs=crs2)
    grd <- st_as_sf(data.frame(cell_id=seq(1,length(grd)),as.data.frame(grd)))
    grd <- grd[study_area,] # save grd in output? makek write_functions to save things.
    # rescaling
    if(is.na(st_crs(grd))){st_crs(grd)<-st_crs(x)}
    if(!st_crs(grd)==st_crs(x)){x <- st_transform(x, crs=crs2)}
    x <- aggregate(x, grd, FUN = function(y){mean(na.omit(y))})
    resolution <- c(x=cellsize,y=cellsize)
    suppressWarnings(coords <- as.data.frame(st_coordinates(st_centroid(grd))))
    cell_id <- grd$cell_id
  } else {
    resolution <- st_res(x)
    coords <- st_coordinates(x)
    df <- as.data.frame(x)
    cell_id <- na.omit(data.frame(cell_id=1:ncell(x), df))[,'cell_id']
  }
  bbox <- st_bbox(x)[c(1,3,2,4)]
  epsg <- as.character(st_crs(x))[1]

  if(is.null(grd)){
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              paths=NULL,
              data=x)
  } else {
    x <- list(coords=coords,
              bbox=bbox,
              resolution=resolution,
              epsg=epsg,
              cell_id=cell_id,
              data=x,
              paths=NULL,
              grid=grd)
  }

  scen <- .scenarios(x)
  return(scen)
}

#' @export
scenarios.character <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL, scenarios_names=NULL, ...){
  l <- list.files(x, full.names = T, ...)
  #s <- lapply(l, function(x){s <- read_stars(x)})
  s <- read_stars(l)
  if(is.null(scenarios_names)){
    if(length(unique(l)) == length(l)){
      scenarios_names <- basename(l)
    } else {
      paste0('scenario_',1:length(l))
    }
  }
  names(s) <- scenarios_names
  scen <- scenarios(s, study_area, predictors_names, rescaling, scenarios_names)
  return(scen)
}

#' @export
scenarios.list <- function(x, study_area=NULL, predictors_names=NULL, rescaling=NULL, scenarios_names=NULL, ...){
  s <- x
  s <- sapply(s, function(x){scenarios(x, study_area=study_area, predictors_names=predictors_names, rescaling=rescaling, ...)}, simplify = F, USE.NAMES = TRUE)

  coords <- lapply(s, function(x){x$coords})
  if(all(apply(combn(length(coords), 2), 2, function(x){all.equal(coords[[x[1]]], coords[[x[2]]])}))){
    coords <- coords[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  bbox <- lapply(s, function(x){x$bbox})
  if(all(apply(combn(length(bbox), 2), 2, function(x){all.equal(bbox[[x[1]]], bbox[[x[2]]])}))){
    bbox <- bbox[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  resolution <- lapply(s, function(x){x$resolution})
  if(all(apply(combn(length(resolution), 2), 2, function(x){all.equal(resolution[[x[1]]], resolution[[x[2]]])}))){
    resolution <- resolution[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  epsg <- lapply(s, function(x){x$epsg})
  if(all(apply(combn(length(epsg), 2), 2, function(x){all.equal(epsg[[x[1]]], epsg[[x[2]]])}))){
    epsg <- epsg[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  cell_id <- lapply(s, function(x){x$cell_id})
  if(all(apply(combn(length(cell_id), 2), 2, function(x){all.equal(cell_id[[x[1]]], cell_id[[x[2]]])}))){
    cell_id <- cell_id[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  paths <- lapply(s, function(x){x$paths})
  if(all(apply(combn(length(paths), 2), 2, function(x){all.equal(paths[[x[1]]], paths[[x[2]]])}))){
    paths <- paths[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  grid <- lapply(s, function(x){x$grid})
  if(all(apply(combn(length(grid), 2), 2, function(x){all.equal(grid[[x[1]]], grid[[x[2]]])}))){
    grid <- grid[[1]]
  } else {
    stop('Scenarios are different from each other.')
  }
  s_data <- sapply(s, function(x){x$data}, simplify = F, USE.NAMES = TRUE)

  x <- list(coords=coords,
            bbox=bbox,
            resolution=resolution,
            epsg=epsg,
            cell_id=cell_id,
            paths=paths,
            grid=grid,
            data=s_data)
  scen <- .scenarios(x)
  return(scen)
}

#' @export
.scenarios <- function(x){
  scen <- structure(list(coords=x$coords,
                        bbox=x$bbox,
                        resolution=x$resolution,
                        epsg=x$epsg,
                        cell_id=x$cell_id,
                        paths=x$paths,
                        grid=x$grid,
                        data=x$data),
                   class = "scenarios")
  return(scen)
}

#' Print method for scenarios
#' @exportS3Method base::print
print.scenarios <- function(x) {
                                         cat("          caretSDM         \n")
                                         cat("...........................\n")
                                         cat("Class                     : Scenarios\n")
                                         cat("Scenarios Names           :", names(x$data), "\n")
                                         cat("Number of Scenarios       :", length(x$data), "\n")
  if(!is.null(x$bbox)){                  cat("Extent                    :", x$bbox, "(xmin, xmax, ymin, ymax)\n")}
  if(!is.null(x$epsg)){                  cat("EPSG                      :", x$epsg, "\n")}
  if(!is.null(x$resolution)){            cat("Resolution                :", x$resolution, "(x, y)\n")}
  if(!is.null(x$variable_selection$vif)){cat(cat("Selected Variables (VIF)  :"), cat(x$variable_selection$vif$selected_variables, sep=', '), "\n")}
}
