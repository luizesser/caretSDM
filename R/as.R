#' sdm_as_X functions to transform caretSDM data into other classes.
#'
#' This functions transform data in a caretSDM object to be used in other packages.
#'
#' @param x caretSDM object
#' @param what Sometimes multiple objects inside x could be transformed. This parameter allows users to specify what needs to be converted.
#'
#' @returns The output is the desired class
#'
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
sdm_as_stars <- function(x, what=NULL, spp=NULL, scen=NULL, id=NULL, ens=NULL){
  if(is.null(what)){}
  all_what <- c('ensembles', 'predictions', 'predictors', 'scenarios')
  if(class(x)=='input_sdm'){
    if(what=='scenarios'){return(x$scenarios$data)}
    if(what=='predictors'){return(x$predictors$data)}
    if(what=='predictions'){
      if(is.null(spp)){spp <- names(x$predictions$predictions[[1]])[1]}
      if(is.null(scen)){scen <- names(x$predictions$predictions)[1]}
      if(is.null(id)){id <- names(x$predictions$predictions[[1]][[1]])[1]}
      grd <- x$predictors$grid
      v <- select(x$predictions$predictions[[scen]][[spp]][[id]],-'pseudoabsence')
      result <- st_as_stars(merge(grd,v, by='cell_id'))
      return(result)
    }
    if(what=='ensembles'){
      if(is.null(spp)){spp <- rownames(x$predictions$ensembles)[1]}
      if(is.null(scen)){scen <- colnames(x$predictions$ensembles)[1]}
      if(is.null(ens)){ens <- colnames(x$predictions$ensembles[[spp,scen]])[1]}
      grd <- x$predictors$grid
      v <- x$predictions$ensembles[[spp,scen]][,ens]
      cell_id <- x$predictions$predictions[[scen]][[spp]][[1]]$cell_id
      df <- data.frame(cell_id, v)
      colnames(df) <- c('cell_id', ens)
      result <- st_as_stars(merge(grd, df, by='cell_id'))
      return(result)
    }
  }
}

#' @export
sdm_as_raster <- function(x, what=NULL, spp=NULL, scen=NULL, id=NULL, ens=NULL){
  if(is.null(what)){}
  all_what <- c('ensembles', 'predictions', 'predictors', 'scenarios')
  if(class(x)=='input_sdm'){
    if(what=='scenarios'){return(x$scenarios$data)}
    if(what=='predictors'){return(x$predictors$data)}
    if(what=='predictions'){
      if(is.null(spp)){spp <- names(x$predictions$predictions[[1]])[1]}
      if(is.null(scen)){scen <- names(x$predictions$predictions)[1]}
      if(is.null(id)){id <- names(x$predictions$predictions[[1]][[1]])[1]}
      grd <- x$predictors$grid
      v <- select(x$predictions$predictions[[scen]][[spp]][[id]],-'pseudoabsence')
      result <- st_as_stars(merge(grd,v, by='cell_id'))
      return(result)
    }
    if(what=='ensembles'){
      if(is.null(spp)){spp <- rownames(x$predictions$ensembles)[1]}
      if(is.null(scen)){scen <- colnames(x$predictions$ensembles)[1]}
      if(is.null(ens)){ens <- colnames(x$predictions$ensembles[[spp,scen]])[1]}
      grd <- x$predictors$grid
      v <- x$predictions$ensembles[[spp,scen]][,ens]
      cell_id <- x$predictions$predictions[[scen]][[spp]][[1]]$cell_id
      df <- data.frame(cell_id, v)
      colnames(df) <- c('cell_id', ens)
      result <- st_as_stars(merge(grd, df, by='cell_id'))
      return(result)
    }
  }
}
