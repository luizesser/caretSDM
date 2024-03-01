#' S3 Methods for plot
#'
#' This function creates different plots depending on the input.
#'
#' @param x Object to be plotted.
#'
#' @return A predictors object.
#'
#' @seealso \code{\link{WorldClim_data}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#' @import mapview
#' @import viridis
#' @importFrom gtools mixedsort
#' @importFrom data.table rbindlist
#' @importFrom stringdist stringdist
#'
#' @export
plot.input_data <- function(x, what=NULL, spp_name=NULL, scenario=NULL, id=NULL, pa=TRUE, ensemble_type='mean_occ_prob') {
  if(is.null(what)){
    if("predictions" %in% names(x)){
      if("ensembles" %in% names(x$predictions)){
        #tmp <- plot(x$predictions, spp_name, scenario, id, ensemble=TRUE, ensemble_type) # plot ensembles
        plot(x$predictions, spp_name, scenario, id, ensemble=TRUE, ensemble_type)
      } else {
        #tmp <- plot(x$predictions, spp_name, scenario, id, ensemble=FALSE) # plot predictions
        plot(x$predictions, spp_name, scenario, id, ensemble=FALSE)
      }
    } else if("models" %in% names(x)) {
      #tmp <- plot(x$models, spp_name, id)# plot models
    } else if("occurrences" %in% names(x)){
      #tmp <- plot(x$occurrences, spp_name, pa)# plot occurrences
    }
  } else {
    valid_inputs <- c('predictions', 'ensembles', 'models', 'occurrences')
    what <- stringdist::stringdist(what, valid_inputs)
    #tmp <- plot(x[[what]])# plot what
    plot(x[[what]])
  }
  #return(tmp)
}

#' @exportS3Method base::plot
plot.occurrences <- function(x, spp_name=NULL, pa=TRUE) {
  df <- x$occurrences
  valid_spp <- unique(df$species)
  if(!is.null(spp_name)){spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]} else {spp_name <- valid_spp[1]}
  df <- filter(df,species==spp_name)
  tmp <- plot(df['species'])
  return(tmp)
}

#' @exportS3Method mapview::mapview
mapview.occurrences <- function(x) {
  df <- x$occurrences
  cls <- find_columns(df)
  coordinates(df) <- cls[2:3]
  tmp <- mapview(df, zcol = cls[1], layer.name = "Species")
  return(tmp)
}

#' @exportS3Method base::plot
plot.predictors <- function(x) {
  st <- x$grid
  if("variable_selection" %in% names(x)){st <- st[[x$variable_selection$vif$selected_variables]]}
  nc <- length(unique(st[[1]]))
  tmp <- plot(st, col=viridis(nc))
  return(tmp)
}

#' @exportS3Method base::plot
plot.models <- function(x) {
  ids <- x$validation$metrics
  alg <- x$algorithms
  ids <- as.data.frame(sapply(alg, function(x){rownames(ids[ids$algo==x,])}, USE.NAMES = T))
  models <- sapply(alg,function(a){x$models[names(x$models) %in% ids$a]},  USE.NAMES = T)
  hyp <- as.vector(unlist(sapply(names(models),function(m){colnames(models[[m]][[1]]$bestTune)}, USE.NAMES=T)))
  sa <- sapply(models, function(m){
    sapply(m, function(m2){
      cols <- colnames(m2$results) %in% c(hyp, "ROC")
      df <- m2$results[,cols]
      return(df)
    }, USE.NAMES=T)
  }, USE.NAMES=T)
 df <- lapply(sa, function(x){
    as.data.frame(rbindlist(apply(x, 2, as.data.frame)))
  })
  tmp <- lapply(df, function(x){
    if(ncol(x) > 2){
      sd <- apply(x, 2, sd)
      sd['ROC'] <- 0
      for(n in names(which(sd!=0))){
        tmp2 <- plot(x[,c(n,'ROC')])
      }
    } else {
      tmp2 <- plot(x)
    }
    return(tmp2)
  })
  return(tmp)
}

#' @exportS3Method base::plot
plot.predictions <- function(x, spp_name=NULL, scenario=NULL, id=NULL, ensemble=TRUE, ensemble_type='mean_occ_prob') {
  valid_spp <- names(x$predictions[[1]])
  valid_scen <- names(x$predictions)
  ens <- ifelse(ensemble, 'ensembles', 'predictions')
  # valid_id
  grd <- x$grid
  if(!is.null(scenario)){scenario <- valid_scen[which.min(stringdist::stringdist(scenario, valid_scen))]} else {scenario <- valid_scen[1]}
  if(!is.null(spp_name)){spp_name <- valid_spp[which.min(stringdist::stringdist(spp_name, valid_spp))]} else {spp_name <- valid_spp[1]}

  if(ensemble){
    v <- x[[ens]][[spp_name,scenario]][,ensemble_type]
    cell_id <- v$cell_id
    grd <- filter(grd, grd$cell_id==cell_id)
    grd['result'] <- v
  } else {
    v <- x[[ens]][[scenario]][[spp_name]][[id]]$presence
    cell_id <- v$cell_id
    grd <- filter(grd, grd$cell_id==cell_id)
    grd['result'] <- v
  }
  return(plot(st_as_stars(grd['result'])))
}
