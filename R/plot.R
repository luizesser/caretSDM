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
#'
#' @exportS3Method base::plot
plot.input_data <- function(x) {
  tmp <- plot()
  return(tmp)
}

#' @exportS3Method base::plot
plot.occurrences <- function(x) {
  df <- x$occurrences
  cls <- find_columns(df)
  coordinates(df) <- cls[2:3]
  tmp <- plot(df)
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
plot.predictions <- function(x, scenario=NULL, id=NULL, ensemble=NULL) {
  r <- x$grid[[1]]
  if(!is.null(scenario)){
    if(scenario %in% names(x$predictions)){
      if(is.null(ensemble)){
        if(is.null(id)){
          r[] <- ifelse(is.na(r[]),NA,x$predictions[[scenario]][[1]]$presence)
          tmp <- plot(r)
        } else {
          r[] <- ifelse(is.na(r[]),NA,x$predictions[[scenario]][[id]]$presence)
          tmp <- plot(r)
        }

      } else {
        r[] <- ifelse(is.na(r[]),NA,x$ensembles[ensemble,scenario])
        tmp <- plot(r)
      }
    }
  } else {
    if(is.null(ensemble)){
      if(is.null(id)){
        r[] <- ifelse(is.na(r[]),NA,x$predictions[[1]][[1]]$presence)
        tmp <- plot(r)
      } else {
        r[] <- ifelse(is.na(r[]),NA,x$predictions[[1]][[id]]$presence)
        tmp <- plot(r)
      }
    } else {
      r[] <- ifelse(is.na(r[]),NA,x$ensembles[ensemble,1])
      tmp <- plot(r)
    }
  }
  return(tmp)
}
