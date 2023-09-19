#' Predict SDM models in new data
#'
#' This function projects SDM models to new scenarios
#'
#' @param m A models object
#' @param scen A scenarios object
#' @param th Thresholds for metrics to be used
#'
#' @return A models object
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import caret
#'
#' @export
predict_sdm <- function(m, scen, th=0.8, tp='prob', file=NULL){
  if(th == 'mean'){
    tm <- paste0('threshold: mean')
    means <- with(m$validation$metrics, by(ROC, algo, function(x){mean(x, na.rm=T)}))
    #th <- m$validation$metrics[m$validation$metrics[,'ROC']>means,]
  } else {
    if(is.numeric(th)){
      tm <- paste0('threshold: ',th)
      th1 <- m$validation$metrics[m$validation$metrics[,'ROC']>th,]
    }
  }

  m1 <- subset(m$models,names(m$models) %in% rownames(th1))
  scen$df$cell_id <- seq_along(scen$df[,1])
  p <- predict(m1, newdata=scen$df, type=tp)
  p <- lapply(p, function(x){x <- na.omit(subset(x,select=c('presence')))})
  p2 <- do.call(cbind, p)
  colnames(p2) <- names(p)
  p2$cell_id <- rownames(p2)
  p2 <- merge(scen$df, p2, by = "cell_id", all.x = TRUE)
  p2 <- p2[,c('cell_id', 'x', 'y', names(p))]
  if(!is.null(file)){
    write.csv(p2, file)
  }

  p3 <- list(thresholds=list(values=th1, method=tm, criteria=th),
             predictions=p2,
             models=m,
             file=file)
  predictions <- predictions(p3)
  return(predictions)
}


#' @export
.predictions <- function(x){
  predictions <- structure(list(
                           thresholds=x$thresholds,
                           predictions=x$predictions,
                           models=x$models,
                           file=x$file),
                      class = "predictions")
  return(predictions)
}

#' Print method for predictors
#' @export
print.predictions <- function(x) {
  cat("Predictions Object:\n")
  cat("Thresholds Names:", x$algorithms, "\n")
  cat("Thresholds:\n",
      "Method:", x$thresholds$method, "\n",
      "Criteria:", x$thresholds$criteria, "\n",
      "Metrics:\n" )
  print(x$thresholds$values)
}
