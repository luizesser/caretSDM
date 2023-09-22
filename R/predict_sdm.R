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
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
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
  #scen$df$cell_id <- seq_along(scen$df[,1])
  #p <- predict(m1, newdata=na.omit(scen$df), type=tp)
  if(class(scen$data)=='data.frame'){scen$data <- list(current=scen$data)}
  p <- list()
  for (i in 1:length(scen$data)) {
    print(paste0('Projecting: ',i,'/',length(scen$data)))
    x <- scen$data[[i]]
    if(class(x)!='data.frame'){x <- as.data.frame(x)}
    x <- x[,m$predictors]
    p[[i]] <- predict(m1, newdata=x, type=tp)
    write.csv(p[[i]], paste0(names(scen$data)[i], '.csv'))
  }
  names(p) <- names(scen$data)
  ##p <- lapply(p, function(x){x <- subset(x,select=c('presence'))
  ##                           x$cell_id <- rownames(x)
  ##                           x <- x[,c('cell_id', 'presence')]
  ##                           return(x)})
  ##models_names <- names(p)
  ##p <- reduce(p,inner_join, by='cell_id')
  ##colnames(p) <- c('cell_id',models_names)
  ##p <- merge(scen$df, p, by = "cell_id", all.x = TRUE)
  ##p <- p[,c('cell_id', 'x', 'y', models_names)]
#
  ##if(!is.null(file)){
  ##  write.csv(p, file)
  ##}

  ### INCLUIR ENSEMBLES AQUI ###


  p <- list(thresholds=list(values=th1, method=tm, criteria=th),
             predictions=p,
             models=m,
             file=file)
  predictions <- .predictions(p)
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
  cat("Thresholds:\n",
      "Method:", x$thresholds$method, "\n",
      "Criteria:", x$thresholds$criteria, "\n",
      "Metrics:\n" )
  print(x$thresholds$values)
}
