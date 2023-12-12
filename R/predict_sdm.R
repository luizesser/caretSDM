#' Predict SDM models in new data
#'
#' This function projects SDM models to new scenarios
#'
#' @param m A models object
#' @param scen A scenarios object
#' @param th Thresholds for metrics to be used
#'
#' @return A predictions object
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import caret
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#'
#' @export
predict_sdm <- function(m, scen=NULL, th=0.9, tp='prob', file=NULL, ensembles=TRUE){
  if(class(m)=='input_sdm'){
    y <- m$models
    scen <- m$scenarios
  } else {
    y <- m
  }
  if(th == 'mean'){
    tm <- paste0('threshold: mean')
    means <- with(y$validation$metrics, by(ROC, algo, function(x){mean(x, na.rm=T)}))
    #th <- y$validation$metrics[y$validation$metrics[,'ROC']>means,]
  } else {
    if(is.numeric(th)){
      tm <- paste0('threshold: ',th)
      th1 <- y$validation$metrics[y$validation$metrics[,'ROC']>th,]
    }
  }

  m1 <- subset(y$models,names(y$models) %in% rownames(th1))

  if(length(m1)==0){
    stop("No models passing the threshold.")
  }

  #scen$df$cell_id <- seq_along(scen$df[,1])
  #p <- predict(m1, newdata=na.omit(scen$df), type=tp)
  if(class(scen$data)=='data.frame'){scen$data <- list(current=scen$data)}
  p <- list()
  for (i in 1:length(scen$data)) {
    print(paste0('Projecting: ',i,'/',length(scen$data)))
    x <- scen$data[[i]]
    if(class(x)!='data.frame'){x <- as.data.frame(x)}
    x <- na.omit(x[,y$predictors])
    p[[i]] <- predict(m1, newdata=x, type=tp)
    if(!ensembles){write.csv(p[[i]], paste0(names(scen$data)[i], '.csv'))}
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
  if(ensembles){
    e <- sapply(p, function(x){
      # Prepare data
      df <- bind_cols(x)
      df <- select(df, contains('presence'))
      # mean_occ_prob
      mean_occ_prob <- rowMeans(df)
      # wmean_AUC
      wmean_AUC <- apply(df,1,function(x){stats::weighted.mean(x,th1$ROC)})
      # Obtain Thresholds:
      th2 <- lapply(m1, function(x){thresholder(x,
                                               threshold = seq(0, 1, by = 0.01),
                                               final = TRUE,
                                               statistics = "all")})
      th2 <- lapply(th2, function(x){ x <- x %>% mutate(th=Sensitivity+Specificity)
                                      th <- x[x$th==max(x$th),"prob_threshold"]
                                      if(length(th)>1){th <- mean(th)}
                                      return(th)})
      # binary
      for (i in 1:ncol(df)) {
        df[,i] <- ifelse(df[,i][]>th2[i],1,0)
      }
      committee_avg <- rowMeans(df)

      # save everything
      df <- data.frame(mean_occ_prob, wmean_AUC, committee_avg)
      return(df)

    }, USE.NAMES = T)
  }
  p2 <- list(thresholds=list(values=th1, method=tm, criteria=th),
             predictions=p,
             models=m,
             file=file,
             ensembles=e,
             grid=scen$grid)
  predictions <- .predictions(p2)
  if(class(m)=='input_sdm'){
    m$predictions <- predictions
    predictions <- m
  }
  return(predictions)
}


#' @export
.predictions <- function(x){
  predictions <- structure(list(
                           thresholds=x$thresholds,
                           predictions=x$predictions,
                           models=x$models,
                           file=x$file,
                           ensembles=x$ensembles,
                           grid=x$grid),
                      class = "predictions")
  return(predictions)
}

#' Print method for predictors
#' @exportS3Method base::print
print.predictions <- function(x) {
  cat("         caretSDM        \n")
  cat(".........................\n")
  cat("Class             : Predictions\n")
  cat("Ensembles         :\n",
      "        Methods  :", rownames(x$ensembles), "\n")
  cat("Thresholds        :\n",
      "        Method   :", x$thresholds$method, "\n",
      "        Criteria :", x$thresholds$criteria, "\n",
      "        Metrics  :\n" )
  print(x$thresholds$values)
}
