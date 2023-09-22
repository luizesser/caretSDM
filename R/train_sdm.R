#' Train SDM models
#'
#' This function fit SDM models
#'
#' @param occ A occurrences object
#' @param pred A predictors object
#' @param algo Algorithms to be used
#'
#' @return A models object
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import caret
#' @import raster
#' @importFrom dplyr arrange
#'
#' @export
train_sdm <- function(occ, pred, algo, ctrl=NULL){
  if(is.null(ctrl)){
    ctrl <- trainControl(method = "repeatedcv", number=4, repeats=1, classProbs = TRUE, returnResamp='all',# retornar folds
                         summaryFunction = twoClassSummary)
  }
  l <- list()
  occ2 <- occ$occurrences
  col_names <- find_columns(occ2)
  coordinates(occ2) <- col_names[c(2,3)]
  pa <- occ$pseudoabsences$data[[1]]
  g <- intersect(names(pred$grid), colnames(pa))
  occ2 <- extract(pred$grid[[g]], occ2)
  for(i in 1:length(occ$pseudoabsences$data)){
    pa <- occ$pseudoabsences$data[[i]]
    pa <- pa[,match(colnames(occ2), colnames(pa))]
    x <- rbind(occ2,pa)
    y <- as.factor(c(rep('presence', nrow(occ2)),rep('pseudoabsence',nrow(pa))))
    m <- lapply(algo, function(a){train(x,
                                        y,
                                        method = a,
                                        trControl = ctrl)
    })
    l[[paste0("m",i)]] <- m
  }
  m <- unlist(l, recursive = F)
  metrics <- lapply(m, function(x){
    bt <- names(x$bestTune)
    res <- x$results[,!colnames(x$results) %in% bt]
    mx <- apply(res,2,max)
    r <- cbind(data.frame(algo=x$method),t(as.data.frame(mx)))
    return(r)
  })
  metrics <- do.call(rbind, metrics)
  metrics <- arrange(metrics, algo)

  # round(with(metrics, by(ROC, algo, function(x){mean(x, na.rm=T)})), digits=3)

  m2 <- list(validation=list(method=ctrl$method, number=ctrl$number, metrics=metrics),
             predictors=colnames(pa),
             algorithms=algo,
             models=m)
  models <- .models(m2)
  return(models)
}


#' @export
.models <- function(x){
  models <- structure(list(validation=list(method=x$validation$method, number=x$validation$number, metrics=x$validation$metrics),
                           predictors=x$predictors,
                           algorithms=x$algorithms,
                           models=x$models,
                           tuning=10),
                   class = "models")
  return(models)
}

#' Print method for predictors
#' @export
print.models <- function(x) {
  cat("Models Object:\n")
  cat("Algorithms Names:", x$algorithms, "\n")
  cat("Variables Names:", x$predictors, "\n")
  cat("Model Validation:\n",
      "Method:", x$validation$method, "\n",
      "Number:", x$validation$number, "\n",
      "Metrics:\n" )
  print(x$validation$metrics)
}
