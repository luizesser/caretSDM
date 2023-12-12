# Custom MaxEnt model to be implemented in caret:
maxent <- list(label = "Maximum Entropy Modeling",
               library = "maxnet",
               loop = NULL,
               type = c("Classification","Regression"),
               levels = c("presence","pseudoabsence"),
               parameters = data.frame(parameter = c("regmult"),
                                       class = c("numeric"),
                                       label = c("Regularization Multiplier")),
               grid = function(x, y, len = NULL, search = "grid") {
                 if(search == "grid") { out <- expand.grid(regmult = 1:len)
                 } else { out <- expand.grid(regmult = 1:len) }
                 out},
               fit = function(x, y, wts, param, lev, last, classProbs, ...){
                 model <- maxnet::maxnet(p=ifelse(as.numeric(y)==1,1,0), data=x,
                                f = maxnet.formula(p=ifelse(as.numeric(y)==1,1,0), data=x),
                                regmult=param$regmult,
                                regfun = maxnet.default.regularization, addsamplestobackground=T, ...)
                 return(model)
               },
               predict = function(modelFit, newdata, preProc = NULL, submodels = NULL){
                 pred_maxnet <- function (object, newdata, clamp = T, type = c("logistic"), ...){
                   if (clamp) {
                     for (v in intersect(names(object$varmax), names(newdata))) {
                       newdata[, v] <- pmin(pmax(newdata[, v], object$varmin[v]),
                                            object$varmax[v])
                     }
                   }
                   hingeval <- function(x, min, max){pmin(1, pmax(0, (x-min)/(max-min)))}
                   terms <- sub("hinge\\((.*)\\):(.*):(.*)$", "hingeval(\\1,\\2,\\3)",
                                names(object$betas))
                   terms <- sub("categorical\\((.*)\\):(.*)$", "categoricalval(\\1,\"\\2\")",
                                terms)
                   terms <- sub("thresholds\\((.*)\\):(.*)$", "thresholdval(\\1,\\2)",
                                terms)
                   f <- formula(paste("~", paste(terms, collapse = " + "), "-1"))
                   mm <- model.matrix(f, data.frame(newdata))
                   if (clamp)
                     mm <- t(pmin(pmax(t(mm), object$featuremins[names(object$betas)]),
                                  object$featuremaxs[names(object$betas)]))
                   link <- (mm %*% object$betas) + object$alpha
                   if (type == "logistic"){ res <- 1 / (1 + exp(-object$entropy - link)) }
                   return(res)
                 }
                 pred <- pred_maxnet(modelFit, newdata, clamp=T, type=c("logistic"))
                 pred <- data.frame(presence=pred, pseudoabsence=1-pred)
                 pred <- as.factor(colnames(pred)[apply(pred,1,which.max)])
                 return(pred)
               },
               prob = function(modelFit, newdata, preProc = NULL, submodels = NULL){
                 prob_maxnet <- function (object, newdata, clamp = T, type = c("cloglog"), ...){
                   if (clamp) {
                     for (v in intersect(names(object$varmax), names(newdata))) {
                       newdata[, v] <- pmin(pmax(newdata[, v], object$varmin[v]),
                                            object$varmax[v])
                     }
                   }
                   hingeval <- function(x, min, max){pmin(1, pmax(0, (x-min)/(max-min)))}
                   terms <- sub("hinge\\((.*)\\):(.*):(.*)$", "hingeval(\\1,\\2,\\3)",
                                names(object$betas))
                   terms <- sub("categorical\\((.*)\\):(.*)$", "categoricalval(\\1,\"\\2\")",
                                terms)
                   terms <- sub("thresholds\\((.*)\\):(.*)$", "thresholdval(\\1,\\2)",
                                terms)
                   f <- formula(paste("~", paste(terms, collapse = " + "), "-1"))
                   mm <- model.matrix(f, data.frame(newdata))
                   if (clamp)
                     mm <- t(pmin(pmax(t(mm), object$featuremins[names(object$betas)]),
                                  object$featuremaxs[names(object$betas)]))
                   link <- (mm %*% object$betas) + object$alpha
                   if (type == "cloglog"){ res <- 1 - exp(0 - exp(object$entropy + link)) }
                   return(res)
                 }
                 prob <- prob_maxnet(modelFit, newdata, clamp=T, type=c("cloglog"))
                 prob <- data.frame(presence=prob, pseudoabsence=1-prob)
                 return(prob)
               },
               predictors = function(x, ...) { colnames(x) },
               varImp = NULL,
               tags = c("MaxEnt"))
