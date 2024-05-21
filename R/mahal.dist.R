# Custom Mahalanobis Distance model to be implemented in caret:
mahal.dist <- list(
  label = "Mahalanobis Distance",
  library = "dismo",
  loop = NULL,
  type = c("Classification", "Regression"),
  levels = c("presence", "pseudoabsence"),
  parameters = data.frame(
    parameter = c("abs"),
    class = c("logical"),
    label = c("Absolute absence")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      out <- expand.grid(abs = c(TRUE, FALSE))
    } else {
      out <- expand.grid(abs = c(TRUE, FALSE))
    }
    return(out)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    model <- dismo::mahal(x = x[y == "presence", ])
    result <- list(model = model, abs = param$abs)
    return(result)
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    pred <- predict(modelFit$model, newdata)
    pred <- data.frame(presence = pred, pseudoabsence = 1 - pred)
    if (modelFit$abs) {
      pred <- as.factor(ifelse(pred$presence > 0, "presence", "pseudoabsence"))
    } else {
      pred <- as.factor(colnames(pred)[apply(pred, 1, which.max)])
    }
    return(pred)
  },
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    prob <- predict(modelFit$model, newdata)
    prob <- data.frame(presence = prob, pseudoabsence = 1 - prob)
    return(prob)
  },
  predictors = function(x, ...) {
    colnames(x)
  },
  varImp = NULL,
  tags = c("Distance")
)
