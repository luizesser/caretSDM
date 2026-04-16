# Custom Mahalanobis Distance model to be implemented in caret
#' @keywords internal
#' @importFrom stats cov mahalanobis pchisq ecdf
.mahal.dist <- list(

  label = "Mahalanobis Distance Classifier",
  library = NULL,
  type = "Classification",

  parameters = data.frame(
    parameter = c("abs", "method"),
    class = c("logical", "character"),
    label = c("Absolute Binarization", "Suitability method")
  ),

  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(
      abs = c(TRUE, FALSE),
      method = c("chisq", "ecdf")
    )
  },

  fit = function(x, y, wts, param, lev, last, classProbs, ...) {

    # Use only presence data
    presence_data <- x[y == "presence", , drop = FALSE]

    if (nrow(presence_data) < 2) {
      stop("Not enough 'presence' data points to calculate covariance.")
    }

    # Core Mahalanobis parameters
    center_vec <- colMeans(presence_data, na.rm = TRUE)
    cov_mat <- stats::cov(presence_data)
    inv_cov_matrix <- solve(cov_mat)

    # Training distances (for ECDF)
    d2_train <- stats::mahalanobis(
      x = presence_data,
      center = center_vec,
      cov = inv_cov_matrix,
      inverted = TRUE
    )

    # Store model
    result <- list(
      center = center_vec,
      inv_cov = inv_cov_matrix,
      df = ncol(x),
      abs = param$abs,
      method = param$method,
      ecdf_fun = stats::ecdf(d2_train),
      levels = lev
    )

    return(result)
  },

  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {

    probs <- .mahal.dist$prob(modelFit, newdata)

    if (modelFit$abs) {
      pred <- ifelse(
        probs[, modelFit$levels[1]] >= 0.05,
        modelFit$levels[1],
        modelFit$levels[2]
      )
    } else {
      pred <- colnames(probs)[apply(probs, 1, which.max)]
    }

    factor(pred, levels = modelFit$levels)
  },

  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {

    d2 <- stats::mahalanobis(
      x = newdata,
      center = modelFit$center,
      cov = modelFit$inv_cov,
      inverted = TRUE
    )

    # Switch between methods
    if (modelFit$method == "chisq") {

      p_presence <- 1 - stats::pchisq(d2, df = modelFit$df)

    } else if (modelFit$method == "ecdf") {

      p_presence <- 1 - modelFit$ecdf_fun(d2)

    } else {
      stop("Unknown method: ", modelFit$method)
    }

    prob_df <- data.frame(
      presence = p_presence,
      pseudoabsence = 1 - p_presence
    )

    colnames(prob_df) <- modelFit$levels
    return(prob_df)
  },

  predictors = function(x, ...) {
    names(x$center)
  },

  varImp = function(object, ...) {
    # simple proxy: variance contribution
    data.frame(Overall = diag(solve(object$inv_cov)))
  },

  levels = function(x) x$levels,

  tags = c("mahalanobis", "Presence-Only", "ECDF", "Nonparametric")
)
