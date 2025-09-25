# Custom Mahalanobis Distance model to be implemented in caret
#' @keywords internal
#' @importFrom stats cov mahalanobis pchisq
.mahal.dist <- list(
  label = "Mahalanobis Distance Classifier",
  library = NULL,
  type = "Classification",
  parameters = data.frame(
    parameter = c("abs"),
    class = c("logical"),
    label = c("Absolute Binarization")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    # We define a simple grid that will test both TRUE and FALSE
    # for the 'abs' parameter. Here, search can be anything that
    # the output is the same. But in other implementations the user
    # may want to change when search is not "grid".
    if (search == "grid") {
      out <- expand.grid(abs = c(TRUE, FALSE))
    } else {
      out <- expand.grid(abs = c(TRUE, FALSE))
    }
    return(out)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    # The 'fit' function is trained only on presence data.
    # It calculates and stores the mean vector and inverse covariance matrix.
    presence_data <- x[y == "presence", , drop = FALSE]

    if (nrow(presence_data) < 2) {
      stop("Not enough 'presence' data points to calculate covariance.")
    }

    # Calculate model parameters
    center_vec <- colMeans(presence_data, na.rm = TRUE)
    inv_cov_matrix <- solve(stats::cov(presence_data))

    # The model object here is just a list of parameters.
    result <- list(
      center = center_vec,
      inv_cov = inv_cov_matrix,
      df = ncol(x), # Correction demonstrated by Etherington 2019.
      abs = param$abs,
      levels = lev # Retain data information dor consistency.
    )
    return(result)
  },
  # Prediction function (must match caret's expected signature)
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # 'predict' generates class labels based on the probabilities.
    # 1. Get the probabilities by calling the 'prob' function.
    probs <- .mahal.dist$prob(modelFit, newdata)

    # 2. The 'abs' parameter determines the binarization type.
    if (modelFit$abs) {
      # For "Absolute Binarization", we threshold the p-value.
      # A common choice is alpha = 0.05. If p-value >= 0.05, the point is
      # considered within the "presence" environment.
      pred <- ifelse(probs[, modelFit$levels[1]] >= 0.05,
                     modelFit$levels[1], # presence
                     modelFit$levels[2]) # pseudoabsence
    } else {
      # Standard method: assign the class with the highest probability.
      pred <- colnames(probs)[apply(probs, 1, which.max)]
    }

    # 3. Return a factor with the correct levels.
    pred <- factor(pred, levels = modelFit$levels)
    return(pred)
  },

  predictors = function(x, ...) {
    # This correctly extracts predictor names from the fitted model.
    names(x$center)
  },

  # Optional: Specify if probabilities are supported
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # 'prob' calculates class probabilities using the fitted model.
    # 1. Calculate the squared Mahalanobis distance (D^2) for newdata.
    d2 <- stats::mahalanobis(x = newdata,
                             center = modelFit$center,
                             cov = modelFit$inv_cov,
                             inverted = TRUE) # Use inverted = TRUE for efficiency ######################

    # 2. Convert distance to a p-value using the chi-squared distribution.
    # This p-value can be interpreted as the probability of "presence".
    p_presence <- 1 - stats::pchisq(q = d2, df = modelFit$df)

    # 3. The output is a data frame of probabilities for both classes.
    prob_df <- data.frame(
      presence = p_presence,
      pseudoabsence = 1 - p_presence
    )
    colnames(prob_df) <- modelFit$levels # Ensure column names match levels
    return(prob_df)
  },
  tags = c("mahalanobis", "Presence-Only")
)
