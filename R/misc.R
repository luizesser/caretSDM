#' Miscelaneous functions for \code{input_sdm} objects
#'
#' Set of functions to retrieve information and data from \code{input_sdm} objects.
#'
#' @usage n_records(x)
#' n_pseudoabsences(x)
#' pseudoabsence_method(x)
#' pseudoabsence_data(x)
#' species_names(x)
#' predictors_names(x)
#' get_coords(x)
#' get_predictors(x)
#' scenarios_names(x)
#' get_scenarios_data(x)
#' get_tune_length(x)
#' algorithms_used(x)
#' get_models(x)
#' get_validation_metrics(x)
#' mean_validation_metrics(x)
#' get_predictions(x)
#' get_ensembles(x)
#'
#' @param x A input_sdm object.
#'
#' @seealso \code{\link{input_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
n_records <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$n_presences)
}

#' @export
n_pseudoabsences <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$n_pa)
}

#' @export
pseudoabsence_method <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$method)
}

#' @export
pseudoabsence_data <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$data)
}

#' @export
species_names <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$spp_names)
}

#' @export
predictors_names <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$predictors
  } else {
    y <- x
  }
  if ("vif" %in% names(y$variable_selection)) {
    res <- y$variable_selection$vif$selected_variables
  } else {
    res <- y$predictors_names
  }
  return(res)
}

#' @export
get_coords <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$predictors
  } else {
    y <- x
  }
  return(y$coords)
}

#' @export
get_predictors <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$predictors
  } else {
    y <- x
  }
  res <- cbind(y$grid, y$data$current)
  names(res) <- c("cell_id", y$predictors_names, "geometry")
  return(res)
}

#' @export
scenarios_names <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$scenarios
  } else {
    y <- x
  }
  res <- names(y$data)
  return(res)
}

#' @export
get_scenarios_data <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$scenarios
  } else {
    y <- x
  }
  return(y$data)
}

#' @export
get_tune_length <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  return(y$tuning)
}

#' @export
algorithms_used <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  return(y$algorithms)
}

#' @export
get_models <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  return(y$models)
}

#' @export
get_validation_metrics <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  return(y$validation$metrics)
}

#' @export
mean_validation_metrics <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$models
  } else {
    y <- x
  }
  algo <- y$algorithms
  res <- sapply(y$validation$metrics, function(met) {
    v <- summarise(group_by(met, algo), ROC = mean(ROC), Sensitivity = mean(Sens), Specificity = mean(Spec), TSS = mean(TSS))
    return(v)
  }, simplify = FALSE, USE.NAMES = TRUE)
  return(res)
}

#' @export
get_predictions <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$predictions
  } else {
    y <- x
  }
  return(y$predictions)
}

#' @export
get_ensembles <- function(x) {
  if (class(x) == "input_sdm") {
    y <- x$predictions
  } else {
    y <- x
  }
  return(y$ensembles)
}

#' @export
set_band_names <- function(x, new_names) {
  if (class(x) == "stars") {
    st_dimensions(x)$band$values <- new_names
  } else {
    stop('x must be a stars object')
  }
  return(x)
}

#' @export
get_band_names <- function(x) {
  if (class(x) == "stars") {
    return(st_dimensions(x)$band$values)
  } else {
    stop('x must be a stars object')
  }
}
