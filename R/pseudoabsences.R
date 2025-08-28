#' Obtain Pseudoabsences
#'
#' This function obtains pseudoabsences given a set of predictors.
#'
#' @usage
#' pseudoabsences(occ,
#'                pred = NULL,
#'                method = "random",
#'                n_set = 10,
#'                n_pa = NULL,
#'                variables_selected = NULL,
#'                th = 0)
#'
#' @param occ A \code{occurrences_sdm} or \code{input_sdm} object.
#' @param pred A \code{sdm_area} object. If \code{NULL} and \code{occ} is a \code{input_sdm},
#' \code{pred} will be retrieved from \code{occ}.
#' @param method Method to create pseudoabsences. One of: "random", "bioclim" or "mahal.dist".
#' @param n_set \code{numeric}. Number of datasets of pseudoabsence to create.
#' @param n_pa \code{numeric}. Number of pseudoabsences to be generated in each dataset created.
#' If \code{NULL} then the function prevents imbalance by using the same number of presence records
#' (\code{n_records(occ)}). If you want to address different sizes to each species, you must provide
#' a named vector (as in \code{n_records(occ)}).
#' @param variables_selected A vector with variables names to be used while building pseudoabsences.
#' Only used when method is not "random".
#' @param th \code{numeric} Threshold to be applied in bioclim/mahal.dist projections. See details.
#' @param i A \code{input_sdm} object.
#'
#' @returns A \code{occurrences_sdm} or \code{input_sdm} object with pseudoabsence data.
#'
#' @details
#' \code{pseudoabsences} is used in the SDM workflow to obtain pseudoabsences, a step necessary for
#' most of the algorithms to run. We implemented three methods so far: \code{"random"}, which is
#' self-explanatory, \code{"bioclim"} and \code{"mahal.dist"}. The two last are built with the idea
#' that pseudoabsences should be environmentally different from presences. Thus, we implemented
#' two presence-only methods to infer the distribution of the species. \code{"bioclim"} uses an
#' envelope approach (bioclimatic envelope), while \code{"mahal.dist"} uses a distance approach
#' (mahalanobis distance). \code{th} parameter enters here as a threshold to binarize those results.
#' Pseudoabsences are retrieved outside the projected distribution of the species.
#'
#' \code{n_pseudoabsences} returns the number of pseudoabsences obtained per species.
#'
#' \code{pseudoabsence_method} returns the method used to obtain pseudoabsences.
#'
#' \code{pseudoabsence_data} returns a \code{list} of species names. Each species name will have a
#' \code{list}s with pseudoabsences data from class \code{sf}.
#'
#' @seealso \code{link{input_sdm} \link{sdm_area} \link{occurrences_sdm}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#'
#' # Include scenarios:
#' sa <- add_scenarios(sa)
#'
#' # Create occurrences:
#' oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#'
#' # Create input_sdm:
#' i <- input_sdm(oc, sa)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsences(i, method="bioclim")
#'
#' @importFrom sf st_as_sf st_crs st_transform st_intersection st_geometry_type
#' @importFrom dplyr select all_of filter
#' @importFrom stars st_extract
#' @importFrom dismo bioclim predict
#' @importFrom cli cli_abort cli_warn
#' @importFrom caret train
#' @importFrom stats pchisq cov mahalanobis
#'
#' @export
pseudoabsences <- function(occ, pred = NULL, method = "random", n_set = 10, n_pa = NULL, variables_selected = NULL, th = 0) {
  assert_class_cli(occ, "input_sdm")
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    pred <- occ$predictors
  }
  assert_class_cli(pred, "sdm_area")
  assert_choice_cli(method, c("random", "bioclim", "mahal.dist"))
  assert_int_cli(n_set)
  assert_int_cli(n_pa, null.ok = TRUE)
  if(length(n_pa)!=1){assert_numeric_cli(n_pa, len = length(species_names(y)), null.ok=T)}
  assert_numeric_cli(th, len=1, null.ok=FALSE, upper=1, lower=0, any.missing=FALSE)
  assert_subset_cli(variables_selected, c(get_predictor_names(pred), "vif", "pca"), empty.ok = TRUE)

  if (!is.null(y$pseudoabsences)) {
    cli::cli_warn("Previous pseudoabsence element on Occurrences object was overwrited.", call. = FALSE)
  }
  if (is.null(n_pa)) {
    n_pa <- y$n_presences
  } else if(is.null(names(n_pa))){
      cli::cli_warn(c("{.var n_pa} has no names.",
                    "i" = "Trying to set n_pa names to: {species_names(y)}"))
    if(length(n_pa) == length(species_names(y))) {
      names(n_pa) <- species_names(y)
    } else {
      if(length(n_pa)==1) {
        cli::cli_warn(c("Length of {.var n_pa} is 1.",
                        "i" = "Setting all species to have {n_pa} pseudoabsences."))
        n_pa <- rep(n_pa, length(species_names(y)))
        names(n_pa) <- species_names(y)
      } else {
        cli::cli_abort(c("{.var n_pa} could not be addressed to species.",
                         "i" = "Provide a named numeric vector with species names and number of pseudoabsences"))
      }
    }
  }

  if (is_sdm_area(pred)) {
    if (is.null(variables_selected)) {
      selected_vars <- get_predictor_names(pred)
    } else if (any(variables_selected %in% get_predictor_names(pred))) {
      p_names <- get_predictor_names(pred)
      selected_vars <- p_names[p_names %in% variables_selected]
    } else if (variables_selected == "vif"){
      selected_vars <- pred$variable_selection$vif$selected_variables
    } else if (variables_selected == "pca"){
      selected_vars <- pred$variable_selection$pca$selected_variables
    }
  }

  if (is_sdm_area(pred)) {
    df <- pred$grid |>
      dplyr::select(dplyr::all_of(c("cell_id", selected_vars)))
  }

  if (method == "random") {
    l <- sapply(y$spp_names, function(sp) {
      l <- list()
      for (j in 1:n_set) {
        if (n_pa[sp] < nrow(df)) {
          samp <- sample(df$cell_id, n_pa[sp])
        } else {
          samp <- sample(df$cell_id, n_pa[sp], replace = TRUE)
        }
        l[[j]] <- df[df$cell_id %in% samp, ]
      }
      return(l)
    }, simplify = FALSE, USE.NAMES = TRUE)
    pa <- .pseudoabsences(y, l, method, n_set, n_pa)
  }
  if (method == "bioclim") {
    if (is_input_sdm(occ)) {
      l <- sapply(y$spp_names, function(sp) {
        if(sf::st_crs(y$occurrences) != sf::st_crs(df)){
           sf_occ <- sf::st_transform(y$occurrences, crs = sf::st_crs(df))
        } else {
          sf_occ <- y$occurrences
        }
        if(unique(sf::st_geometry_type(df)) == "LINESTRING") {
          occ2 <- df[df$cell_id %in% sf_occ$cell_id,]
        } else {
          suppressWarnings(occ2 <- sf::st_intersection(sf_occ, df))
        }
        model <- dismo::bioclim(x = dplyr::select(as.data.frame(occ2), dplyr::all_of(selected_vars)))
        p <- dismo::predict(model, as.data.frame(df))
        p[p[] > th] <- NA
        p <- data.frame(cell_id = df$cell_id, pred = p)
        p <- p[!is.na(p$pred), ]
        l <- list()
        if (nrow(p) == 0) {
          cli::cli_abort(c("bioclim envelope for ", sp, " covered all the study area. Change th argument or change the method."))
        } else {
          for (j in 1:n_set) {
            if (n_pa[sp] < length(p$cell_id)) {
              samp <- sample(p$cell_id, n_pa[sp])
            } else {
              samp <- sample(p$cell_id, n_pa[sp], replace = TRUE)
            }
            l[[j]] <- df[df$cell_id %in% samp, ]
          }
        }

        return(l)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
    pa <- .pseudoabsences(y, l, method, n_set, n_pa)
  }
  if (method == "mahal.dist") {
    mahal.dist <- list(
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
        probs <- mahal.dist$prob(modelFit, newdata)

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
      }
    )
    if (is_input_sdm(occ)) {
      l <- sapply(y$spp_names, function(sp) {
        occ2 <- df[df$cell_id %in% y$occurrences[y$occurrences$species == sp, ]$cell_id, ]
        model <- caret::train(dplyr::select(as.data.frame(occ2), dplyr::all_of(selected_vars)),
                     rep("presence", nrow(occ2)),
                     method = mahal.dist) |>
          suppressWarnings()

        p1 <- predict(model, as.data.frame(df)) # classification in Presence and NA
        p <- data.frame(cell_id = df$cell_id, pred = p1)
        p <- p[is.na(p1), ] # Maintain only areas with NA, removing areas with presence.
        l <- list()
        if (nrow(p) == 0) {
          cli::cli_abort(c("Mahalanobis envelope for ", sp, " covered all the study area."))
        } else {
          for (j in 1:n_set) {
            if (n_pa[sp] < length(p$cell_id)) {
              samp <- sample(p$cell_id, n_pa[sp])
            } else {
              samp <- sample(p$cell_id, n_pa[sp], replace = TRUE)
            }
            l[[j]] <- df[df$cell_id %in% samp, ]
          }
        }
        return(l)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
    pa <- .pseudoabsences(y, l, method, n_set, n_pa)
  }

  if (is_input_sdm(occ)) {
    occ$occurrences <- pa
    pa <- occ
  }
  return(pa)
}

#' @rdname pseudoabsences
#' @export
n_pseudoabsences <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$n_pa)
}

#' @rdname pseudoabsences
#' @export
pseudoabsence_method <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$method)
}

#' @rdname pseudoabsences
#' @export
pseudoabsence_data <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$occurrences
  } else {
    y <- x
  }
  return(y$pseudoabsences$data)
}

.pseudoabsences <- function(occ, l, method, n_set, n_pa) {
  occ$pseudoabsences$data <- l
  occ$pseudoabsences$method <- method
  occ$pseudoabsences$n_set <- n_set
  occ$pseudoabsences$n_pa <- n_pa
  return(occ)
}
