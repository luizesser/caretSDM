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
#' @param occ A \code{occurrences} or \code{input_sdm} object.
#' @param pred A \code{predictors} object. If \code{NULL} and \code{occ} is a \code{input_sdm},
#' \code{pred} will be retrieved from \code{occ}.
#' @param method Method to create pseudoabsences. One of: "random", "bioclim" or "mahal.dist".
#' @param n_set \code{numeric}. Number of datasets of pseudoabsence to create.
#' @param n_pa \code{numeric}. Number of pseudoabsences to be generated in each dataset created.
#' If \code{NULL} then the function prevents imbalance by using the same number of presence records.
#' @param variables_selected A vector with variables names to be used while building pseudoabsences.
#' Only used when method is not "random".
#' @param th \code{numeric} Threshold to be applied in bioclim/mahal.dist projections. See details.
#' @param i A \code{input_sdm} object.
#'
#' @returns A \code{occurrences} or \code{input_sdm} object with pseudoabsence data.
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
#' @seealso \code{link{input_sdm} \link{sdm_area} \link{occurrences} \link{predictors}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, epsg = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc)
#'
#' # Create input_sdm:
#' i <- input_sdm(occurrences(occ), sa)
#'
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' # VIF calculation:
#' i <- vif_predictors(i)
#'
#' # Pseudoabsence generation:
#' i <- pseudoabsence(i, method="bioclim", variables_selected = "vif")
#' i
#'
#' @import sdmppa
#' @importFrom sf st_centroid st_as_sf st_crs st_transform st_intersection
#' @importFrom dplyr select all_of filter
#' @importFrom stars st_extract
#' @importFrom dismo bioclim predict
#'
#' @export
pseudoabsences <- function(occ, pred = NULL, method = "random", n_set = 10, n_pa = NULL, variables_selected = NULL, th = 0) {
  if (is_input_sdm(occ)) {
    y <- occ$occurrences
    pred <- occ$predictors
  } else {
    y <- occ
  }

  if (!is.null(occ$pseudoabsences)) {
    warning("Previous pseudoabsence element on Occurrences object was overwrited.", call. = F)
  }
  if (is.null(n_pa)) {
    n_pa <- y$n_presences
  }
  #if (is_predictors(pred)) {
  #  if (is.null(variables_selected)) {
  #    selected_vars <- pred$predictors_names
  #    cat("Using all variables available: ", selected_vars)
  #  }
  #  if (any(variables_selected %in% pred$predictors_names)) {
  #    selected_vars <- pred$predictors_names[pred$predictors_names %in% variables_selected]
  #    cat("Using given variables: ", selected_vars)
  #  }
  #} else
  if (is_sdm_area(pred)) {
    if (is.null(variables_selected)) {
      selected_vars <- get_predictor_names(pred)
      cat("Using all variables available: ", selected_vars)
    } else if (any(variables_selected %in% get_predictor_names(pred))) {
      p_names <- get_predictor_names(pred)
      selected_vars <- p_names[p_names %in% variables_selected]
      cat("Using given variables: ", selected_vars)
    } else if (variables_selected == "vif"){
      selected_vars <- pred$variable_selection$vif$selected_variables
    } else if (variables_selected == "pca"){
      selected_vars <- pred$variable_selection$pca$selected_variables
    }
  }

  #if (length(variables_selected) == 1) {
  #  if (length(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected]) == 0) {
  #    print(paste0("Variable selection method not detected."))
  #    stop()
  #  }
  #  selected_vars <- unlist(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected], rec = F)[[paste0(variables_selected, ".selected_variables")]]
  #  cat("Using variables selected by ", variables_selected, ": ", selected_vars)
  #}
  #if (is_predictors(pred)) {
  #  suppressWarnings(df <- sf::st_centroid(sf::st_as_sf(dplyr::filter(na.omit(pred$data), band %in% selected_vars))))
  #  df <- dplyr::select(cbind(pred$grid, df), -"geometry.1")
  #} else
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
          samp <- sample(df$cell_id, n_pa[sp], replace = T)
        }
        l[[j]] <- df[df$cell_id %in% samp, ]
      }
      return(l)
    }, simplify = FALSE, USE.NAMES = TRUE)
    pa <- .pseudoabsences(y, l, method, n_set, n_pa)
  }
  if (method == "bioclim") {
    if (class(occ) == "input_sdm") {
      l <- sapply(y$spp_names, function(sp) {
        if(sf::st_crs(y$occurrences) != sf::st_crs(df)){
           sf_occ <- sf::st_transform(y$occurrences, crs = sf::st_crs(df))
        } else {
          sf_occ <- y$occurrences
        }
        occ2 <- sf::st_intersection(sf_occ, df)
        model <- dismo::bioclim(x = dplyr::select(as.data.frame(occ2), dplyr::all_of(selected_vars)))
        p <- dismo::predict(model, as.data.frame(df))
        p[p[] > th] <- NA
        p <- data.frame(cell_id = df$cell_id, pred = p)
        p <- p[!is.na(p$pred), ]
        l <- list()
        if (nrow(p) == 0) {
          print(paste0("bioclim envelope for ", sp, " covered all the study area. Change th argument or change the method."))
          stop()
        } else {
          for (j in 1:n_set) {
            if (n_pa[sp] < length(p$cell_id)) {
              samp <- sample(p$cell_id, n_pa[sp])
            } else {
              samp <- sample(p$cell_id, n_pa[sp], replace = T)
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
    if (class(occ) == "input_sdm") {
      l <- sapply(y$spp_names, function(sp) {
        occ2 <- df[df$cell_id %in% y$occurrences[y$occurrences$species == sp, ]$cell_id, ]
        model <- dismo::mahal(x = select(as.data.frame(occ2), all_of(selected_vars)))
        p <- predict(model, as.data.frame(df))
        p[p[] < th] <- NA # A value of 1 means that the lower distance we are considering is 1 standard deviation from the mean in each dimention.
        p <- data.frame(cell_id = df$cell_id, pred = p)
        p <- p[!is.na(p$pred), ]
        l <- list()
        if (nrow(p) == 0) {
          print(paste0("bioclim envelope for ", sp, " covered all the study area. Change th argument or change the method."))
          stop()
        } else {
          for (j in 1:n_set) {
            if (n_pa[sp] < length(p$cell_id)) {
              samp <- sample(p$cell_id, n_pa[sp])
            } else {
              samp <- sample(p$cell_id, n_pa[sp], replace = T)
            }
            l[[j]] <- df[df$cell_id %in% samp, ]
          }
        }
        return(l)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }
    pa <- .pseudoabsences(y, l, method, n_set, n_pa)
  }
  if (method == "cluster") {
    # Reginaldo
  }

  if (class(occ) == "input_sdm") {
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

#' @export
.pseudoabsences <- function(occ, l, method, n_set, n_pa) {
  occ$pseudoabsences$data <- l
  occ$pseudoabsences$method <- method
  occ$pseudoabsences$n_set <- n_set
  occ$pseudoabsences$n_pa <- n_pa
  return(occ)
}
