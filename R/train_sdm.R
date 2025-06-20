#' Train SDM models
#'
#' This function is a wrapper to fit models in caret using caretSDM data.
#'
#' @usage
#' train_sdm(occ,
#'           pred = NULL,
#'           algo,
#'           ctrl = NULL,
#'           variables_selected = NULL,
#'           parallel = FALSE,
#'           ...)
#'
#' @param occ A \code{occurrences} or a \code{input_sdm} object.
#' @param pred A \code{predictors object}. If \code{occ} is a \code{input_sdm} object, then
#' \code{pred} is obtained from it.
#' @param algo A \code{character} vector. Algorithms to be used. For a complete list see
#' (https://topepo.github.io/caret/available-models.html) or in caretSDM::algorithms.
#' @param ctrl A \code{trainControl} object to be used to build models. See
#' \code{?caret::trainControl}.
#' @param variables_selected A \code{vector} of variables to be used as predictors. If \code{NULL},
#' predictors names from \code{pred} will be used. Can also be a selection method (e.g. 'vif').
#' @param ... Additional arguments to be passed to \code{caret::train} function.
#' @param parallel Should a paralelization method be used (not yet implemented)?
#' @param i A \code{models} or a \code{input_sdm} object.
#'
#' @return A \code{models} or a \code{input_sdm} object.
#'
#' @details
#' The object \code{\link{algorithms}} has a table comparing algorithms available. If the function
#' detects that the necessary packages are not available it will ask for installation. This will
#' happen just in the first time you use the algorithm.
#'
#' \code{get_tune_length} return the length used in grid-search for tunning.
#'
#' \code{algorithms_used} return the names of the algorithms used in the modeling process.
#'
#' \code{get_models} returns a \code{list} with trained models (class \code{train}) to each species.
#'
#' \code{get_validation_metrics} return a \code{list} with a \code{data.frame} to each species
#' with complete values for ROC, Sensitivity, Specificity, with their respectives Standard
#' Deviations (SD) and TSS to each of the algorithms and pseudoabsence datasets used.
#'
#' \code{mean_validation_metrics} return a \code{list} with a \code{tibble} to each species
#' summarizing values for ROC, Sensitivity, Specificity and TSS to each of the algorithms used.
#'
#' @seealso \code{\link{input_sdm} \link{sdm_area} \link{algorithms}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
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
#' # Custom trainControl:
#' ctrl_sdm <- caret::trainControl(method = "repeatedcv",
#'                                 number = 2,
#'                                 repeats = 1,
#'                                 classProbs = TRUE,
#'                                 returnResamp = "all",
#'                                 summaryFunction = summary_sdm,
#'                                 savePredictions = "all")
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
#' suppressWarnings()
#'
#' @importFrom sf st_centroid st_as_sf st_join st_intersection st_geometry_type
#' @importFrom dplyr arrange select filter all_of bind_cols summarise group_by across everything
#' @importFrom raster extract
#' @importFrom pROC roc
#' @importFrom caret train trainControl
#' @importFrom stars st_extract
#'
#' @global species
#'
#' @export
train_sdm <- function(occ, pred = NULL, algo, ctrl = NULL, variables_selected = NULL, parallel = FALSE, ...) {
  assert_class_cli(occ, "input_sdm")
  if (is_input_sdm(occ)) {
    z <- occ$occurrences
    pred <- occ$predictors
  }
  assert_character_cli(algo)
  if(!is.null(ctrl)){
    assert_list_cli(ctrl, len=27)
    assert_names_cli(names(ctrl),
                     must.include = c("method", "number", "repeats", "search", "p", "initialWindow",
                                      "horizon", "fixedWindow", "skip", "verboseIter", "returnData",
                                      "returnResamp", "savePredictions", "classProbs",
                                      "summaryFunction", "selectionFunction", "preProcOptions",
                                      "sampling", "index", "indexOut", "indexFinal", "timingSamps",
                                      "predictionBounds", "seeds", "adaptive", "trim", "allowParallel"))
  }
  assert_subset_cli(variables_selected, c(get_predictor_names(pred), "vif", "pca"), empty.ok = TRUE)

  if (is_sdm_area(pred)) {
    if (is.null(variables_selected)) {
      selected_vars <- get_predictor_names(pred)
    } else if (any(variables_selected %in% get_predictor_names(pred))) {
      selected_vars <- get_predictor_names(pred)[get_predictor_names(pred) %in% variables_selected]
    } else if (variables_selected == "vif"){
      selected_vars <- pred$variable_selection$vif$selected_variables
    } else if (variables_selected == "pca"){
      selected_vars <- pred$variable_selection$pca$selected_variables
    }
  }

  if (is.null(ctrl)) {
    ctrl <- caret::trainControl(
      method = "repeatedcv", number = 4, repeats = 1, classProbs = TRUE, returnResamp = "all", # retornar folds
      summaryFunction = summary_sdm, savePredictions = "all", allowParallel = FALSE
    )
  }
  algo2 <- algo

  l <- list()
  if ("independent_test" %in% names(z)) {
    indep_val <- list()
    it <- z$independent_test
    it <- stars::st_extract(pred$grid[[selected_vars]], it)
  }

  l <- sapply(z$spp_names, function(sp) {

    if (is_sdm_area(pred)) {
      if(unique(sf::st_geometry_type(pred$grid)) == "LINESTRING") {
        df <- pred$grid |>
          dplyr::select(dplyr::all_of(c("cell_id", selected_vars)))
        sp_ids <- z$occurrences[z$occurrences$species == sp,]
        occ2 <- df[df$cell_id %in% sp_ids$cell_id,]
      } else {
        if(sf::st_crs(z$occurrences) != sf::st_crs(pred$grid)){
          occ2 <- sf::st_transform(z$occurrences, crs = sf::st_crs(pred$grid)) |>
            sf::st_join(pred$grid) |>
            dplyr::filter(species == sp) |>
            dplyr::select(dplyr::all_of(selected_vars))
        } else {
          occ2 <- z$occurrences |>
            sf::st_join(pred$grid) |>
            dplyr::filter(species == sp) |>
            dplyr::select(dplyr::all_of(selected_vars))
        }
        suppressWarnings(occ2 <- sf::st_intersection(occ2, pred$grid))
      }
    }

    for (i in 1:length(z$pseudoabsences$data[[sp]])) {
      pa <- z$pseudoabsences$data[[sp]][[i]]
      pa <- pa[, names(occ2)[match(names(pa), names(occ2))]]
      occ2 <- occ2[, names(occ2)[match(names(pa), names(occ2))]]
      x <- rbind(occ2, pa)
      x <- dplyr::select(as.data.frame(x), dplyr::all_of(selected_vars))
      df <- as.factor(c(rep("presence", nrow(occ2)), rep("pseudoabsence", nrow(pa))))
          m <- lapply(algo, function(a) {
            caret::train(
              df~.,
              data=cbind(df,x),
              method = a,
              trControl = ctrl,
              ...
            ) # lapply retorna diferentes valores de tuning (padronizar com seed?)
          })

      l[[paste0("m", i, ".")]] <- m
      #  }
    }
    return(l)
  }, simplify = TRUE, USE.NAMES = TRUE)

  m <- apply(l, 2, function(x) {
    unlist(x, recursive = FALSE)
  })

  if(length(algo) == 1) {
    for (j in 1:length(m)) {
      names(m[[j]]) <- paste0(names(m[[j]]), 1)
    }
  }

  metrics <- sapply(z$spp_names, function(sp) {
    metrics <- lapply(m[[sp]], function(x) {
      bt <- names(x$bestTune)
      res <- x$results[, !colnames(x$results) %in% bt]
      mx <- apply(res, 2, max)
      r <- cbind(data.frame(algo = x$method), t(as.data.frame(mx)))
      return(r)
    })
    metrics <- do.call(rbind, metrics)
    metrics <- dplyr::arrange(metrics, algo)
    return(metrics)
  }, simplify = FALSE, USE.NAMES = TRUE)

  m2 <- list(
    validation = list(method = ctrl$method, number = ctrl$number, metrics = metrics),
    predictors = selected_vars,
    algorithms = algo2,
    models = m
  )

  models <- .models(m2)

  if (is_input_sdm(occ)) {
    occ$models <- models
    models <- occ
  }
  return(models)
}

#' @rdname train_sdm
#' @export
get_tune_length <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  return(y$tuning)
}

#' @rdname train_sdm
#' @export
algorithms_used <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  return(y$algorithms)
}

#' @rdname train_sdm
#' @export
get_models <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  return(y$models)
}

#' @rdname train_sdm
#' @export
get_validation_metrics <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  return(y$validation$metrics)
}

#' @rdname train_sdm
#' @export
mean_validation_metrics <- function(i) {
  x=i
  if (is_input_sdm(x)) {
    y <- x$models
  } else {
    y <- x
  }
  algo <- y$algorithms
  res <- sapply(y$validation$metrics, function(met) {
    v <- dplyr::summarise(dplyr::group_by(met, algo), dplyr::across(dplyr::everything(), mean))
    return(v)
  }, simplify = FALSE, USE.NAMES = TRUE)
  return(res)
}

.models <- function(x) {
  if ("independent_test" %in% names(x)) {
    models <- structure(
      list(
        validation = list(
          method = x$validation$method,
          number = x$validation$number,
          metrics = x$validation$metrics
        ),
        independent_validation = x$independent_validation,
        predictors = x$predictors,
        algorithms = x$algorithms,
        models = x$models,
        tuning = 10
      ),
      class = "models"
    )
  } else {
    models <- structure(
      list(
        validation = list(
          method = x$validation$method,
          number = x$validation$number,
          metrics = x$validation$metrics
        ),
        predictors = x$predictors,
        algorithms = x$algorithms,
        models = x$models,
        tuning = 10
      ),
      class = "models"
    )
  }
  return(models)
}

#' Print method for models
#' @param x models object
#' @param ... passed to other methods
#' @exportS3Method base::print
print.models <- function(x, ...) {
  cat("         caretSDM        \n")
  cat(".........................\n")
  cat("Class                   : Models\n")
  cat("Algorithms Names        :", x$algorithms, "\n")
  cat("Variables Names         :", x$predictors, "\n")
  if ("independent_test" %in% names(x)) {
    cat(
      "Independent Validation  :\n",
      "        ROC: ", x$independent_validation
    )
  }
  cat(
    "Model Validation        :\n",
    "        Method          :", x$validation$method, "\n",
    "        Number          :", x$validation$number, "\n",
    "        Metrics         :\n"
  )
  print(x$validation$metrics)
  invisible(x)
}
