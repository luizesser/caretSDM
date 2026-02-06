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
#' \code{?caret::trainControl} and details.
#' @param variables_selected A \code{vector} of variables to be used as predictors. If \code{NULL},
#' predictors names from \code{pred} will be used. Can also be a selection method (e.g. 'vif').
#' @param ... Additional arguments to be passed to \code{caret::train} function.
#' @param parallel Should a paralelization method be used (not yet implemented)?
#' @param i A \code{models} or a \code{input_sdm} object.
#' @param m1 A \code{models} object.
#' @param m2 A \code{models} object.
#'
#'
#' @return A \code{models} or a \code{input_sdm} object.
#'
#' @details
#' The object \code{\link{algorithms}} has a table comparing algorithms available. If the function
#' detects that the necessary packages are not available it will ask for installation. This will
#' happen just in the first time you use the algorithm.
#' \code{caret::trainControl} holds multiple resources for validation and model tuning. Make sure
#' to understand its parameters beforehand. As it is a key function in the modeling process, we also
#' implemented spatial crossvalidation on it. You can set \code{methods} to be \code{cv_spatial} or
#' \code{cv_cluster} and \code{train_sdm} will detect that and apply the method according to
#' \code{blockCV} package.
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
#' i <- pseudoabsences(i, method = "random")
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
#' i <- train_sdm(i, algo = c("naive_bayes"), ctrl = ctrl_sdm) |>
#' suppressWarnings()
#'
#' @importFrom sf st_centroid st_as_sf st_join st_intersection st_geometry_type
#' @importFrom dplyr arrange select filter all_of bind_cols summarise group_by across everything mutate
#' @importFrom raster extract
#' @importFrom pROC roc
#' @importFrom caret train trainControl
#' @importFrom stars st_extract
#' @importFrom utils combn
#' @importFrom blockCV cv_spatial cv_cluster
#' @importFrom cli cli_abort
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
  assert_subset_cli(class(algo), c("list", "character"), empty.ok = FALSE)

  if (!is.null(ctrl)) {
    assert_list_cli(ctrl, len=27)
    assert_names_cli(names(ctrl),
                     must.include = c("method", "number", "repeats", "search", "p", "initialWindow",
                                      "horizon", "fixedWindow", "skip", "verboseIter", "returnData",
                                      "returnResamp", "savePredictions", "classProbs",
                                      "summaryFunction", "selectionFunction", "preProcOptions",
                                      "sampling", "index", "indexOut", "indexFinal", "timingSamps",
                                      "predictionBounds", "seeds", "adaptive", "trim", "allowParallel"))
  } else {
    ctrl <- caret::trainControl(
      method = "repeatedcv", number = 4, repeats = 1, classProbs = TRUE, returnResamp = "all",
      summaryFunction = summary_sdm, savePredictions = "all", allowParallel = FALSE)
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

  algo2 <- algo
  custom_algo <- c("library", "type", "parameters", "grid",
                   "fit", "predict", "prob")
  if(is.list(algo) && all(custom_algo %in% names(algo))) {
    algo2 <- deparse(substitute(algo))
  }

  if("maxent" %in% algo && !"background" %in% names(z)) {
    cli::cli_abort(c(
      "{.var algo} contains maxent, but no background data is provided.",
      "i" = "Perhaps you have confused the concepts of pseudoabsence and background data."
    ))
  }

  l <- sapply(z$spp_names, function(sp) {
    l1 <- list()
    l2 <- list()

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

    if ("pseudoabsences" %in% names(z)) {
      for (j in 1:length(z$pseudoabsences$data[[sp]])) {
        pa <- z$pseudoabsences$data[[sp]][[j]]
        pa <- sf::st_centroid(pa[, names(occ2)[match(names(pa), names(occ2))]]) |>
          suppressWarnings()
        occ2 <- occ2[, names(occ2)[match(names(pa), names(occ2))]]
        x <- rbind(occ2, pa)

        if ("cv_spatial" %in% ctrl$method) {
          test <- x |> dplyr::mutate(presence = c(rep(1, nrow(occ2)), rep(0, nrow(pa))))
          spatial_blocks <- blockCV::cv_spatial(
            x = test,
            column = "presence",
            r = sdm_as_terra(occ, what = "predictors")[[-1]], # removes cell_id
            k = ctrl$number,
            biomod2 = FALSE,
            progress = FALSE
          )
          ctrl$method <- "cv"
          ctrl$number <- NA
          ctrl$index <- lapply(spatial_blocks$folds_list, function(x) x[[1]])
          ctrl$indexOut <- lapply(spatial_blocks$folds_list, function(x) x[[2]])
        }
        if ("cv_cluster" %in% ctrl$method) {
          test <- x |> dplyr::mutate(presence = c(rep(1, nrow(occ2)), rep(0, nrow(pa))))
          env_blocks <- blockCV::cv_cluster(
            test,
            column = "presence",
            r = sdm_as_terra(occ, what = "predictors")[[-1]], # removes cell_id
            k = ctrl$number,
            biomod2 = FALSE,
            report = FALSE
          )
          ctrl$method <- "cv"
          ctrl$number <- NA
          ctrl$index <- lapply(env_blocks$folds_list, function(x) x[[1]])
          ctrl$indexOut <- lapply(env_blocks$folds_list, function(x) x[[2]])
        }

        x <- dplyr::select(as.data.frame(x), dplyr::all_of(selected_vars))
        df <- as.factor(c(rep("presence", nrow(occ2)), rep("pseudoabsence", nrow(pa))))

        # TRAIN MODELS ##################
        if (sp %in% z$esm$spp) {
          cli::cli_progress_message("ESM species")
          m1 <- list()
          vars_comb <- colnames(x) |> utils::combn(2)
          if(is.character(algo)) {
            algo_pa <- algo[!algo %in% c("maxent")]
            for (vars in 1:ncol(vars_comb)) {
              m1[[vars]] <- lapply(algo_pa, function(a) {
                if (a == "mahal.dist") { a <- .mahal.dist }
                caret::train(
                  df~.,
                  data = cbind(df,x[,vars_comb[,vars]]),
                  method = a,
                  trControl = ctrl,
                  ...
                )
              })
            }
            m <- unlist(m1, recursive = FALSE)
          } else if (is.list(algo)) {
            for (vars in 1:ncol(vars_comb)) {
              m1[[vars]] <- caret::train(
                df~.,
                data = cbind(df,x),
                method = algo,
                trControl = ctrl,
                ...
              ) |> list()
            }
            m <- unlist(m1, recursive = FALSE)
          }
        } else if(is.character(algo)) {
          algo_pa <- algo[!algo %in% c("maxent")]
          m <- lapply(algo_pa, function(a) {
            if (a == "mahal.dist") { a <- .mahal.dist }
            caret::train(
              df~.,
              data = cbind(df,x),
              method = a,
              trControl = ctrl,
              ...
            ) # lapply retorna diferentes valores de tuning (padronizar com seed?)
          })
        } else if (is.list(algo)) {
          m <- caret::train(
            df~.,
            data = cbind(df,x),
            method = algo,
            trControl = ctrl,
            ...
          ) |> list()
        }
        l1[[paste0("m", j, ".")]] <- m
      }
    }

    if ("background" %in% names(z)) {
      for (j in 1:length(z$background$data[[sp]])) {
        pa <- z$background$data[[sp]][[j]]
        pa <- sf::st_centroid(pa[, names(occ2)[match(names(pa), names(occ2))]]) |>
          suppressWarnings()
        occ2 <- occ2[, names(occ2)[match(names(pa), names(occ2))]]
        x <- rbind(occ2, pa)

        if ("cv_spatial" %in% ctrl$method) {
          test <- x |> dplyr::mutate(presence = c(rep(1, nrow(occ2)), rep(0, nrow(pa))))
          spatial_blocks <- blockCV::cv_spatial(
            x = test,
            column = "presence",
            r = sdm_as_terra(occ, what = "predictors")[[-1]], # removes cell_id
            k = ctrl$number,
            biomod2 = FALSE,
            progress = FALSE
          )
          ctrl$method <- "cv"
          ctrl$number <- NA
          ctrl$index <- lapply(spatial_blocks$folds_list, function(x) x[[1]])
          ctrl$indexOut <- lapply(spatial_blocks$folds_list, function(x) x[[2]])
        }
        if ("cv_cluster" %in% ctrl$method) {
          test <- x |> dplyr::mutate(presence = c(rep(1, nrow(occ2)), rep(0, nrow(pa))))
          env_blocks <- blockCV::cv_cluster(
            test,
            column = "presence",
            r = sdm_as_terra(occ, what = "predictors")[[-1]], # removes cell_id
            k = ctrl$number,
            biomod2 = FALSE,
            report = FALSE
          )
          ctrl$method <- "cv"
          ctrl$number <- NA
          ctrl$index <- lapply(env_blocks$folds_list, function(x) x[[1]])
          ctrl$indexOut <- lapply(env_blocks$folds_list, function(x) x[[2]])
        }

        x <- dplyr::select(as.data.frame(x), dplyr::all_of(selected_vars))
        df <- as.factor(c(rep("presence", nrow(occ2)), rep("pseudoabsence", nrow(pa))))

        # TRAIN MODELS ##################
        if (sp %in% z$esm$spp) {
          cli::cli_progress_message("ESM species")
          m1 <- list()
          vars_comb <- colnames(x) |> utils::combn(2)
          if(is.character(algo)) {
            for (vars in 1:ncol(vars_comb)) {
              algo_bg <- algo[algo %in% c("maxent", "mahal.dist")]
              m1[[vars]] <- lapply(algo_bg, function(a) {
                if (a == "mahal.dist") { a <- .mahal.dist } else
                  if (a == "maxent") { a <- .maxent }
                caret::train(
                  df~.,
                  data = cbind(df,x[,vars_comb[,vars]]),
                  method = a,
                  trControl = ctrl,
                  ...
                )
              })
            }
            m <- unlist(m1, recursive = FALSE)
          } else if (is.list(algo)) {
            for (vars in 1:ncol(vars_comb)) {
              m1[[vars]] <- caret::train(
                df~.,
                data = cbind(df,x),
                method = algo,
                trControl = ctrl,
                ...
              ) |> list()
            }
            m <- unlist(m1, recursive = FALSE)
          }
        } else if(is.character(algo)) {
          algo_bg <- algo[algo %in% c("maxent", "mahal.dist")]
          m <- lapply(algo_bg, function(a) {
            if (a == "mahal.dist") { a <- .mahal.dist } else
              if (a == "maxent") { a <- .maxent } ###########################################
            caret::train(
              df~.,
              data = cbind(df,x),
              method = a,
              trControl = ctrl,
              ...
            ) # lapply retorna diferentes valores de tuning (padronizar com seed?)
          })
        } else if (is.list(algo)) {
          m <- caret::train(
            df~.,
            data = cbind(df,x),
            method = algo,
            trControl = ctrl,
            ...
          ) |> list()
        }
        l2[[paste0("m", j, ".")]] <- m
      }
    }
    l <- append(l1, l2)
    names(l) <- paste0("m", 1:length(l), ".")
    return(l)
  }, simplify = TRUE, USE.NAMES = TRUE)

  #################################################################################################
  if (!is.matrix(l)) {
    l0 <- t(as.matrix(l))
    colnames(l0) <- species_names(z)
    rownames(l0) <- "m1."
    l <- l0
  }

  m <- apply(l, 2, function(x) {
    unlist(x, recursive = FALSE)
  })

  if(length(algo2) == 1) {
    for (j in 1:length(m)) {
      names(m[[j]]) <- paste0(names(m[[j]]), 1)
    }
  }

  metrics <- sapply(z$spp_names, function(sp) {
    metrics <- lapply(m[[sp]], function(x) {
      if(x$method == "custom") {
        if("tags" %in% names(x$modelInfo)){
          if(x$modelInfo$tags[1] == "maxent") {
            x$method <- "maxent"
          } else
          if(x$modelInfo$tags[1] == "mahalanobis") {
            x$method <- "mahal.dist"
          }
        } else {
          x$method <- algo2
        }
      }
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

  if ("independent_test" %in% names(z)) {
    it <- sf::st_join(z$independent_test, dplyr::select(pred$grid, -"cell_id"))
    indep_val <- sapply(z$spp_names, function(sp) {
      it_sp <- as.data.frame(it[it$species==sp,])
      it_sp$presence <- factor(rep("presence", nrow(it_sp)))
      levels(it_sp$presence) <- c("presence", "pseudoabsence")
      independent_metrics <- sapply(m[[sp]], function(x) {
          res <- validate_on_independent_data(x,
                                              data_independent = it_sp,
                                              obs_col_name = "presence")
          return(res)
      }, simplify = FALSE, USE.NAMES = TRUE)
      res_mean <- do.call(rbind, independent_metrics) |>
        colMeans()
      res_sd <- apply(do.call(rbind, independent_metrics), 2, sd)
      df <- data.frame(mean = res_mean, sd = res_sd)
      return(df)
    }, simplify = FALSE, USE.NAMES = TRUE)

    m2$independent_validation <- indep_val
  }

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

#' @rdname train_sdm
#' @export
add_models <- function(m1, m2) {
  assert_class_cli(m1, "models", null.ok = TRUE)
  assert_class_cli(m2, "models", null.ok = TRUE)
  if(is.null(m1)) {return(m2)}
  if(is.null(m2)) {return(m1)}
  m <- list(validation = list(method = unique(c(m1$validation$method, m2$validation$method)),
                              number = unique(c(m1$validation$number, m2$validation$number)),
                              metrics = c(m1$validation$metrics, m2$validation$metrics)
  ),
  predictors = unique(c(m1$predictors, m2$predictors)),
  algorithms = unique(c(m1$algorithms, m2$algorithms)),
  models = c(m1$models, m2$models),
  tuning = unique(c(m1$tuning, m2$tuning))
  )
  msum <- .models(m)
  return(msum)
}

.models <- function(x) {
  if ("independent_validation" %in% names(x)) {
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
#' @returns Concatenate structured characters to showcase what is stored in the object.
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
