#' Train SDM models
#'
#' This function is a wrapper to fit models in caret using caretSDM data.
#'
#' @usage
#' train_sdm(occ,
#'           pred = NULL,
#'           algo,
#'           ctrl = NULL,
#'           variables_selected = NULL)
#'
#' @param occ A \code{occurrences} or a \code{input_sdm} object.
#' @param pred A \code{predictors object}. If \code{occ} is a \code{input_sdm} object, then
#' \code{pred} is obtained from it.
#' @param algo A \code{character} vector. Algorithms to be used. For a complete list see
#' (https://topepo.github.io/caret/available-models.html). 'maxent', 'mahal.dist' and 'bioclim' are
#' also implemented, but they are only available to run alone, with no other algorithm together.
#' @param ctrl A \code{trainControl} object to be used to build models. See \code{\link{trainControl}}.
#' @param variables_selected A \code{vector} of variables to be used as predictors. If \code{NULL},
#' predictors names from \code{pred} will be used. Can also be a selection method (e.g. 'vif').
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
#'
#' # Train models:
#' i <- train_sdm(i, algo = c("nnet", "kknn"), variables_selected = "vif")
#' i
#'
#' @importFrom sf st_centroid st_as_sf st_join
#' @importFrom maxnet maxnet.default.regularization maxnet.formula maxnet
#' @importFrom dismo bioclim mahal
#' @importFrom dplyr arrange select filter all_of bind_cols summarise group_by across everything
#' @importFrom raster extract
#' @importFrom pROC roc
#' @importFrom caret train trainControl twoClassSummary
#' @importFrom stars st_extract
#'
#' @export
train_sdm <- function(occ, pred = NULL, algo, ctrl = NULL, variables_selected = NULL, parallel = FALSE, ...) {
  if (is_input_sdm(occ)) {
    z <- occ$occurrences
    pred <- occ$predictors
  } else {
    z <- occ
  }

  if (is_predictors(pred)) {
    if (is.null(variables_selected)) {
      selected_vars <- pred$predictors_names
      cat("Using all variables available: ", selected_vars)
    }
    if (any(variables_selected %in% pred$predictors_names)) {
      selected_vars <- pred$predictors_names[pred$predictors_names %in% variables_selected]
      cat("Using given variables: ", selected_vars)
    }
  } else if (is_sdm_area(pred)) {
    if (is.null(variables_selected)) {
      selected_vars <- pred$predictors
      cat("Using all variables available: ", selected_vars)
    }
    if (any(variables_selected %in% pred$predictors)) {
      selected_vars <- pred$predictors[pred$predictors %in% variables_selected]
      cat("Using given variables: ", selected_vars)
    }
  }

  if (length(variables_selected) == 1) {
    if (length(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected]) == 0) {
      print(paste0("Variable selection method not detected."))
      stop()
    }
    selected_vars <- unlist(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected], rec = F)[[paste0(variables_selected, ".selected_variables")]]
    print(cat("Using variables selected by ", variables_selected, ": ", selected_vars))
  }
  if (is.null(ctrl)) {
    ctrl <- caret::trainControl(
      method = "repeatedcv", number = 4, repeats = 1, classProbs = TRUE, returnResamp = "all", # retornar folds
      summaryFunction = caret::twoClassSummary, savePredictions = "all", allowParallel = FALSE
    )
  }
  algo2 <- algo
  if (length(algo2) == 1) {
    if ("maxent" %in% algo2) {
      algo <- list(
        label = "Maximum Entropy Modeling",
        library = "maxnet",
        loop = NULL,
        type = c("Classification", "Regression"),
        levels = c("presence", "pseudoabsence"),
        parameters = data.frame(
          parameter = c("regmult"),
          class = c("numeric"),
          label = c("Regularization Multiplier")
        ),
        grid = function(x, y, len = NULL, search = "grid") {
          if (search == "grid") {
            out <- expand.grid(regmult = 1:len)
          } else {
            out <- expand.grid(regmult = 1:len)
          }
          out
        },
        fit = function(x, y, wts, param, lev, last, classProbs, ...) {
          model <- maxnet::maxnet(
            p = ifelse(as.numeric(y) == 1, 1, 0), data = x,
            f = maxnet::maxnet.formula(p = ifelse(as.numeric(y) == 1, 1, 0), data = x),
            regmult = param$regmult,
            regfun = maxnet::maxnet.default.regularization, addsamplestobackground = T, ...
          )
          return(model)
        },
        predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
          pred_maxnet <- function(object, newdata, clamp = T, type = c("logistic"), ...) {
            if (clamp) {
              for (v in intersect(names(object$varmax), names(newdata))) {
                newdata[, v] <- pmin(
                  pmax(newdata[, v], object$varmin[v]),
                  object$varmax[v]
                )
              }
            }
            hingeval <- function(x, min, max) {
              pmin(1, pmax(0, (x - min) / (max - min)))
            }
            terms <- sub(
              "hinge\\((.*)\\):(.*):(.*)$", "hingeval(\\1,\\2,\\3)",
              names(object$betas)
            )
            terms <- sub(
              "categorical\\((.*)\\):(.*)$", "categoricalval(\\1,\"\\2\")",
              terms
            )
            terms <- sub(
              "thresholds\\((.*)\\):(.*)$", "thresholdval(\\1,\\2)",
              terms
            )
            f <- formula(paste("~", paste(terms, collapse = " + "), "-1"))
            mm <- model.matrix(f, data.frame(newdata))
            if (clamp) {
              mm <- t(pmin(
                pmax(t(mm), object$featuremins[names(object$betas)]),
                object$featuremaxs[names(object$betas)]
              ))
            }
            link <- (mm %*% object$betas) + object$alpha
            if (type == "logistic") {
              res <- 1 / (1 + exp(-object$entropy - link))
            }
            return(res)
          }
          pred <- pred_maxnet(modelFit, newdata, clamp = T, type = c("logistic"))
          pred <- data.frame(presence = pred, pseudoabsence = 1 - pred)
          pred <- as.factor(colnames(pred)[apply(pred, 1, which.max)])
          return(pred)
        },
        prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
          prob_maxnet <- function(object, newdata, clamp = T, type = c("cloglog"), ...) {
            if (clamp) {
              for (v in intersect(names(object$varmax), names(newdata))) {
                newdata[, v] <- pmin(
                  pmax(newdata[, v], object$varmin[v]),
                  object$varmax[v]
                )
              }
            }
            hingeval <- function(x, min, max) {
              pmin(1, pmax(0, (x - min) / (max - min)))
            }
            terms <- sub(
              "hinge\\((.*)\\):(.*):(.*)$", "hingeval(\\1,\\2,\\3)",
              names(object$betas)
            )
            terms <- sub(
              "categorical\\((.*)\\):(.*)$", "categoricalval(\\1,\"\\2\")",
              terms
            )
            terms <- sub(
              "thresholds\\((.*)\\):(.*)$", "thresholdval(\\1,\\2)",
              terms
            )
            f <- formula(paste("~", paste(terms, collapse = " + "), "-1"))
            mm <- model.matrix(f, data.frame(newdata))
            if (clamp) {
              mm <- t(pmin(
                pmax(t(mm), object$featuremins[names(object$betas)]),
                object$featuremaxs[names(object$betas)]
              ))
            }
            link <- (mm %*% object$betas) + object$alpha
            if (type == "cloglog") {
              res <- 1 - exp(0 - exp(object$entropy + link))
            }
            return(res)
          }
          prob <- prob_maxnet(modelFit, newdata, clamp = T, type = c("cloglog"))
          prob <- data.frame(presence = prob, pseudoabsence = 1 - prob)
          return(prob)
        },
        predictors = function(x, ...) {
          colnames(x)
        },
        varImp = NULL,
        tags = c("MaxEnt")
      )
    } else if ("bioclim" %in% algo2) {
      algo <- list(
        label = "Bioclimatic Envelope Model",
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
          model <- dismo::bioclim(x = x[y == "presence", ])
          result <- list(model = model, abs = param$abs)
          return(result)
        },
        predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
          pred <- predict(modelFit$model, newdata) ## should be dismo::predict?
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
        tags = c("Bioclim")
      )
    } else if ("mahal.dist" %in% algo2) {
      algo <- list(
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
    }
  }

  l <- list()
  if ("independent_test" %in% names(z)) {
    indep_val <- list()
    it <- z$independent_test
    it <- stars::st_extract(pred$grid[[selected_vars]], it)
  }

  # occ2 <- z$occurrences
  # col_names <- find_columns(occ2)
  # sp::coordinates(occ2) <- col_names[c(2,3)]
  # occ2 <- extract(pred$grid[[selected_vars]], occ2)

  ### parallel == TRUE
  # if(parallel){
  #  l <- mclapply(z$pseudoabsences$data, function(pa){
  #    pa <- pa[,match(colnames(occ2), colnames(pa))]
  #    x <- rbind(occ2,pa)
  #    df <- as.factor(c(rep('presence', nrow(occ2)),rep('pseudoabsence',nrow(pa))))
  #
  #    if(class(algo)=='list' & !'fit' %in% names(algo)){
  #      print(paste0('Stacking ensemble'))
  #      m <- list()
  #      for(j in 1:length(algo)){
  #        if(j == 1){x2 <- x}
  #        print(paste0('Layer ',j))
  #        layer1 <- lapply(algo[[j]], function(a){train(x2,
  #                                                      df,
  #                                                      method = a,
  #                                                      trControl = ctrl)})
  #        layer1_output <- lapply(layer1, function(l1){predict(l1, newdata=x2, type='prob')$presence})
  #        layer1_output <- as.data.frame(dplyr::bind_cols(layer1_output))
  #        #layer1_output <- as.data.frame(lapply(layer1, function(z){z$pred$presence}))
  #        colnames(layer1_output) <- algo[[j]]
  #        x2 <- layer1_output
  #        m[[j]] <- layer1
  #        names(m[[j]]) <- algo[[j]]
  #      }
  #      names(m) <- paste0('layer_',1:length(m))
  #    } else {
  #      m <- lapply(algo, function(a){train(x,
  #                                          df,
  #                                          method = a,
  #                                          trControl = ctrl) # lapply retorna diferentes valores de tuning (padronizar com seed?)
  #      })
  #    }
  #
  #    if('independent_test' %in% names(y)){
  #      iv <- lapply(m, function(m2){
  #        r <- rep(1, nrow(it))
  #        p <- predict(m2, newdata=it, type='prob')
  #        iv2 <- pROC::roc(r, p$presence)
  #        iv2 <- as.numeric(iv2$auc)
  #      })
  #      indep_val[[paste0("m",i)]] <- iv
  #    }
  #
  #    l[[paste0("m",i)]] <- m
  #    return(l)
  #  }, mc.cores=6)
  # } else {

  l <- sapply(z$spp_names, function(sp) {
    #if (is_predictors(pred)) {
    #  occ2 <- z$occurrences[z$occurrences$species == sp, ]$cell_id
    #  suppressWarnings(env <- dplyr::select(cbind(sf::st_centroid(sf::st_as_sf(pred$data)), pred$grid), -"geometry.1"))
    #  occ2 <- dplyr::filter(env, env$cell_id %in% occ2)
    #  occ2 <- dplyr::select(occ2, dplyr::all_of(selected_vars))
    #} else
    if (is_sdm_area(pred)) {
      occ2 <- z$occurrences |>
        sf::st_join(pred$grid) |>
        dplyr::filter(species == sp) |>
        dplyr::select(dplyr::all_of(selected_vars))
      ## Talvez alguma coisa com st_intersect?
      if(sf::st_crs(z$occurrences) != sf::st_crs(pred$grid)){
          occ2 <- sf::st_transform(z$occurrences, crs = sf::st_crs(pred$grid))
      }
      suppressWarnings(occ2 <- st_intersection(occ2, pred$grid))
    }

    for (i in 1:length(z$pseudoabsences$data[[sp]])) {
      pa <- z$pseudoabsences$data[[sp]][[i]]
      pa <- pa[, names(occ2)[match(names(pa), names(occ2))]]
      occ2 <- occ2[, names(occ2)[match(names(pa), names(occ2))]]
      x <- rbind(occ2, pa)
      x <- dplyr::select(as.data.frame(x), dplyr::all_of(selected_vars))
      df <- as.factor(c(rep("presence", nrow(occ2)), rep("pseudoabsence", nrow(pa))))

      if (class(algo) == "list" & !"fit" %in% names(algo)) {
        print(paste0("Stacking ensemble"))
        m <- list()
        for (j in 1:length(algo)) {
          if (j == 1) {
            x2 <- x
          }
          print(paste0("Layer ", j))
          layer1 <- lapply(algo[[j]], function(a) {
            caret::train(x2,
              df,
              method = a,
              trControl = ctrl,
              ...
            )
          })
          layer1_output <- lapply(layer1, function(l1) {
            predict(l1, newdata = x2, type = "prob")$presence
          })
          layer1_output <- as.data.frame(dplyr::bind_cols(layer1_output))
          # layer1_output <- as.data.frame(lapply(layer1, function(z){z$pred$presence}))
          colnames(layer1_output) <- algo[[j]]
          x2 <- layer1_output
          m[[j]] <- layer1
          names(m[[j]]) <- algo[[j]]
        }
        names(m) <- paste0("layer_", 1:length(m))
      } else {
        if (class(algo) == "list" & "fit" %in% names(algo)) {
          m <- caret::train(x,
            df,
            method = algo,
            trControl = ctrl
          )
          m$method <- algo2
          m <- list(m)
        } else {
          m <- lapply(algo, function(a) {
            caret::train(x, # usar sapply para que o id seja mais organizado
              df,
              method = a,
              trControl = ctrl
            ) # lapply retorna diferentes valores de tuning (padronizar com seed?)
          })
        }
      }

      if ("independent_test" %in% names(z)) {
        iv <- lapply(m, function(m2) {
          r <- rep(1, nrow(it))
          p <- predict(m2, newdata = it, type = "prob")
          iv2 <- pROC::roc(r, p$presence)
          iv2 <- as.numeric(iv2$auc)
        })
        indep_val[[paste0("m", i)]] <- iv
      }

      l[[paste0("m", i, ".")]] <- m
      #  }
    }
    return(l)
  }, simplify = TRUE, USE.NAMES = TRUE)

  ##################################

  if (class(algo) == "list" & !"fit" %in% names(algo)) {
    n <- length(algo)
    n <- paste0("layer_", n)
    m <- unlist(lapply(l, function(x) x[[n]]), recursive = F)
  } else {
    m <- apply(l, 2, function(x) {
      unlist(x, recursive = F)
    })
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

  if ("independent_test" %in% names(z)) {
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

#' @export
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

#' Print method for predictors
#' @exportS3Method base::print
print.models <- function(x) {
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
}
