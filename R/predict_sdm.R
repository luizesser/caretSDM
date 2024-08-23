#' Predict SDM models in new data
#'
#' This function projects SDM models to new scenarios
#'
#' @usage
#' predict_sdm(m,
#'             scen = NULL,
#'             th = 0.9,
#'             tp = "prob",
#'             ensembles = TRUE)
#'
#' @param m A \code{input_sdm} or a \code{models} object.
#' @param scen A \code{scenarios} object or \code{NULL}. If \code{NULL} and \code{m} is a
#' \code{input_sdm} with a scenarios slot, it will be used.
#' @param th Thresholds for metrics. Can be numeric or a function.
#' @param metric A character containing the metric in which the \code{th} will be calculated/applied.
#' Default is ROC. See \code{?mean_validation_metrics} for the metrics available.
#' @param tp Type of output to be retrieved. See details.
#' @param ensembles Boolean. Should ensembles be calculated? If \code{TRUE} a series of ensembles
#' are obtained. See details.
#' @param i A \code{input_sdm} or a \code{predictions} object.
#'
#' @returns A \code{input_sdm} or a \code{predictions} object.
#'
#' @details
#' \code{tp} is a parameter to be passed on caret to retrieve either the probabilities of classes
#' (tp="prob") or the raw output (tp="raw"), which could vary depending on the algorithm used, but
#' usually would be on of the classes (factor vector with presences and pseudoabsences).
#'
#' When \code{ensembles} is set to \code{TRUE}, three ensembles are currently implemented.
#' mean_occ_prob is the mean occurrence probability, which is a simple mean of predictions,
#' wmean_AUC is the same mean_occ_prob, but weighted by AUC, and committee_avg is the committee
#' average, as known as majority rule, where predictions are binarized and then a mean is obtained.
#'
#' \code{get_predictions} returns the \code{list} of all predictions to all scenarios, all species,
#' all algorithms and all repetitions. Useful for those who wish to implement their own ensemble
#' methods.
#'
#' \code{get_ensembles} returns a \code{matrix} of \code{data.frame}s, where each column is a
#' scenario and each row is a species.
#'
#' \code{scenarios_names} returns the scenarios names in a \code{sdm_area} or \code{input_sdm}
#' object.
#'
#' \code{get_scenarios_data} returns the data from scenarios in a \code{sdm_area} or
#' \code{input_sdm} object.
#'
#' @seealso \code{\link{sdm_area} \link{input_sdm} \link{mean_validation_metrics}}
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
#'
#' # Predict models:
#' i  <- predict_sdm(i)
#' i
#'
#' @importFrom dplyr bind_cols select mutate all_of
#' @importFrom stringdist stringdist
#' @importFrom gtools mixedsort
#' @importFrom caret thresholder
#' @importFrom stars st_dimensions
#' @importFrom sf st_centroid st_as_sf st_coordinates
#'
#' @export
predict_sdm <- function(m, scen = NULL, metric = "ROC", th = 0.9, tp = "prob", file = NULL, ensembles = TRUE, add.current = TRUE) {
  if (is_input_sdm(m)) {
    scen <- m$scenarios
  }
  UseMethod("predict_sdm", scen)
}

#' @export
predict_sdm.sdm_area <- function(m, scen, metric = "ROC", th = 0.9, tp = "prob", file = NULL, ensembles = TRUE) {
  if (is_input_sdm(m)) {
    y <- m$models
    scen <- m$scenarios
  }
  assert_subset_cli(metric, colnames(select(mean_validation_metrics(m)[[1]], -algo)))
  assert_subset_cli(tp, c("prob", "raw"))

  if (is.function(th)) {
    tm <- deparse(th)
    th1 <- sapply(names(y$models), function(sp) {
      th2 <- th(y$validation$metrics[[sp]][,metric])
      res <- filter(y$validation$metrics[[sp]], .data[[metric]] > th2)
      return(res)
    }, simplify = FALSE, USE.NAMES = TRUE)
  } else {
    if (is.numeric(th)) {
      tm <- paste0("threshold: ", th)
      th1 <- sapply(names(y$models), function(sp) {
        res <- filter(y$validation$metrics[[sp]], .data[[metric]] > th)
        return(res)
      }, simplify = FALSE, USE.NAMES = TRUE)
    } else {
    assert_numeric(th, lower = 0, upper = 1, len = 1)
    }
  }

  m1 <- sapply(names(y$models), function(sp) {
    subset(y$models[[sp]], names(y$models[[sp]]) %in% rownames(th1[[sp]]))
  }, simplify = FALSE, USE.NAMES = TRUE)

  p <- list()
  for (i in 1:length(scen$data)) {
    print(paste0("Projecting: ", i, "/", length(scen$data)))
    x <- scen$data[[i]] |>
      as.data.frame() |>
      na.omit() |>
      dplyr::select(dplyr::all_of(c(m$models$predictors, "cell_id")))
    p[[i]] <- sapply(m1, function(m2) {
      p2 <- predict(m2, newdata = x, type = tp) # maybe newdata=na.omit(x), but this could cause trouble in cell_id
      cell_id <- scen$data[[i]] |>
        na.omit()
      p2 <- sapply(p2, function(z) {
        cbind(cell_id, na.omit(z))
      }, simplify = F, USE.NAMES = T)
      return(p2)
    }, simplify = F, USE.NAMES = T)
  }

  names(p) <- names(scen$data)

  if (ensembles) {
    print("Ensembling...")
    e <- sapply(names(p), function(y) {
      print(y)
      y <- p[[y]]
      e2 <- sapply(names(y), function(sp) {
        print(sp)
        x <- y[[sp]]
        if (length(x) > 0) {
          # Prepare data
          suppressMessages(df <- dplyr::bind_cols(x))
          df <- dplyr::select(df, contains("presence"))
          # mean_occ_prob
          mean_occ_prob <- df |>
            as.data.frame() |>
            apply(2, function(x) {
              as.numeric(gsub(NaN, NA, x))
            }) |>
            rowMeans()
          # mean_occ_prob <- rowMeans(df)
          # wmean_AUC
          wmean_AUC <- apply(df, 1, function(x) {
            weighted.mean(x, th1[[sp]]$ROC)
          })
          # Obtain Thresholds:
          suppressWarnings(th2 <- lapply(m1[[sp]], function(x) {
            caret::thresholder(x,
              threshold = seq(0, 1, by = 0.01),
              final = TRUE,
              statistics = "all"
            )
          }))
          th2 <- lapply(th2, function(x) {
            x <- x %>% dplyr::mutate(th = Sensitivity + Specificity)
            th <- x[x$th == max(x$th), "prob_threshold"]
            if (length(th) > 1) {
              th <- mean(th)
            }
            return(th)
          })
          # binary
          for (i in 1:ncol(df)) {
            df[, i] <- ifelse(df[, i][] > th2[i], 1, 0)
          }
          committee_avg <- rowMeans(df)

          # save everything
          df <- data.frame(cell_id = x[[1]]$cell_id, mean_occ_prob, wmean_AUC, committee_avg)
          return(df)
        } else {
          warning(paste0(sp, " has no models passing the threshold."))
          df <- NULL
        }
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, USE.NAMES = TRUE)
    if (length(names(p[[1]])) == 1 & !any(class(e) == "matrix")) {
      e <- t(as.matrix(e))
      rownames(e) <- names(p[[1]])
      colnames(e) <- gsub(paste0(".", names(p[[1]])), "", colnames(e))
    }
  }
  p2 <- list(
    thresholds = list(values = th1, method = tm, criteria = deparse(th)),
    predictions = p,
    models = m,
    file = file,
    ensembles = e,
    grid = scen$grid
  )
  predic <- .predictions(p2)
  if (is_input_sdm(m)) {
    m$predictions <- predic
    predic <- m
  }
  return(predic)
}

#
#predict_sdm.scenarios <- function(m, scen, th = 0.9, tp = "prob", file = NULL, ensembles = TRUE, add.current = TRUE) {
#  if (is_input_sdm(m)) {
#    y <- m$models
#    scen <- m$scenarios
#  }
#  if (th == "mean") {
#    tm <- paste0("threshold: mean")
#    means <- with(y$validation$metrics, by(ROC, algo, function(x) {
#      mean(x, na.rm = T)
#    }))
#    # th <- y$validation$metrics[y$validation$metrics[,'ROC']>means,]
#  } else {
#    if (is.numeric(th)) {
#      tm <- paste0("threshold: ", th)
#      th1 <- sapply(names(y$models), function(sp) {
#        y$validation$metrics[[sp]][y$validation$metrics[[sp]][, "ROC"] > th, ]
#      }, simplify = FALSE, USE.NAMES = TRUE)
#    }
#  }
#
#  m1 <- sapply(names(y$models), function(sp) {
#    subset(y$models[[sp]], names(y$models[[sp]]) %in% rownames(th1[[sp]]))
#  }, simplify = FALSE, USE.NAMES = TRUE)
#
#  if (length(m1) == 0) {
#    stop("No models passing the threshold.")
#  }
#
#  find_closest_matches <- function(inputs, valid_inputs) {
#    closest_matches <- character(length(inputs))
#    for (i in seq_along(inputs)) {
#      distances <- stringdist::stringdist(inputs[i], valid_inputs)
#      closest_index <- which.min(distances)
#      closest_matches[i] <- valid_inputs[closest_index]
#      valid_inputs <- valid_inputs[-closest_index]
#    }
#    cat(inputs, "\n")
#    cat(closest_matches)
#    return(closest_matches)
#  }
#  if (add.current == TRUE) {
#    if (is_input_sdm(m)) {
#      if (!is.null(scen)) {
#        if (is_predictors(m$predictors)) {
#          closest_match <- find_closest_matches(stars::st_dimensions(scen$data)$band$values, gtools::mixedsort(m$predictors$predictors_names))
#          stars::st_dimensions(scen$data)$band$values <- closest_match
#          scen$data[["current"]] <- m$predictors$data
#        } else if (is_sdm_area(m$predictors)) {
#          closest_match <- find_closest_matches(stars::st_dimensions(scen$data)$band$values, gtools::mixedsort(m$predictors$predictors))
#          stars::st_dimensions(scen$data)$band$values <- closest_match
#          scen$data[["current"]] <- m$predictors$data
#        }
#      } else {
#        if (is_predictors(m$predictors)) {
#          # scenarios()
#          scen$data[["current"]] <- m$predictors$data
#        } else if (is_sdm_area(m$predictors)) {
#          #
#          scen$data[["current"]] <- m$predictors$data
#        }
#      }
#    }
#  }
#  p <- list()
#  for (i in 1:length(scen$data)) {
#    print(paste0("Projecting: ", i, "/", length(scen$data)))
#    suppressWarnings(x <- cbind(sf::st_coordinates(sf::st_centroid(sf::st_as_sf(scen$data[i]))), dplyr::select(as.data.frame(sf::st_as_sf(scen$data[i])), -"geometry")))
#    # x <- na.omit(x[,y$predictors])
#    # closest_match <- find_closest_matches(st_dimensions(scen$data)$band$values, gtools::mixedsort(m$predictors$predictors_names))
#    # colnames(x) <- c("x","y",closest_match)
#    x <- apply(x, 2, function(x) {
#      as.numeric(gsub(NaN, NA, x))
#    })
#    suppressWarnings(p[[i]] <- sapply(m1, function(m2) {
#      p2 <- predict(m2, newdata = na.omit(x), type = tp)
#      cell_id <- scen$cell_id[!is.nan(as.data.frame(sf::st_as_sf(scen$data[i]))[, 1])]
#      p2 <- sapply(p2, function(x) {
#        cbind(cell_id, x)
#      }, simplify = F, USE.NAMES = T)
#      return(p2)
#    }, simplify = F, USE.NAMES = T))
#
#    if (!ensembles) {
#      write.csv(p[[i]], paste0(names(scen$data)[i], ".csv"))
#    }
#  }
#  names(p) <- names(scen$data)
#  ## p <- lapply(p, function(x){x <- subset(x,select=c('presence'))
#  ##                           x$cell_id <- rownames(x)
#  ##                           x <- x[,c('cell_id', 'presence')]
#  ##                           return(x)})
#  ## models_names <- names(p)
#  ## p <- reduce(p,inner_join, by='cell_id')
#  ## colnames(p) <- c('cell_id',models_names)
#  ## p <- merge(scen$df, p, by = "cell_id", all.x = TRUE)
#  ## p <- p[,c('cell_id', 'x', 'y', models_names)]
#  #
#  ## if(!is.null(file)){
#  ##  write.csv(p, file)
#  ## }
#
#  ### INCLUIR ENSEMBLES AQUI ###
#  if (ensembles) {
#    print("Ensembling...")
#    e <- sapply(names(p), function(y) {
#      print(y)
#      y <- p[[y]]
#      e2 <- sapply(names(y), function(sp) {
#        print(sp)
#        x <- y[[sp]]
#        if (length(x) > 0) {
#          # Prepare data
#          suppressMessages(df <- dplyr::bind_cols(x))
#          df <- dplyr::select(df, contains("presence"))
#          # mean_occ_prob
#          mean_occ_prob <- rowMeans(df)
#          # wmean_AUC
#          wmean_AUC <- apply(df, 1, function(x) {
#            stats::weighted.mean(x, th1[[sp]]$ROC)
#          })
#          # Obtain Thresholds:
#          suppressWarnings(th2 <- lapply(m1[[sp]], function(x) {
#            caret::thresholder(x,
#              threshold = seq(0, 1, by = 0.01),
#              final = TRUE,
#              statistics = "all"
#            )
#          }))
#          th2 <- lapply(th2, function(x) {
#            x <- x %>% dplyr::mutate(th = Sensitivity + Specificity)
#            th <- x[x$th == max(x$th), "prob_threshold"]
#            if (length(th) > 1) {
#              th <- mean(th)
#            }
#            return(th)
#          })
#          # binary
#          for (i in 1:ncol(df)) {
#            df[, i] <- ifelse(df[, i][] > th2[i], 1, 0)
#          }
#          committee_avg <- rowMeans(df)
#
#          # save everything
#          df <- data.frame(cell_id = x[[1]]$cell_id, mean_occ_prob, wmean_AUC, committee_avg)
#          return(df)
#        } else {
#          warning(paste0(sp, " has no models passing the threshold."))
#          df <- NULL
#        }
#      }, simplify = FALSE, USE.NAMES = TRUE)
#    }, USE.NAMES = TRUE)
#    if (length(names(p[[1]])) == 1 & !any(class(e) == "matrix")) {
#      e <- t(as.matrix(e))
#      rownames(e) <- names(p[[1]])
#      colnames(e) <- gsub(paste0(".", names(p[[1]])), "", colnames(e))
#    }
#  }
#  p2 <- list(
#    thresholds = list(values = th1, method = tm, criteria = th),
#    predictions = p,
#    models = m,
#    file = file,
#    ensembles = e,
#    grid = scen$grid
#  )
#  predic <- .predictions(p2)
#  if (is_input_sdm(m)) {
#    m$predictions <- predic
#    predic <- m
#  }
#  return(predic)
#}
#' @rdname predict_sdm
#' @export
scenarios_names <- function(i) {
  if (is_input_sdm(i) | is_sdm_area(i)) {
    return(names(i$scenarios$data))
  }
  return(NULL)
}

#' @rdname predict_sdm
#' @export
get_scenarios_data <- function(i) {
  if (is_input_sdm(i)) {
    return(i$scenarios$data)
  }
  return(NULL)
}

#' @rdname predict_sdm
#' @export
get_predictions <- function(i) {
  if (is_input_sdm(i)) {
    return(i$predictions$predictions)
  }
  return(NULL)
}

#' @rdname predict_sdm
#' @export
get_ensembles <- function(i) {
  if (is_input_sdm(i)) {
    return(i$predictions$ensembles)
  }
  return(NULL)
}

#' @export
.predictions <- function(x) {
  predictions <- structure(
    list(
      thresholds = x$thresholds,
      predictions = x$predictions,
      models = x$models,
      file = x$file,
      ensembles = x$ensembles,
      grid = x$grid
    ),
    class = "predictions"
  )
  return(predictions)
}

#' Print method for predictors
#' @exportS3Method base::print
print.predictions <- function(x) {
  cat("         caretSDM        \n")
  cat(".........................\n")
  cat("Class             : Predictions\n")
  cat(
    "Ensembles         :\n",
    "        Methods  :", colnames(x$ensembles[1, 1][[1]])[-1], "\n"
  )
  cat(
    "Thresholds        :\n",
    "        Method   :", x$thresholds$method, "\n",
    "        Criteria :", x$thresholds$criteria, "\n",
    "        Metrics  :\n"
  )
  print(x$thresholds$values)
}
