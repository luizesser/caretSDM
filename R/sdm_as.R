#' \code{sdm_as_X} functions to transform \code{caretSDM} data into other classes.
#'
#' This functions transform data from a \code{caretSDM} object to be used in other packages.
#'
#' @usage
#' sdm_as_stars(x,
#'              what = NULL,
#'              spp = NULL,
#'              scen = NULL,
#'              id = NULL,
#'              ens = NULL)
#'
#' @param x A \code{caretSDM} object.
#' @param what Sometimes multiple data inside \code{x} could be transformed. This parameter allows
#' users to specify what needs to be converted.It can be one of: "predictors", "scenarios",
#' "predictions" or "ensembles".
#' @param spp \code{character}. Which species should be converted?
#' @param scen \code{character}. Which scenario should be converted?
#' @param id \code{character}. Which id should be converted?
#' @param ens \code{character}. Which ensemble should be converted?
#'
#' @returns The output is the desired class.
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
#' i <- pseudoabsences(i, method="random")
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
#'   suppressWarnings()
#'
#' # Predict models:
#' i  <- predict_sdm(i, th=0.8)
#'
#' # Transform in stars:
#' sdm_as_stars(i)
#'
#' @importFrom terra rast
#' @importFrom dplyr select
#' @importFrom stars st_as_stars st_rasterize
#' @importFrom sf st_as_sf
#' @importFrom methods as
#'
#' @export
sdm_as_stars <- function(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL) {
  if (is.null(what)) {
    if ("predictions" %in% names(x)) {
      if ("ensembles" %in% names(x$predictions) & !is.null(x$predictions$ensembles)) {
        what <- "ensembles"
      } else {
        what <- "predictions"
      }
    } else if ("scenarios" %in% names(x)) {
      what <- "scenarios"
    } else if ("predictors" %in% names(x)) {
      what <- "predictors"
    }
  }
  if (is_input_sdm(x)) {
    if (what == "scenarios") {
      s <- sapply(x$scenarios$data, function(y){stars::st_as_stars(y)}, USE.NAMES = TRUE, simplify = FALSE)
      return(s)
    }
    if (what == "predictors") {
      return(stars::st_as_stars(x$predictors$grid))
    }
    if (what == "predictions") {
      if (is.null(spp)) {
        spp <- species_names(x)[[1]][1]
      }
      if (is.null(scen)) {
        scen <- scenarios_names(x)[1]
      }
      if (is.null(id)) {
        id <- names(x$predictions$predictions[[1]][[1]])[1]
      }
      grd <- x$predictors$grid
      v <- get_predictions(x)[[scen]][[spp]][[id]]
      result <- stars::st_as_stars(v)
      return(result)
    }
    if (what == "ensembles") {
      if (is.null(spp)) {
        spp <- species_names(x)[1]
      }
      if (is.null(scen)) {
        scen <- scenarios_names(x)[1]
      }
      if (is.null(ens)) {
        ens <- "mean_occ_prob"
      }
      grd <- x$predictors$grid
      v <- get_ensembles(x)[[spp, scen]][, c("cell_id", ens)]
      result <- stars::st_as_stars(merge(grd, v, by = "cell_id"))
      result <- dplyr::select(result, all_of(c("cell_id", ens)))
      return(result)
    }
  }
}

#' @rdname sdm_as_stars
#' @export
sdm_as_raster <- function(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL) {
  if (is.null(what)) {
    if ("predictions" %in% names(x)) {
      if ("ensembles" %in% names(x$predictions)) {
        what <- "ensembles"
      } else {
        what <- "predictions"
      }
    } else if ("predictors" %in% names(x)) {
      what <- "predictors"
    } else if ("scenarios" %in% names(x)) {
      what <- "scenarios"
    }
  }
  if (is_input_sdm(x)) {
    if (what == "scenarios") {
      if (is.null(scen)) {
        scen <- names(x$scenarios$data)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      result <- methods::as(stars::st_rasterize(sf::st_as_sf(x$scenarios$data[scen])), "Raster")
      return(result)
    }
    if (what == "predictors") {
      result <- methods::as(stars::st_rasterize(sf::st_as_sf(x$predictors$data[scen])), "Raster")
      return()
    }
    if (what == "predictions") {
      if (is.null(spp)) {
        spp <- names(x$predictions$predictions[[1]])[1]
        print(paste0("spp not detected. Using spp=", spp))
      }
      if (is.null(scen)) {
        scen <- names(x$predictions$predictions)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      if (is.null(id)) {
        id <- names(x$predictions$predictions[[1]][[1]])[1]
        print(paste0("id not detected. Using id=", id))
      }
      grd <- x$predictors$grid
      v <- dplyr::select(x$predictions$predictions[[scen]][[spp]][[id]], -"pseudoabsence")
      result <- methods::as(stars::st_rasterize(sf::st_as_sf(dplyr::select(stars::st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster")
      return(result)
    }
    if (what == "ensembles") {
      if (is.null(spp)) {
        spp <- names(x$predictions$predictions[[1]])[1]
        print(paste0("spp not detected. Using spp=", spp))
      }
      if (is.null(scen)) {
        scen <- names(x$predictions$predictions)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      if (is.null(ens)) {
        ens <- "mean_occ_prob"
        print(paste0("ens not detected. Using ens=", ens))
      }
      grd <- x$predictors$grid
      v <- x$predictions$ensembles[[spp, scen]][, c("cell_id", ens)]
      result <- methods::as(stars::st_rasterize(sf::st_as_sf(dplyr::select(stars::st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster")
      return(result)
    }
  }
}

#' @rdname sdm_as_stars
#' @export
sdm_as_terra <- function(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL) {
  if (is.null(what)) {
    if ("predictions" %in% names(x)) {
      if ("ensembles" %in% names(x$predictions)) {
        what <- "ensembles"
      } else {
        what <- "predictions"
      }
    } else if ("predictors" %in% names(x)) {
      what <- "predictors"
    } else if ("scenarios" %in% names(x)) {
      what <- "scenarios"
    }
  }
  if (is_input_sdm(x)) {
    if (what == "scenarios") {
      if (is.null(scen)) {
        scen <- names(x$scenarios$data)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      result <- terra::rast(methods::as(stars::st_rasterize(sf::st_as_sf(x$scenarios$data[scen])), "Raster"))
      return(result)
    }
    if (what == "predictors") {
      result <- terra::rast(methods::as(stars::st_rasterize(sf::st_as_sf(x$predictors$data[scen])), "Raster"))
      return()
    }
    if (what == "predictions") {
      if (is.null(spp)) {
        spp <- names(x$predictions$predictions[[1]])[1]
        print(paste0("spp not detected. Using spp=", spp))
      }
      if (is.null(scen)) {
        scen <- names(x$predictions$predictions)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      if (is.null(id)) {
        id <- names(x$predictions$predictions[[1]][[1]])[1]
        print(paste0("id not detected. Using id=", id))
      }
      grd <- x$predictors$grid
      v <- dplyr::select(x$predictions$predictions[[scen]][[spp]][[id]], -"pseudoabsence")
      result <- terra::rast(methods::as(stars::st_rasterize(sf::st_as_sf(dplyr::select(stars::st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster"))
      return(result)
    }
    if (what == "ensembles") {
      if (is.null(spp)) {
        spp <- names(x$predictions$predictions[[1]])[1]
        print(paste0("spp not detected. Using spp=", spp))
      }
      if (is.null(scen)) {
        scen <- names(x$predictions$predictions)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      if (is.null(ens)) {
        ens <- "mean_occ_prob"
        print(paste0("ens not detected. Using ens=", ens))
      }
      grd <- x$predictors$grid
      v <- x$predictions$ensembles[[spp, scen]][, c("cell_id", ens)]
      result <- terra::rast(methods::as(stars::st_rasterize(sf::st_as_sf(dplyr::select(stars::st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster"))
      return(result)
    }
  }
}
