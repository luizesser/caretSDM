#' sdm_as_X functions to transform caretSDM data into other classes.
#'
#' This functions transform data in a caretSDM object to be used in other packages.
#'
#' @param x caretSDM object
#' @param what Sometimes multiple objects inside x could be transformed. This parameter allows users to specify what needs to be converted.
#'
#' @returns The output is the desired class
#'
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @importFrom terra rast
#'
#' @export
sdm_as_stars <- function(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL) {
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
  if (class(x) == "input_sdm") {
    if (what == "scenarios") {
      return(x$scenarios$data)
    }
    if (what == "predictors") {
      return(x$predictors$data)
    }
    if (what == "predictions") {
      if (is.null(spp)) {
        spp <- names(x$predictions$predictions[[1]])[1]
      }
      if (is.null(scen)) {
        scen <- names(x$predictions$predictions)[1]
      }
      if (is.null(id)) {
        id <- names(x$predictions$predictions[[1]][[1]])[1]
      }
      grd <- x$predictors$grid
      v <- select(x$predictions$predictions[[scen]][[spp]][[id]], -"pseudoabsence")
      result <- st_as_stars(merge(grd, v, by = "cell_id"))
      return(result)
    }
    if (what == "ensembles") {
      if (is.null(spp)) {
        spp <- rownames(x$predictions$ensembles)[1]
      }
      if (is.null(scen)) {
        scen <- colnames(x$predictions$ensembles)[1]
      }
      if (is.null(ens)) {
        ens <- "mean_occ_prob"
      }
      grd <- x$predictors$grid
      v <- x$predictions$ensembles[[spp, scen]][, c("cell_id", ens)]
      result <- st_as_stars(merge(grd, v, by = "cell_id"))
      return(result)
    }
  }
}

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
  if (class(x) == "input_sdm") {
    if (what == "scenarios") {
      if (is.null(scen)) {
        scen <- names(x$scenarios$data)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      result <- as(st_rasterize(st_as_sf(x$scenarios$data[scen])), "Raster")
      return(result)
    }
    if (what == "predictors") {
      result <- as(st_rasterize(st_as_sf(x$predictors$data[scen])), "Raster")
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
      v <- select(x$predictions$predictions[[scen]][[spp]][[id]], -"pseudoabsence")
      result <- as(st_rasterize(st_as_sf(select(st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster")
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
      result <- as(st_rasterize(st_as_sf(select(st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster")
      return(result)
    }
  }
}

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
  if (class(x) == "input_sdm") {
    if (what == "scenarios") {
      if (is.null(scen)) {
        scen <- names(x$scenarios$data)[1]
        print(paste0("scen not detected. Using scen=", scen))
      }
      result <- terra::rast(as(st_rasterize(st_as_sf(x$scenarios$data[scen])), "Raster"))
      return(result)
    }
    if (what == "predictors") {
      result <- terra::rast(as(st_rasterize(st_as_sf(x$predictors$data[scen])), "Raster"))
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
      v <- select(x$predictions$predictions[[scen]][[spp]][[id]], -"pseudoabsence")
      result <- terra::rast(as(st_rasterize(st_as_sf(select(st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster"))
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
      result <- terra::rast(as(st_rasterize(st_as_sf(select(st_as_stars(merge(grd, v, by = "cell_id")), -"cell_id"))), "Raster"))
      return(result)
    }
  }
}
