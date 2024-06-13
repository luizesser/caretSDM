#' Indlude pseudoabsence data in a occurrences object
#'
#' This function includes pseudoabsences in a given occurrences object given a set of predictors
#'
#' @param occ A occurrences object
#' @param pred A predictors object
#' @param method Method to create pseudoabsences
#' @param n_set Number of sets of pseudoabsence to create
#' @param n_pa Number of pseudoabsences to be generated in each dataset (vector of size number of species, with species names)
#'
#' @return A occurrences object with pseudoabsence data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @import dismo
#' @import sp
#' @import dplyr
#' @import stars
#' @importFrom raster extract
#'
#' @export
pseudoabsences <- function(occ, pred = NULL, method = "random", n_set = 10, n_pa = NULL, variables_selected = NULL, th=0) {
  if (class(occ) == "input_sdm") {
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
  if(is_predictors(pred)){
    if (is.null(variables_selected)) {
      selected_vars <- pred$predictors_names
      cat("Using all variables available: ", selected_vars)
    }
    if (any(variables_selected %in% pred$predictors_names)) {
      selected_vars <- pred$predictors_names[pred$predictors_names %in% variables_selected]
      cat("Using given variables: ", selected_vars)
    }
  } else if (is_sdm_area(pred)){
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
    cat("Using variables selected by ", variables_selected, ": ", selected_vars)
  }
  if(is_predictors(pred)){
    suppressWarnings(df <- st_centroid(st_as_sf(filter(na.omit(pred$data), band %in% selected_vars))))
    df <- select(cbind(pred$grid, df), -"geometry.1")
  } else if (is_sdm_area(pred)){
    df <- pred$grid |>
      select(all_of(c("cell_id",selected_vars)))
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
        occ2 <- df[df$cell_id %in% y$occurrences[y$occurrences$species == sp, ]$cell_id, ]
        model <- dismo::bioclim(x = select(as.data.frame(occ2), all_of(selected_vars)))
        p <- predict(model, as.data.frame(df))
        p[p[] > th] <- NA
        p <- data.frame(cell_id = df$cell_id, pred = p)
        p <- p[!is.na(p$pred), ]
        l <- list()
        if(nrow(p)==0){
          print(paste0("bioclim envelope for ",sp," covered all the study area. Change th argument or change the method."))
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
        if(nrow(p)==0){
          print(paste0("bioclim envelope for ",sp," covered all the study area. Change th argument or change the method."))
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

#' @export
.pseudoabsences <- function(occ, l, method, n_set, n_pa) {
  occ$pseudoabsences$data <- l
  occ$pseudoabsences$method <- method
  occ$pseudoabsences$n_set <- n_set
  occ$pseudoabsences$n_pa <- n_pa
  return(occ)
}
