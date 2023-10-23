#' Indlude pseudoabsence data in a occurrences object
#'
#' This function includes pseudoabsences in a given occurrences object given a set of predictors
#'
#' @param occ A occurrences object
#' @param pred A predictors object
#' @param method Method to create pseudoabsences
#' @param n_set Number of sets of pseudoabsence to create
#' @param n_pa Number of pseudoabsences to be generated in each dataset
#'
#' @return A occurrences object with pseudoabsence data
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#'
#'
#' @export
pseudoabsences <- function(occ, pred=NULL, method='random', n_set=10, n_pa=NULL, variables_selected=NULL){
  if(class(occ)=='input_sdm'){
    y <- occ$occurrences
    pred <- occ$predictors
  } else {
    y <- occ
  }
  if(!is.null(y$pseudoabsences)){
    warning('Previous pseudoabsence element on Occurrences object was overwrited.', call.=F)
  }
  if(is.null(n_pa)){n_pa <- as.numeric(y$n_presences)}
  if(is.null(variables_selected)){
    selected_vars <- pred$predictors_names
    print(cat('Using all variables available: ', selected_vars))
  }
  if(any(variables_selected %in% pred$predictors_names) ){
    selected_vars <- pred$predictors_names[pred$predictors_names %in% variables_selected]
    print(cat('Using given variables: ', selected_vars))
  }
  if(length(variables_selected) == 1){
    if(length(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected])==0){
      print(paste0('Variable selection method not detected.'))
      stop()
    }
    selected_vars <- unlist(pred$variable_selection[attributes(pred$variable_selection)$names %in% variables_selected], rec=F)[[paste0(variables_selected,'.selected_variables')]]
    print(cat('Using variables selected by ',variables_selected,': ', selected_vars))
  }
  df <- na.omit(pred$data)
  df <- df[,selected_vars]
  if(method=="random"){
    l <- list()
    for(i in 1:n_set){
      l[[i]] <- df[sample(rownames(df), size=n_pa),]
    }
    pa <- .pseudoabsences(y,l, method, n_set, n_pa)
  }
  if(method=="bioclim"){
    # Criar envelope
  }
  if(method=="cluster"){
    # Reginaldo
  }

  if(class(occ)=='input_sdm'){
    i$occurrences <- y
    pa <- i
  }
  return(pa)
}

.pseudoabsences <- function(occ, l, method, n_set, n_pa){
  occ$pseudoabsences$data <- l
  occ$pseudoabsences$method <- method
  occ$pseudoabsences$n_set <- n_set
  occ$pseudoabsences$n_pa <- n_pa
  return(occ)
}
