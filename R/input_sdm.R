#' Create a input_sdm object
#'
#' This function creates a new predictors object
#'
#' @param ... Data to be used in SDMs. Can be a occurrences, a predictors and/or a scenarios object.
#'
#' @return A input_sdm object.
#'
#' @seealso \code{\link{occurrences}\link{predictors}\link{scenarios}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @export
input_sdm <- function(...){
  x <- list(...)

  #occ <- UseMethod("occurrences", occ)
  #pred <- UseMethod("predictors", pred)
  #scen <- UseMethod("scenarios", s)

  inp <- .input_sdm(x)
  return(inp)
}

#' @export
.input_sdm <- function(x){
  classes <- lapply(x, class)
  l <- list()
  if('occurrences' %in% classes){l$occurrences <- x[classes %in% 'occurrences'][[1]]}
  if('predictors'  %in% classes){l$predictors  <- x[classes %in% 'predictors' ][[1]]}
  if('scenarios'   %in% classes){l$scenarios   <- x[classes %in% 'scenarios'  ][[1]]}
  inp <- structure(l,
                   class = "input_sdm")
  return(inp)
}

#' Print method for predictors
#' @exportS3Method base::print
print.input_sdm <- function(x) {
  cat("          caretSDM         \n")
  cat("...........................\n")
  cat("Class                     : input_sdm\n")
  if('occurrences' %in% names(x)){
    cat("Species Names             :", x$occurrences$spp_names, "\n")
    cat("Number of presences       :", x$occurrences$n_presences, "\n")
    if(!is.null(x$occurrences$pseudoabsences)){cat("Pseudoabsence methods     :\n",
                                                   "        Method to obtain PAs       :", x$occurrences$pseudoabsences$method, "\n",
                                                   "        Number of PA sets          :", x$occurrences$pseudoabsences$n_set, "\n",
                                                   "        Number of PAs in each set  :", as.numeric(x$occurrences$pseudoabsences$n_pa), "\n" )}
    if(!is.null(x$occurrences$background)){cat("Background sets           :", length(x$occurrences$background), "\n")}
    if('independent_test' %in% names(x$occurrences)){cat("Independent Test          : TRUE (number of records = ",nrow(x$occurrences$independent_test),")\n")}
    if(!is.null(x$occurrences$data_cleaning)){cat(cat("Data Cleaning             : "), cat(x$occurrences$data_cleaning, sep=', '), "\n")}
  }
  if('predictors' %in% names(x)){
    cat("Number of Predictors      :", length(x$predictors$predictors_names), "\n")
    cat(cat("Predictors Names          : "), cat(x$predictors$predictors_names, sep=', '), "\n")
    if(!is.null(x$predictors$bbox)){                  cat("Extent                    :", x$predictors$bbox, "(xmin, xmax, ymin, ymax)\n")}
    if(!is.null(x$predictors$epsg)){                  cat("EPSG                      :", x$predictors$epsg, "\n")}
    if(!is.null(x$predictors$resolution)){            cat("Resolution                :", x$predictors$resolution, "(x, y)\n")}
    if(!is.null(x$predictors$variable_selection$vif)){cat(cat("Area (VIF)                : "), cat(x$predictors$variable_selection$vif$area), cat("\n"),
                                                          cat("Selected Variables (VIF)  : "), cat(x$predictors$variable_selection$vif$selected_variables, sep=', '), "\n")}
  }
  if('scenarios' %in% names(x)){
    cat("Scenarios Names           :", names( x$scenarios$data), "\n")
    cat("Number of Scenarios       :", length(x$scenarios$data), "\n")
  }
  if('models' %in% names(x)){
    if(class(x$models$algorithms)=='list'){
      cat("Algorithms Names          : Stacked Ensemble\n")
      for(j in 1:length(x$models$algorithms)){
        cat("        Layer ",j,": ",x$models$algorithms[[j]], '\n')
      }
    } else {
      cat("Algorithms Names          :", x$models$algorithms, "\n")
    }
    cat("Variables Names           :", x$models$predictors, "\n")
    cat("Model Validation          :\n",
        "        Method    :", x$models$validation$method, "\n",
        "        Number    :", x$models$validation$number, "\n",
        "        Metrics   :\n" )
    print(x$models$validation$metrics)
  if('independent_validation' %in% names(x$models)){
    cat("Independent Validation    :\n",
        "        ROC (mean ± sd)  : ", round(mean(unlist(x$models$independent_validation)),3),
        " ± ",
        round(sd(unlist(x$models$independent_validation)),3), "\n")
  }
  }
  if('predictions' %in% names(x)){
    cat("Ensembles         :\n",
        "        Methods  :", rownames(x$predictions$ensembles), "\n")
    cat("Thresholds        :\n",
        "        Method   :", x$predictions$thresholds$method, "\n",
        "        Criteria :", x$predictions$thresholds$criteria, "\n")
  }
}