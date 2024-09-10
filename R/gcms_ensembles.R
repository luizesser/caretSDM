#' Ensemble GCMs into one scenario
#'
#' An ensembling method to group different GCMs into one SSP scenario
#'
#' @usage
#' ensemble_gcms(i, gcms = NULL)
#'
#' @param i A \code{input_sdm} object.
#' @param gcms GCM codes in \code{scenarios_names(i)} to group scenarios.
#'
#' @returns A \code{input_sdm} object with grouped GCMs.
#'
#' @details
#'
#' @seealso \code{\link{GBIF_data} \link{occurrences} \link{sdm_area} \link{input_sdm}
#' \link{predictors}}
#'
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#'
#' @examples
#' # Create sdm_area object:
#' sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#'
#' # Include predictors:
#' sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))
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
#' # Clean coordinates:
#' i <- data_clean(i)
#'
#' @importFrom stars st_extract st_rasterize
#' @importFrom stringdist stringdist
#' @importFrom sf st_as_sf st_crs st_transform st_join
#' @importFrom dplyr bind_cols
#'
#' @export
gcms_ensembles <- function(i, gcms=NULL) {
  if (is_input_sdm(i)) {
    y <- i$predictions$ensembles
  }
  emet <- colnames(y[[1]])[-1]
  cols <- colnames(y)
  for (g in gcms) {
    cols <- gsub(g,"",cols)
  }

  scen_names <- names(table(cols)[table(cols)>1])

  l <- sapply(scen_names, function(sc){
    ysc <- y[,grep(sc, colnames(y))]
    ysc <- dplyr::bind_cols(ysc)
    l2 <- list()
    for (m in emet) {
      l2[[m]] <- rowMeans(ysc[,grep(m, colnames(ysc))])
    }
    l2 <- data.frame(cell_id=ysc[,1] , dplyr::bind_cols(l2))
  }, simplify=FALSE, USE.NAMES=TRUE)

  for(s in scen_names){
    y[,s] <- l[[s]]
  }

  return(y)
}
