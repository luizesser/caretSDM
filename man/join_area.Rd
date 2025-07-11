% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_area.R
\name{join_area}
\alias{join_area}
\title{Join Area}
\usage{
join_area(occ, pred)
}
\arguments{
\item{occ}{A \code{occurrences} object or \code{input_sdm}.}

\item{pred}{A \code{sdm_area} object to retrieve cell_id from.}
}
\value{
A \code{occurrences} object with \code{cell_id} to each record.
}
\description{
Join cell_id data from sdm_area to a occurrences
}
\details{
This function is key in this SDM workflow. It attaches cell_id values to \code{occ}, deletes
records outside \code{pred} and allows the use of pseudoabsences. This function also tests if
CRS from both \code{occ} and \code{pred} are equal, otherwise the CRS of \code{pred} is used to
convert \code{occ}.
}
\examples{
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 50000, crs = 6933)

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))

# Include scenarios:
sa <- add_scenarios(sa, scen)

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

}
\seealso{
\code{\link{occurrences_sdm} \link{sdm_area} \link{input_sdm}
\link{pseudoabsences}}
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
