% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occurrences_sdm.R
\name{occurrences_sdm}
\alias{occurrences_sdm}
\alias{n_records}
\alias{species_names}
\alias{get_coords}
\alias{occurrences_as_df}
\alias{add_occurrences}
\title{Occurrences Managing}
\usage{
occurrences_sdm(x,
                independent_test = NULL,
                p = 0.1,
                crs = NULL,
                independent_test_crs = NULL,
                ...)

n_records(i)

species_names(i)

get_coords(i)

occurrences_as_df(i)

add_occurrences(oc1, oc2)
}
\arguments{
\item{x}{A \code{data.frame}, \code{tibble} or \code{sf} with species records.}

\item{independent_test}{Boolean. If \code{independet_test} is \code{TRUE}, a fraction of the data
is kept for independent testing. Otherwise, the whole dataset \code{x} is used. It can also be a
\code{data.frame} or a \code{sf}, with species records to be used as independent test. Structure
and names should be identical to those in \code{x}.}

\item{p}{Numeric. Fraction of data to be used as independent test. Standard is 0.1.}

\item{crs}{Numeric. CRS of \code{x}.}

\item{independent_test_crs}{Numeric. CRS of \code{independent_test} if it is a
\code{data.frame}.}

\item{...}{A vector with column names addressing the columns with species names, longitude and
latitude, respectively, in \code{x}.}

\item{i}{\code{input_sdm} or \code{occurrences} object.}

\item{oc1}{A \code{occurrences} object to be summed with.}

\item{oc2}{A \code{occurrences} object to be summed with.}
}
\value{
A \code{occurrences} object.
}
\description{
This function creates and manage \code{occurrences} objects.
}
\details{
\code{x} must have three columns: species, decimalLongitude and decimalLatitude. When \code{sf}
it is only necessary a species column.
\code{n_records} return the number of presence records to each species.
\code{species_names} return the species names.
\code{get_coords} return a \code{data.frame} with coordinates of species records.
\code{add_occurrences} return a \code{occurrences}. This function sums two \code{occurrences} objects.
It can also sum a \code{occurrences} object with a \code{data.frame} object.
\code{occurrences_as_df} returns a \code{data.frame} with species names and coordinates.
}
\examples{
# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933)

}
\seealso{
\code{\link{input_sdm} \link{GBIF_data} \link{occ}}
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
