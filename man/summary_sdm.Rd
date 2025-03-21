% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_sdm.R
\name{summary_sdm}
\alias{summary_sdm}
\title{Calculates performance across resamples}
\usage{
summary_sdm(data, lev = NULL, model = NULL)
}
\arguments{
\item{data}{A \code{data.frame} with observed and predicted values.}

\item{lev}{A \code{character} vector of factors levels for the response.}

\item{model}{Models names taken from \code{train} object.}

\item{custom_fun}{A custom function to be applied in models (not yet implemented).}
}
\value{
A \code{input_sdm} or a \code{predictions} object.
}
\description{
This function is used in \code{caret::trainControl(summaryFunction=summary_sdm)} to calculate
performance metrics across resamples.
}
\details{
See \code{?caret::defaultSummary} for more details and options to pass on
\code{caret::trainControl}.
}
\examples{
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 25000, crs = 6933)

# Include predictors:
sa <- add_predictors(sa, bioc) |> dplyr::select(c("bio01", "bio12"))

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Clean coordinates:
i <- data_clean(i)

# VIF calculation:
i <- vif_predictors(i)

# Pseudoabsence generation:
i <- pseudoabsences(i, method="bioclim", variables_selected = "vif")

# Custom trainControl:
ctrl_sdm <- caret::trainControl(method = "repeatedcv", number = 4, repeats = 10, classProbs = TRUE,
returnResamp = "all", summaryFunction = summary_sdm, savePredictions = "all")

# Train models:
i <- train_sdm(i, algo = c("nnet", "kknn"), variables_selected = "vif", ctrl = ctrl_sdm)
i

}
\seealso{
\code{\link{train_sdm} \link{caret::trainControl}}
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
