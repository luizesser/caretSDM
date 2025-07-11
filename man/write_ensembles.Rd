% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{write_ensembles}
\alias{write_ensembles}
\alias{write_predictions}
\alias{write_predictors}
\alias{write_models}
\alias{write_gpkg}
\alias{write_gpkg.sdm_area}
\alias{write_occurrences}
\alias{write_pseudoabsences}
\alias{write_grid}
\alias{write_validation_metrics}
\title{Write caretSDM data}
\usage{
write_ensembles(x, path = NULL, ext = ".tif", centroid = FALSE)

write_predictions(x, path = NULL, ext = ".tif", centroid = FALSE)

write_predictors(x, path = NULL, ext = ".tif", centroid = FALSE)

write_models(x, path = NULL)

write_gpkg(x, file_path, file_name)

\method{write_gpkg}{sdm_area}(x, file_path, file_name)

write_occurrences(x, path = NULL, grid = FALSE, ...)

write_pseudoabsences(x, path = NULL, ext = ".csv", centroid = FALSE)

write_grid(x, path = NULL, centroid = FALSE)

write_validation_metrics(x, path = NULL)
}
\arguments{
\item{x}{Object to be written. Can be of class \code{input_sdm}, \code{occurrences},
\code{predictions} or \code{models}.}

\item{path}{A path with filename and the proper extension (see details) or the directory to save
files in.}

\item{ext}{How it should be saved?}

\item{centroid}{Should coordinates for the centroids of each cell be included? Standard is FALSE.}

\item{file_path}{A path to save the \code{sdm_area} GeoPackage file.}

\item{file_name}{The name of the \code{sdm_area} GeoPackage file to be saved without extension.}

\item{grid}{Boolean. Return a grid.}

\item{...}{Arguments to pass to \code{sf::st_write} or \code{write.csv}.}
}
\value{
No return value, called for side effects.
}
\description{
This function exports caretSDM data.
}
\details{
\code{ext} can be set accordingly to the desired output. Possible values are .tif and .asc for
rasters, .csv for for a spreadsheet, but also  one of: c("bna", "csv", "e00", "gdb", "geojson",
"gml", "gmt", "gpkg", "gps", "gtm", "gxt", "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp",
"sqlite", "vdv", "xls", "xlsx").
\code{path} ideally should only provide the folder. We recommend using:
\code{results/what_are_you_writting}. So for writting ensembles users are advised to run:
\code{path = "results/ensembles"}
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
