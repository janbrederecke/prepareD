% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_overview.R
\name{data_overview}
\alias{data_overview}
\title{data_overview}
\usage{
data_overview(.data, .caption = "", .print = TRUE, .percent_missing = 30)
}
\arguments{
\item{.data}{A data.frame or tibble.}

\item{.caption}{A character vector used as the caption of the output.}

\item{.print}{If .print = TRUE, an HTML output is generated, else a matrix is
returned.}

\item{.percent_missing}{A number determining the percentage of
missingness used to count variables with high amount of missing values.}
}
\value{
Either a HTML output for use in Markdown files or a matrix.
}
\description{
Provides an overview table for the input dataset.
}
\examples{
-
}
