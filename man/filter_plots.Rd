% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{filter_plots}
\alias{filter_plots}
\title{Filter plots}
\usage{
filter_plots(data, length)
}
\arguments{
\item{data}{Data table. Any data with a plot column.}

\item{length}{Character. Denotes if user wants only long-term plots.}
}
\value{
Data.table filtered to the desired subset of plots.
}
\description{
Removes plots not needed for analysis. Currently only returns long-term
plots but could be adjusted in the future to return other subsets as well.
}
