% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{make_crosstab}
\alias{make_crosstab}
\title{Make Crosstab}
\usage{
make_crosstab(summary_data, variable_name = quo(abundance), ...)
}
\arguments{
\item{summary_data}{summarized rodent data}

\item{variable_name}{what variable to spread (default is "abundance")}

\item{...}{other arguments to pass on to tidyr::spread}
}
\description{
convert summarized rodent data to crosstab form
}
