% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{process_granivores}
\alias{process_granivores}
\title{Filters out non-granivores.}
\usage{
process_granivores(rodent_species_merge, type)
}
\arguments{
\item{rodent_species_merge}{Data table with raw rodent records
merged with species attributes from
species_table.}

\item{type}{String. If type=granivores', non-granivores removed.}
}
\value{
data.table with granivores processed according to argument 'type'.
}
\description{
If type=granivores, removes all non-granivore species.
}
