% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowmute.R
\name{rowmute}
\alias{rowmute}
\title{Apply mutate rowwise}
\usage{
rowmute(.data, ...)
}
\arguments{
\item{.data}{A data frame or a data frame extension (e.g. a tibble)}

\item{...}{<data-masking> Name-value pairs. The name gives the name of the column in the output.
Any output other than a vector of length 1 will be stored in a list-column.}
}
\value{
An object of the same type as `.data`, with modified columns.
}
\description{
A version of mutate that automatically works row-wise and handles list-columns elegantly
}
\details{
\code{\link[dplyr]{mutate}} is an extremely powerful function for adding or transforming
variables in a data frame. However, it's not designed for working with list-columns, so the
syntax is often unwieldy. \code{rowmute} is a wrapper around \code{mutate} that handles that
syntax and helps avoid some of the pitfalls.

\code{rowmute} differs from \code{mutate} in 3 ways:
\enumerate{
   \item \code{rowmute} always works one row at a time.
   \item \code{rowmute} automatically puts complex output, such as model fit results, into list
     columns
   \item \code{rowmute} can automatically display progress output using the
     \code{\link[progressr:progressr-package]{progressr}} package
   }
}
\examples{
library(dplyr)
library(tidyr)
library(broom)
data(mtcars)

by_cyl <- mtcars \%>\%
  nest(data = -cyl)

model_results <- by_cyl \%>\%
  rowmute(mod = lm(mpg ~ wt, data = data),
          aug_data = augment(mod, data),
          mod_summary = glance(mod))
}
