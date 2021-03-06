% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alucard.R
\name{char_summary}
\alias{char_summary}
\title{A summary of a character vector}
\usage{
char_summary(x, na.rm = T)
}
\arguments{
\item{x}{a character vector}

\item{na.rm}{an optional logical parameter. TRUE by default}
}
\value{
A named vector with six values
}
\description{
Custom version of summary function for a character vector
}
\examples{
x <- c("A","b","c","E")
char_summary(x)
}
