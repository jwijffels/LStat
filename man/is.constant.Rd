% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{is.constant}
\alias{is.constant}
\title{Checks if all values in a vector are the same}
\usage{
is.constant(x, na.rm = FALSE, ...)
}
\arguments{
\item{x}{vector}

\item{na.rm}{exclude missing values before checking if the data in x is constant}

\item{...}{other arguments passed to unique}
}
\value{
returns \code{TRUE} if all the data in x have the same value, else it returns \code{FALSE}. For vectors of length 0, returns a logical element of length zero
}
\description{
Checks if all values in a vector are the same. \cr
Uses \code{\link[base]{unique}}
}
\examples{
x <- c(1,1,1,1,NA)
is.constant(x)
is.constant(x, na.rm=TRUE)
x <- 1:100
is.constant(x)
is.constant(numeric(0))
\dontrun{
is.constant(list(x = 1:10))
}
}
\seealso{
\code{\link[base]{unique}}
}
