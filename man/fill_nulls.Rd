% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{fill_nulls}
\alias{fill_nulls}
\title{Convert null values to specified value}
\usage{
fill_nulls(data, null_val = NA)
}
\arguments{
\item{data}{value to be checked}

\item{null_val}{value to return if data is \code{NULL}}
}
\description{
Since .bbrz files will sometimes have no value recorded for some things (examples seen include \code{x} position of ball, game turn, etc.), we get a NULL if we try to call them. Passing all calls for data from a replay file through here allows us to control the returned value if the data is unrecorded. For example, it appears that the Ball$IsHeld value is set to "1" when the ball is held, but is not reported at all if the ball is free.
}
