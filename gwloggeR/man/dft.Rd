% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fourier.R
\name{dft}
\alias{dft}
\title{Deiscrete Fourier Transform}
\usage{
dft(h, x)
}
\arguments{
\item{h}{integer from 0 to N-1.}

\item{x}{a vector of values.}
}
\description{
This is similar to DTFT, but here the frequency is an integer between 0 and
N - 1. This function is equivalent to stats::fft(), just slower.
It also computes the complex value for one h, while stats::fft() returns
a vector for all h.
}
\keyword{internal}
