#' Deiscrete Fourier Transform
#'
#' This is similar to DTFT, but here the frequency is an integer between 0 and
#' N - 1. This function is equivalent to stats::fft(), just slower.
#' It also computes the complex value for one h, while stats::fft() returns
#' a vector for all h.
#'
#' @param h integer from 0 to N-1.
#' @param x a vector of values.
#'
#' @keywords internal
#'
dft <- function(h, x) {
  stopifnot(length(h) == 1L)
  N <- length(x)
  stopifnot(is.integer(h))
  stopifnot(h <= N - 1)
  sum(x*exp(-2*pi*1i*(0:(N-1))*(h)/N))
}

#' Discrete-Time Fourier Transform (DTFT)
#'
#' See here: https://en.wikipedia.org/wiki/Discrete-time_dtft
#' Note that there is also Discrete Fourier Transform O(n^2) (DFT), which is
#' equivalent to the better optimized Fast Fourier Transform O(nlog(n)) (FFT)
#' implementation. The DFT is derived from the DTFT as the minimal discrete
#' representation in the frequency domain of the original finite series. The
#' DTFT is more general since the frequency domain representation is continous.
#' Hence why this function will compute the corresponding complex number for
#' any required frequency.
#'
#' @keywords internal
#'
dtft <- function(frequency, x, timestamps, plot = FALSE) {
  stopifnot(length(x) == length(timestamps))
  stopifnot(any(!is.na(x)))
  stopifnot(any(!is.na(timestamps)))
  stopifnot(inherits(timestamps, 'POSIXct'))

  projections <- x*exp(-2*pi*1i*as.numeric(timestamps)*frequency)
  psum <- sum(projections)

  if (plot) {
    graphics::plot(projections)
    arrow_size.inch <- gr_vectorsize.inch(Re(psum)/length(x), Im(psum)/length(x))
    if (arrow_size.inch > 0.001)
      graphics::arrows(0, 0, x1 = Re(psum)/length(x), y1 = Im(psum)/length(x), length = 0.1, lwd = 2)
    graphics::title(sprintf("%0.3g +%0.3gi",  Re(psum)/length(x), Im(psum)/length(x)))
  }

  psum
}

#' Fourier Basis
#'
#' If frequencies are not given, then default harmonics of Fourier Series
#' are used. Optionaly, one can choose to also add a trend and an intercept.
#'
#' @keywords internal
#'
fbasis <- function(timestamps, intercept = FALSE, trend = FALSE, frequencies = NULL) {
  stopifnot(inherits(timestamps, 'POSIXct'))
  stopifnot(intercept == FALSE)

  t <- as.numeric(timestamps) # t is in seconds
  P <- diff(range(t))

  # Max aantal sin en cos componenten volgens Nyquist:
  N <- ceiling(length(t)/2)
  frequencies <- if (is.null(frequencies)) seq(N)/P else frequencies

  basis <- list()

  if (trend) basis[['trend']] <- (t - t[1])/3600/24/365.25

  for (freq in frequencies) {
    s <- sprintf('sin(%s)', 1/freq)
    basis[[s]] <- sin(2*pi*freq*t)
    c <- sprintf('cos(%s)', 1/freq)
    basis[[c]] <- cos(2*pi*freq*t)
  }

  basis[['check.names']] <- FALSE
  do.call(data.frame, basis)
}
