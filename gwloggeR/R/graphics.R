#' Computes the vector size in inch
#'
#' This function is mainly inspired by the following post:
#' https://stackoverflow.com/a/52690054/1548942
#'
#' @param x1,y1 Endpoint of the vector.
#' @param x0,y0 Origin of the vector. Default: (0,0).
#'
#' @keywords internal
#'
gr_vectorsize.inch <- function(x1, y1, x0 = 0, y0 = 0) {
  x <- x1 - x0
  y <- y1 - y0
  pin <- graphics::par('pin')
  usr <- graphics::par('usr')
  x_inch.convfactor <- pin[1L]/diff(usr[1:2])
  y_inch.convfactor <- pin[2L]/diff(usr[3:4])
  sqrt((x_inch.convfactor*x)^2 + (y_inch.convfactor*y)^2)
}
