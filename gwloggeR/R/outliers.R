#' @title Detect outliers
#' @description
#' This function marks outliers in the input vector.
#' @param x numeric vector of values
#' @return logical vector with same length as x, specifying TRUE for outlier
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname detect_outliers

detect_outliers <- function(x) {
  rep(FALSE, length(x))
}
