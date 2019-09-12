#' @title Version object
#' @description Version object holds all the necessary information about the
#' algorithm used to generate the output.
#' @keywords internal
#'
Version <- function(version, url = NULL) {
  if (!is.character(version)) stop('ERROR: version must be a character string.')
  structure(version, 'url' = url)
}

#' @keywords internal
#'
set.version <- function(obj, version) {
  data.table::setattr(x = obj, name = 'version', value = version)
}
