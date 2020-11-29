#' Coalesce for NULLs
#'
#' @name coalesce
#' @keywords internal
#'
`%|%` <- function(a, b) if (is.null(a)) b else a

#' Concat function, Oracle style
#'
#' @name concat
#' @keywords internal
#'
`%||%` <- function(str1, str2) paste0(str1, str2)
