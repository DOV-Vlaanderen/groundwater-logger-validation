sys.source('./utils/utils.cache.r', envir = (cache <- new.env()))

#' Logging function
wlog <- function(string, ...) {
  cat(paste0('# ', Sys.time(),
             ' | ', sprintf(string, ...), '\n'))
}
