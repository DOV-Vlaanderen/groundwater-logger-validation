#' Function for uniform testing of the detect_...() functions
#'
#' @keywords internal
#'
test.detect_function <- function(fun, ..., RESULT.PATH, ATTRIB.PATH = NULL, IMG.PATH = NULL) {

  stopifnot(!is.null(RESULT.PATH))

  cat(sprintf('%s | %s\n', Sys.time(), toupper(basename(tools::file_path_sans_ext(RESULT.PATH)))))

  if (!is.null(IMG.PATH)) {
    dir.create(dirname(IMG.PATH), showWarnings = FALSE, recursive = TRUE)
    png(IMG.PATH, width = 1280, height = 720)
    on.exit(dev.off())
  }

  result <- fun(...)

  if (!is.null(ATTRIB.PATH)) {
    dir.create(dirname(ATTRIB.PATH), showWarnings = FALSE, recursive = TRUE)
    attribs <- attributes(result)
    attribs[sapply(attribs, is.function)] <- NULL # print.function() prints envir and bytecode
    capture.output(attribs, file = ATTRIB.PATH)
  }

  dir.create(dirname(RESULT.PATH), showWarnings = FALSE, recursive = TRUE)
  write.csv(
    data.frame('TIMESTAMP' = list(...)[['timestamps']],
               'VALUE' = list(...)[['x']],
               'RESULT' = result),
    file = RESULT.PATH, row.names = FALSE
  )

  invisible(TRUE)
}
