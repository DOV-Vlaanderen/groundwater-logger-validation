#' Function for uniform testing of the detect_...() functions
#'
#' @keywords internal
#'
test.detect_function <- function(fun, ..., RESULT.PATH, ATTRIB.PATH = NULL, IMG.PATH = NULL, LOG.PATH = NULL) {

  stopifnot(!is.null(RESULT.PATH))

  cat(sprintf('%s | %s\n', Sys.time(), toupper(basename(tools::file_path_sans_ext(RESULT.PATH)))))

  if (!is.null(IMG.PATH)) {
    dir.create(dirname(IMG.PATH), showWarnings = FALSE, recursive = TRUE)
    png(IMG.PATH, width = 1280, height = 720)
    on.exit(dev.off(), add = TRUE)
  }

  result <- try( # try will send message error by default (cf. silent = FALSE)
    local({
      if (!is.null(LOG.PATH)) {
        dir.create(dirname(LOG.PATH), showWarnings = FALSE, recursive = TRUE)
        log.file <- file(LOG.PATH, open = 'wt')
        sink(file = log.file, split = TRUE, type = 'output')
        sink(file = log.file, type = 'message') # cannot split message stream :( !

        # Because r3.2.5 doesn't have after = FALSE argument
        on.exit(sink(type = 'message'), add = TRUE)
        on.exit(sink(), add = TRUE)
        on.exit(close(log.file), add = TRUE)
      }

      fun(...)
    })
  )

  if (inherits(result, 'try-error')) {
    # try() will not allow the error message to be sinked: so we add it to the log now
    # .makeMessage(result) also works, but adds a space between Error and :
    if (!is.null(LOG.PATH))
      write(sprintf('Error: %s', conditionMessage(attr(result, 'condition'))),
            file = LOG.PATH, append = TRUE)
    result <- NA
  }

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

  invisible(result)
}
