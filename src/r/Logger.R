# This file makes the Logger object which also has some "static" functions like
# Logger::enumerate().
# First, static functions are made, and then they are exported as namespace.
# Then, Logger object is defined.

# Static functions in Logger namespace -----------------------------------------
if (is.null(namespace::getRegisteredNamespace('Logger'))) namespace::makeNamespace('Logger')
Logger <- namespace::getRegisteredNamespace('Logger')

Logger$enumerate <- function(partner = c('all', 'inbo', 'geotech'), full.path = FALSE) {
  partner <- match.arg(partner)

  inbo <- function() {
    list.files('./../../data/raw/inbo/', recursive = TRUE, full.names = full.path,
               pattern = ".*\\.csv", ignore.case = TRUE)
  }

  geotech <- function() {
    list.files('./../../data/raw/geotechniek/', recursive = TRUE, full.names = full.path,
               pattern = ".*\\.mon", ignore.case = TRUE)
  }

  switch(partner,
         "inbo" = inbo(),
         "geotech" = geotech(),
         c(inbo(), geotech()))
}

base::namespaceExport(Logger, ls(Logger))

# Logger object ----------------------------------------------------------------
Logger <- function(name) {

  readfile.geotech <- function(mov.file) {
    header <- readLines(con = mov.file, n = 100)
    nrowskip <- grep("[Data]", header, fixed = TRUE)
    nlines <- as.integer(header[nrowskip + 1L])
    df <- data.table::fread(mov.file, dec = ".", skip = nrowskip + 1L, nrows = nlines, sep = " ")
    df[, MEASUREMENT_ID := 1:.N]
    df[, TIMESTAMP_UTC := as.POSIXct(paste(V1, V2), format = "%Y/%m/%d %H:%M:%OS", tz = 'UTC')]
    df[, PRESSURE_VALUE := V3]
    df[, PRESSURE_UNIT := "cmH2O"]
    df[, c("V1","V2","V3","V4") := NULL]

    data.table::setkey(df, MEASUREMENT_ID)

    df
  }

  readfile.inbo <- function(csv.file) {
    cols.selected <- c('DRME_ID', 'DRME_OCR_UTC_DTE', 'DRME_DRU')
    df <- data.table::fread(csv.file, dec = ",", select = cols.selected)
    df[, MEASUREMENT_ID := DRME_ID]
    df[, TIMESTAMP_UTC := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                     format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df[, PRESSURE_VALUE := DRME_DRU]
    df[, PRESSURE_UNIT := "cmH2O"]
    df[, (cols.selected) := NULL]

    data.table::setkey(df, MEASUREMENT_ID)

    df
  }

  readfile <- function(filepath) {
    if (grepl(pattern = 'raw/inbo', files)) return(readfile.inbo(filepath))
    if (grepl(pattern = 'raw/geotechniek', files)) return(readfile.geotech(filepath))
  }

  files <- list.files('./../../data/', recursive = TRUE, full.names = TRUE,
                      pattern = name, ignore.case = TRUE)
  if (length(files) > 1L) stop(sprintf("Multiple matches found for %s", name))
  if (length(files) == 0L) stop(sprintf("No data found for logger %s", name))

  df <- readfile(files)

  list(name = name,
       df = data.table::copy(df))
}
