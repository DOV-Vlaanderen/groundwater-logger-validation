#' @keywords internal

validate.logger.root.path <- function() {
  if (is.null(getOption("logger.root.data.path"))) {
    warning('WARNING: logger.root.data.path option not set. Using default.')
    options(logger.root.data.path = './../data/raw')
  }
}

#' @title Enumerate logger files
#' @export
enumerate <- function(partner = c('all', 'inbo', 'geotech'), full.path = FALSE) {
  partner <- match.arg(partner)
  validate.logger.root.path()

  inbo <- function() {
    list.files(paste0(getOption("logger.root.data.path"), '/inbo/'),
               recursive = TRUE, full.names = full.path,
               pattern = ".*\\.csv", ignore.case = TRUE)
  }

  geotech <- function() {
    list.files(paste0(getOption("logger.root.data.path"), '/geotechniek/'),
               recursive = TRUE, full.names = full.path,
               pattern = ".*\\.mon", ignore.case = TRUE)
  }

  switch(partner,
         "inbo" = inbo(),
         "geotech" = geotech(),
         c(inbo(), geotech()))
}

#' @title Logger object
#' @description Logger object is a uniform way of loading data from various formats.
#' @importFrom data.table :=
#' @keywords internal

Logger <- function(name) {

  validate.logger.root.path()

  readfile.geotech <- function(mov.file) {
    header <- readLines(con = mov.file, n = 100)
    nrowskip <- grep("[Data]", header, fixed = TRUE)
    nlines <- as.integer(header[nrowskip + 1L])
    df <- data.table::fread(mov.file, dec = ".", skip = nrowskip + 1L, nrows = nlines, sep = " ")
    df[, MEASUREMENT_ID := 1:.N]
    df[, TIMESTAMP_UTC := as.POSIXct(paste(V1, V2),
                                     tryFormats = c("%Y/%m/%d %H:%M:%OS",
                                                    "%Y-%m-%d %H:%M:%OS"),
                                     tz = 'UTC')]
    df[, PRESSURE_VALUE := V3]
    df[, PRESSURE_UNIT := "cmH2O"]
    df[, TEMPERATURE_VALUE := V4]
    df[, TEMPERATURE_UNIT := "°C"]
    df[, c("V1","V2","V3","V4") := NULL]

    data.table::setkey(df, MEASUREMENT_ID)

    df
  }

  readfile.inbo <- function(csv.file) {
    cols.selected <- c('DRME_ID', 'DRME_OCR_UTC_DTE', 'DRME_DRU', 'DRME_TPU')
    df <- data.table::fread(csv.file, dec = ",", select = cols.selected)
    df[, MEASUREMENT_ID := DRME_ID]
    df[, TIMESTAMP_UTC := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                     format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df[, PRESSURE_VALUE := DRME_DRU]
    df[, PRESSURE_UNIT := "cmH2O"]
    df[, TEMPERATURE_VALUE := DRME_TPU]
    df[, TEMPERATURE_UNIT := "°C"]
    df[, (cols.selected) := NULL]

    data.table::setkey(df, MEASUREMENT_ID)

    df
  }

  readfile <- function(filepath) {
    if (grepl(pattern = '/inbo', files)) return(readfile.inbo(filepath))
    if (grepl(pattern = '/geotechniek', files)) return(readfile.geotech(filepath))
  }

  files <- list.files(getOption("logger.root.data.path"), recursive = TRUE, full.names = TRUE)
  files <- grep(pattern = name, x = files, value = TRUE, ignore.case = TRUE)

  if (length(files) > 1L) stop(sprintf("Multiple matches found for %s", name))
  if (length(files) == 0L) stop(sprintf("No data found for logger %s", name))

  df <- readfile(files)

  list(name = basename(files),
       df = data.table::copy(df))
}

#' @title Reads logger data and returns a Logger object.
#' @export

read <- function(name) {
  Logger(name)
}
