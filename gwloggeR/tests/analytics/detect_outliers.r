# INBO Barometers --------------------------------------------------------------

sapply(
  grep('barometer', gwloggeR.data::enumerate(partner = 'inbo'), value = TRUE),
  function(f) {
    ROOT.PATH <- './tests/analytics/detect_outliers/inbo/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[!is.na(PRESSURE_VALUE),]
    df <- df[order(TIMESTAMP_UTC),]

    if (nrow(df) == 0L) {
      warning(sprintf('Skipping %s: no valid data.', f), call. = FALSE, immediate. = TRUE)
      return(invisible(FALSE))
    }

    gwloggeR:::test.detect_function(
      fun = gwloggeR::detect_outliers,
      x = df$PRESSURE_VALUE,
      apriori = gwloggeR::apriori("air pressure", "cmH2O"),
      timestamps = df$TIMESTAMP_UTC,
      plot = TRUE,
      title = toupper(f),
      verbose = TRUE,
      RESULT.PATH = paste0(ROOT.PATH, f, '.result'),
      ATTRIB.PATH = paste0(ROOT.PATH, f, '.attribs'),
      IMG.PATH = paste0(ROOT.PATH, f, '.png')
    )
  }
)

# INBO Divers ------------------------------------------------------------------

sapply(
  grep('diver', gwloggeR.data::enumerate(partner = 'inbo'), value = TRUE),
  function(f) {
    ROOT.PATH <- './tests/analytics/detect_outliers/inbo/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[order(TIMESTAMP_UTC),]

    gwloggeR:::test.detect_function(
      fun = gwloggeR::detect_outliers,
      x = df$PRESSURE_VALUE,
      apriori = gwloggeR::apriori("hydrostatic pressure", "cmH2O"),
      timestamps = df$TIMESTAMP_UTC,
      plot = TRUE,
      title = toupper(f),
      verbose = TRUE,
      RESULT.PATH = paste0(ROOT.PATH, f, '.result'),
      ATTRIB.PATH = paste0(ROOT.PATH, f, '.attribs'),
      IMG.PATH = paste0(ROOT.PATH, f, '.png')
    )
  }
)

# GEOTECH Divers ---------------------------------------------------------------

sapply(
  gwloggeR.data::enumerate(partner = 'geotech'),
  function(f) {
    ROOT.PATH <- './tests/analytics/detect_outliers/geotech/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[order(TIMESTAMP_UTC),]

    gwloggeR:::test.detect_function(
      fun = gwloggeR::detect_outliers,
      x = df$PRESSURE_VALUE,
      apriori = gwloggeR::apriori("hydrostatic pressure", "cmH2O"),
      timestamps = df$TIMESTAMP_UTC,
      plot = TRUE,
      title = toupper(f),
      verbose = TRUE,
      RESULT.PATH = paste0(ROOT.PATH, f, '.result'),
      ATTRIB.PATH = paste0(ROOT.PATH, f, '.attribs'),
      IMG.PATH = paste0(ROOT.PATH, f, '.png')
    )
  }
)

