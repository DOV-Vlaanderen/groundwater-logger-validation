# INBO Divers ------------------------------------------------------------------

sapply(
  grep('diver', gwloggeR.data::enumerate(partner = 'inbo'), value = TRUE),
  function(f) {
    ROOT.PATH <- './tests/analytics/detect_levelshifts/inbo/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[order(TIMESTAMP_UTC),]

    test_detect_function(
      fun = gwloggeR::detect_levelshifts,
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
    ROOT.PATH <- './tests/analytics/detect_levelshifts/geotech/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[order(TIMESTAMP_UTC),]

    test_detect_function(
      fun = gwloggeR::detect_levelshifts,
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
