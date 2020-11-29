ref <- sapply(
  tools::file_path_sans_ext(grep('20201103', gwloggeR.data::enumerate(partner = 'knmi'), value = TRUE)),
  function(name) {
    df.ref <- gwloggeR.data::read(name)$df
    list('x' = df.ref[!is.na(PRESSURE_VALUE), PRESSURE_VALUE],
         'timestamps' = df.ref[!is.na(PRESSURE_VALUE), TIMESTAMP_UTC])
  },
  simplify = FALSE, USE.NAMES = TRUE
)

# INBO Barometers --------------------------------------------------------------

sapply(
  grep('barometer', gwloggeR.data::enumerate(partner = 'inbo'), value = TRUE),
  function(f) {
    ROOT.PATH <- './tests/analytics/detect_drift/inbo/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[!is.na(PRESSURE_VALUE),]
    df <- df[order(TIMESTAMP_UTC),]

    if (nrow(df) == 0L) {
      warning(sprintf('Skipping %s: no valid data.', f), call. = FALSE, immediate. = TRUE)
      return(invisible(FALSE))
    }

    gwloggeR:::test.detect_function(
      fun = gwloggeR::detect_drift,
      x = df$PRESSURE_VALUE,
      timestamps = df$TIMESTAMP_UTC,
      reference = ref,
      apriori = gwloggeR::Apriori("air pressure", "cmH2O"),
      alpha = 1,
      verbose = TRUE,
      plot = TRUE,
      title = toupper(f),
      RESULT.PATH = paste0(ROOT.PATH, f, '.result'),
      ATTRIB.PATH = paste0(ROOT.PATH, f, '.attribs'),
      IMG.PATH = paste0(ROOT.PATH, f, '.png'),
      LOG.PATH = paste0(ROOT.PATH, f, '.log')
    )
  }
)
