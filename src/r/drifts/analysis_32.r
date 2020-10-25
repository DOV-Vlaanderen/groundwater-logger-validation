# Test for gwloggeR detect_drift() with KNMI as reference.

logger.names <- tools::file_path_sans_ext(grep('barometer/', gwloggeR.data::enumerate(), value = TRUE))
#logger.names <- 'BAOL079X_177936.csv' # all timestamps are NA
#logger.names <- 'BAOL528X_B_B2152.csv' # has less than 5 matching observations with KNMI data
#logger.names <- 'BAOL544X_B_002A6.csv' # has NA values in x

df.ref <- gwloggeR.data::read('KNMI_20200312_hourly')$df

ref <- list(list(x = df.ref$PRESSURE_VALUE, timestamps = df.ref$TIMESTAMP_UTC))

local({

  for (f in  logger.names) {

    print(Sys.time())
    print(basename(f))

    ROOT.PATH <- './drifts/analysis_32/'

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]
    df <- df[!is.na(PRESSURE_VALUE),]
    data.table::setkey(df, TIMESTAMP_UTC)

    if (nrow(df) == 0L) {
      print('Skipping...')
      next()
    }

    gwloggeR:::test.detect_function(
      fun = gwloggeR::detect_drift,
      x = df$PRESSURE_VALUE,
      timestamps = df$TIMESTAMP_UTC,
      reference = ref,
      apriori = gwloggeR::Apriori(data_type = 'air pressure', units = 'cmH2O'),
      verbose = TRUE,
      plot = TRUE,
      title = paste0(basename(f), ' - v0.01'),
      RESULT.PATH = paste0(ROOT.PATH, basename(f), '.result'),
      ATTRIB.PATH = paste0(ROOT.PATH, basename(f), '.attribs'),
      IMG.PATH = paste0(ROOT.PATH, basename(f), '.png')
    )

  }

  print(Sys.time())

})
