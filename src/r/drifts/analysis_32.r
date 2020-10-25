# Test for gwloggeR detect_drift() with KNMI as reference.

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

df.ref <- gwloggeR.data::read('KNMI_20200312_hourly')$df

ref <- list(list(x = df.ref$PRESSURE_VALUE, timestamps = df.ref$TIMESTAMP_UTC))

local({
  for (f in  logger.names) {

    print(Sys.time())
    print(basename(f))

    df <- gwloggeR.data::read(f)$df
    data.table::setkey(df, TIMESTAMP_UTC)

    local({

      imagename <- sprintf('./drifts/analysis_32/%s.png', basename(f))
      dir.create(dirname(imagename), showWarnings = FALSE, recursive = TRUE)
      png(imagename, width = 1280, height = 720)

      result <- gwloggeR::detect_drift(
        x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC,
        reference = ref,
        apriori = gwloggeR::Apriori(data_type = 'air pressure', units = 'cmH2O'),
        verbose = TRUE, plot = TRUE, title = paste0(basename(f), ' - v0.01')
      )

      on.exit(dev.off())
    })

  }

  print(Sys.time())
})

