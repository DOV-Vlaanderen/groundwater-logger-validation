library(gwloggeR)

local({
  print(Sys.time())
  for (f in Logger::enumerate('inbo')) {
    print(basename(f))
    df <- Logger(f)$df

    local({
      png(paste0('./outliers/outliers_v0.01_inbo/', basename(f), '.png'), width = 1920, height = 1080)
      on.exit(dev.off())
      detect_outliers(df$PRESSURE_VALUE, timestamps = if (!all(is.na(df$TIMESTAMP_UTC))) df$TIMESTAMP_UTC,
                      plot = TRUE, title = basename(f))
    })
  }
  print(Sys.time())
})
