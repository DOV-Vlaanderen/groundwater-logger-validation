library(gwloggeR)

local({
  print(Sys.time())
  for (f in gwloggeR.data::enumerate('inbo')) {
    print(basename(f))
    df <- gwloggeR.data::read(f)$df

    local({
      png(paste0('./outliers/outliers_v0.01_inbo/', basename(f), '.png'), width = 1920, height = 1080)
      on.exit(dev.off())
      detect_outliers(df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC,
                      plot = TRUE, title = paste0(basename(f), ' - v0.01'))
    })
  }
  print(Sys.time())
})
