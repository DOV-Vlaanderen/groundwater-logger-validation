library(gwloggeR)

local({
  print(Sys.time())
  for (f in gwloggeR.data::enumerate('geotech')) {
    print(basename(f))
    df <- gwloggeR.data::read(f)$df

    local({
      png(paste0('./outliers/outliers_v0.02_geotech/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())
      detect_outliers(df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, plot = TRUE, title = f,
                      apriori = apriori("hydrostatic pressure"))
    })
  }
  print(Sys.time())
})
