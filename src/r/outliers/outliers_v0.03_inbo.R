library(gwloggeR)

local({
  print(Sys.time())
  for (f in Logger::enumerate(partner = 'inbo')) {
    print(basename(f))
    df <- Logger(f)$df

    point_sample_type <- substr(basename(f), 4L, 4L)
    ap <- switch (point_sample_type,
                  "L" = apriori("air pressure", "cmH2O"),
                  "P" = apriori("hydrostatic pressure", "cmH2O"),
                  "S" = apriori("hydrostatic pressure", "cmH2O")
    )

    local({
      png(paste0('./outliers/outliers_v0.03_inbo/', basename(f), '.png'), width = 1920, height = 1080)
      on.exit(dev.off())
      detect_outliers(df$PRESSURE_VALUE, apriori = ap,
                      timestamps = df$TIMESTAMP_UTC,
                      plot = TRUE, title = paste0(basename(f), ' - v0.03'))
    })
  }
  print(Sys.time())
})
