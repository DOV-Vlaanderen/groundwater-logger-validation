library(gwloggeR)

# load all data for M and MAD computation
df <- data.table::rbindlist(
  use.names = TRUE,
  lapply(list.files("./../../data/raw/inbo/", pattern = "BAOL.*\\.csv", full.names = TRUE), function(file) {
    if (grepl("BAOL089|BAOL079|BAOL006", file)) return(data.table::data.table()) #excluded
    df_raw <- data.table::fread(file, dec = ",")
    df_raw <- df_raw[DRME_DRU > 800,]
    df_raw <- df_raw[DRME_DRU < 1200,]
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df_raw[, DRME_DRU_DIFF := DRME_DRU - data.table::shift(DRME_DRU)]
    df_raw
  })
)

with(df, {
  hist(DRME_DRU, breaks = 1000, probability = TRUE, main = '')
  curve(dnorm(x, mean = mean(DRME_DRU), sd = sqrt(var(DRME_DRU))), add = TRUE, col = 'red')
})
with(df, {
  qqnorm(DRME_DRU)
  qqline(DRME_DRU)
})

median(df$DRME_DRU)
mad(df$DRME_DRU)^2
apriori("air pressure", "cmH2O")

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
      png(paste0('./outliers/outliers_v0.02_inbo/', basename(f), '.png'), width = 1920, height = 1080)
      on.exit(dev.off())
      detect_outliers(df$PRESSURE_VALUE, apriori = ap,
                      timestamps = if (!all(is.na(df$TIMESTAMP_UTC))) df$TIMESTAMP_UTC,
                      plot = TRUE, title = paste0(basename(f), ' - v0.02'))
    })
  }
  print(Sys.time())
})
