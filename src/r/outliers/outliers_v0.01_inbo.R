library(gwloggeR)

source("outliers/outliers.plot.R")

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_v0.01.pdf', width = 14, height = 7, compress = FALSE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
    print(basename(f))
    df_raw <- data.table::fread(f, dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]

    print(with(df_raw, plot.outliers(x = DRME_OCR_UTC_DTE,
                                     y = DRME_DRU,
                                     outliers = detect_outliers(DRME_DRU),
                                     title = basename(f))))
  }
  dev.off()
})

