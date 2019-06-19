source("outliers/outliers.plot.R")

local({
  print(Sys.time())
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/tsoutliers.pdf', width = 14, height = 7, compress = FALSE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
    print(basename(f))
    df_raw <- data.table::fread(f, dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]


    df_raw <- df_raw[!is.na(DRME_OCR_UTC_DTE),][order(DRME_OCR_UTC_DTE),]

    N <- nrow(df_raw)
    if (N <= 1L || N > 7000L) next()

    tso <- tsoutliers::tso(y = ts(df_raw[,DRME_DRU]),
                           types = 'AO',
                           cval = qnorm(p = (1 - 0.0005)^(1 / (N*2))))

    print(with(df_raw, plot.outliers(x = DRME_OCR_UTC_DTE,
                                     y = DRME_DRU,
                                     outliers = replace(rep(FALSE, N), tso$outliers$ind, TRUE),
                                     title = basename(f),
                                     caption = forecast:::arima.string(tso$fit),
                                     add.diff = TRUE)))
  }
  dev.off()
  print(Sys.time())
})

