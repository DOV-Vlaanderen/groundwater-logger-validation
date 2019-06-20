source("outliers/outliers.plot.R")

library(gwloggeR)

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/tsoutliers_comparison.pdf', width = 14, height = 7, compress = TRUE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
    print(basename(f))
    df <- data.table::fread(f, dec = ",")
    df[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                        format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]

    point_sample_type <- substr(basename(f), 4L, 4L)
    ap <- switch (point_sample_type,
                  "L" = apriori("air pressure", "cmH2O"),
                  "P" = apriori("diver", "cmH2O"),
                  "S" = apriori("diver", "cmH2O")
    )

    p_dmst_cde <- with(df, plot.dmst_cde(x = DRME_OCR_UTC_DTE,
                                         y = DRME_DRU,
                                         dmst_cde = DRME_DMST_CDE,
                                         title = basename(f)))

    p_v0.01 <- with(df, plot.outliers(x = DRME_OCR_UTC_DTE,
                                      y = DRME_DRU,
                                      outliers = detect_outliers(DRME_DRU),
                                      title = paste(basename(f), "(v0.01)")))

    p_v0.02 <- with(df, plot.outliers(x = DRME_OCR_UTC_DTE,
                                      y = DRME_DRU,
                                      outliers = detect_outliers(DRME_DRU, apriori = ap),
                                      title = paste(basename(f), "(v0.02)")))

    df <- df[!is.na(DRME_OCR_UTC_DTE),][order(DRME_OCR_UTC_DTE),]
    N <- nrow(df)
    if (N <= 1L || N > 7000L) p_tsoutliers <- ggplot2::ggplot()
    else {
      tso <- tsoutliers::tso(y = ts(df[,DRME_DRU]),
                             types = 'AO',
                             cval = qnorm(p = (1 - 0.0005)^(1 / (N*2))))

      p_tsoutliers <- with(df, plot.outliers(x = DRME_OCR_UTC_DTE,
                                             y = DRME_DRU,
                                             outliers = replace(rep(FALSE, N), tso$outliers$ind, TRUE),
                                             title = basename(f),
                                             caption = forecast:::arima.string(tso$fit),
                                             add.diff = TRUE))
    }

    p <- gridExtra::grid.arrange(p_dmst_cde, p_v0.01, p_v0.02, p_tsoutliers)
    print(p)
  }
  dev.off()
})
