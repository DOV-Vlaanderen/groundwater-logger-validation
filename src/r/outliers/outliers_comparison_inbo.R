source("outliers/outliers.plot.R")

library(gwloggeR)

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_comparison.pdf', width = 14, height = 7, compress = TRUE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
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

    p <- gridExtra::grid.arrange(p_dmst_cde, p_v0.01, p_v0.02)
    print(p)
  }
  dev.off()
})
