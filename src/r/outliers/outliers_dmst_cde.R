source('outliers/outliers.plot.R')

# ENT (entered) is the basic status meaning that these measurements are used
# in the workflow; DEL (deleted) and INV (invalid) are measurements that were
# visually detected by users as being suspicious, hence those that have to be
# detected by the algorithm.

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_dmst_cde.pdf', width = 14, height = 7, compress = FALSE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
    df_raw <- data.table::fread(f, dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]

    print(with(df_raw, plot.dmst_cde(x = DRME_OCR_UTC_DTE,
                                     y = DRME_DRU,
                                     dmst_cde = DRME_DMST_CDE,
                                     title = basename(f))))
  }
  dev.off()
})
