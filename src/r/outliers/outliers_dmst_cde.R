library(ggplot2)

# ENT (entered) is the basic status meaning that these measurements are used
# in the workflow; DEL (deleted) and INV (invalid) are measurements that were
# visually detected by users as being suspicious, hence those that have to be
# detected by the algorithm.

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_dmst_cde.pdf', width = 14, height = 7)
  for (f in list.files(folder, pattern = ".*\\.csv")) {
    df_raw <- data.table::fread(paste0(folder, f), dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df_raw[, SEQUENCE := 1:.N]

    # in case of overlap, render according to this order:
    df_raw <- df_raw[order(factor(DRME_DMST_CDE, levels = c("ENT","INV","DEL","VLD")))]

    p <- ggplot(data = df_raw, mapping = aes(x = if (all(is.na(DRME_OCR_UTC_DTE))) SEQUENCE
                                                 else DRME_OCR_UTC_DTE, DRME_DRU)) +
      geom_line() +
      geom_point(mapping = aes(color = DRME_DMST_CDE), show.legend = TRUE) +
      scale_color_manual(values = c("ENT" = "black", "INV" = "red", "DEL" = "magenta", "VLD" = "green")) +
      ggtitle(f, subtitle = with(df_raw, {
        freq <- table(DRME_DMST_CDE)
        dupes <- sum(duplicated(DRME_OCR_UTC_DTE, incomparables = NA))
        paste(c(names(freq), 'DUPES'), c(freq, dupes), sep = '=', collapse = ', ')})) +
      theme(legend.position="bottom", legend.justification = 1) +
      xlab('')

    print(p)
  }
  dev.off()
})

