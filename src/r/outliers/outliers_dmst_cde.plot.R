plot.dmst_cde <- function(file) {
  require(ggplot2)

  file.name <- basename(file)

  df_raw <- data.table::fread(file, dec = ",")
  df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                          format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
  df_raw[, SEQUENCE := 1:.N]

  # in case of overlap, render according to this order:
  df_raw <- df_raw[order(factor(DRME_DMST_CDE, levels = c("ENT","INV","DEL","VLD")))]

  ggplot(data = df_raw, mapping = aes(x = if (all(is.na(DRME_OCR_UTC_DTE))) SEQUENCE
                                           else DRME_OCR_UTC_DTE, DRME_DRU)) +
    geom_line() +
    geom_point(mapping = aes(color = DRME_DMST_CDE), show.legend = TRUE) +
    scale_color_manual(values = c("ENT" = "black", "INV" = "red", "DEL" = "magenta", "VLD" = "green")) +
    ggtitle(file.name, subtitle = with(df_raw, {
      freq <- table(DRME_DMST_CDE)
      dupes <- sum(duplicated(DRME_OCR_UTC_DTE, incomparables = NA))
      notime <- sum(is.na(DRME_OCR_UTC_DTE))
      paste(c(names(freq), 'DUPES', 'NOTIME'), c(freq, dupes, notime), sep = '=', collapse = ', ')})) +
    theme(legend.position="bottom", legend.justification = 1) +
    xlab('')
}
