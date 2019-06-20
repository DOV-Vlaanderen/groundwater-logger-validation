library(ggplot2)

local({
  print(Sys.time())
  folder <- "./../../data/raw/inbo/"
  capture.output(
    for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
      print(basename(f))
      df_raw <- data.table::fread(f, dec = ",")
      df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                              format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]

      df_raw <- df_raw[!is.na(DRME_OCR_UTC_DTE),]

      data.table::setkey(df_raw, DRME_OCR_UTC_DTE)

      df_raw[, DRME_OCR_UTC_DTE_DIFF := as.integer(difftime(DRME_OCR_UTC_DTE,
                                                            data.table::shift(DRME_OCR_UTC_DTE),
                                                            units = 'secs'))]
      cat('Frequency table:')
      print(table(df_raw$DRME_OCR_UTC_DTE_DIFF))
      cat('\n')

    }, file = './exploration/timestamps.txt')
  print(Sys.time())
})

dfa <- data.table::rbindlist(
  use.names = TRUE,
  lapply(list.files("./../../data/raw/inbo/", pattern = ".*\\.csv", full.names = TRUE), function(file) {
    df_raw <- data.table::fread(file, dec = ",")
    df_raw[, FILE := basename(file)]
    df_raw[, N := .N]
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    data.table::setkey(df_raw, DRME_OCR_UTC_DTE)
    df_raw[, DRME_OCR_UTC_DTE_DIFF := as.integer(difftime(DRME_OCR_UTC_DTE,
                                                          data.table::shift(DRME_OCR_UTC_DTE),
                                                          units = 'secs'))]
    df_raw
  })
)

ggplot(data = dfa[, .(.N, 'TIME_DIFF_ENTROPY' = (function(x) {
  p <- table(x, useNA = 'no')/sum(!is.na(x))
  -sum(p*log(p))
})(DRME_OCR_UTC_DTE_DIFF)), by = FILE],
mapping = aes(y = TIME_DIFF_ENTROPY, x = paste(FILE, "-", N))) +
  geom_bar(stat = 'identity') + coord_flip()

ggsave(filename = './exploration/timestamps_diff.png', width = 28, height = 14, dpi = 100)
