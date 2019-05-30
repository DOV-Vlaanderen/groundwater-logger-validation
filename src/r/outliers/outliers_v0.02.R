library(ggplot2)

#' Detects outliers
#'
#' This function is based on Leys, C. e.a., Detecting outliers, 2013.
detect.outliers <- function(y, p.value = 0.0005, verbose = FALSE, M, MAD) {
  if (is.na(MAD) | MAD == 0) {
    return(rep(FALSE, length(y)))
  }
  # sigma.reject is the sigma after which we reject points. The 0.0005 means that
  # in 1 of 5000 calculations, we will reject a value we shouldn't have.
  sigma.reject <- qnorm(p = (1 - p.value)^(1 / length(y)))
  y.rejects <- abs((y - M) / MAD) > sigma.reject
  y.rejects[which(is.na(y.rejects))] <- FALSE # NA's are no outliers
  if (verbose) {
    print(sprintf(
      "M: %.3f | MAD: %.3f | sigma.reject: %.3f | # rejects: %i | sd: %.3f | sd.clean: %.3f",
      M, MAD, sigma.reject, sum(y.rejects, na.rm = TRUE),
      sqrt(var(y, na.rm = TRUE)), sqrt(var(y[!y.rejects], na.rm = TRUE))
    ))
  }
  y.rejects
}

# load all data for M and MAD computation
df <- data.table::rbindlist(
  use.names = TRUE,
  lapply(list.files("./../../data/raw/inbo/", pattern = "BAOL.*\\.csv", full.names = TRUE), function(file) {
    # if (grepl("BAOL", file)) return(data.table::data.table()) #excluded
    df_raw <- data.table::fread(file, dec = ",")
    df_raw <- df_raw[DRME_DRU > 500,]
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


M <- median(df$DRME_DRU)
MAD <- mad(df$DRME_DRU)

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_v0.02.pdf', width = 14, height = 7)
  for (f in list.files(folder, pattern = "BAOL.*\\.csv")) {
    df_raw <- data.table::fread(paste0(folder, f), dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df_raw[, SEQUENCE := 1:.N]
    #if (f == "BAOL006X_179843.csv") browser()
    p <- ggplot(data = df_raw, mapping = aes(x = if (all(is.na(DRME_OCR_UTC_DTE))) SEQUENCE
                                                 else DRME_OCR_UTC_DTE, DRME_DRU)) +
      geom_line() +
      geom_point(mapping = aes(color = detect.outliers(DRME_DRU, M = M, MAD = MAD)), show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) + ggtitle(f)

    print(p)
  }
  dev.off()
})

