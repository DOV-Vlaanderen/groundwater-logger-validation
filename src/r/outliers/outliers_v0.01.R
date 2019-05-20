tmp <- data.table::fread('./../../data/raw/inbo/BAOL001X_D0939.csv', dec = ",")
tmp[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                     format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
tmp[, DRME_DRU_DIFF := DRME_DRU - data.table::shift(DRME_DRU)]

library(ggplot2)
ggplot(data = tmp, mapping = aes(x = DRME_OCR_UTC_DTE, DRME_DRU)) + geom_line() + geom_point()
ggplot(data = tmp, mapping = aes(x = DRME_OCR_UTC_DTE, DRME_DRU_DIFF)) + geom_line() + geom_point()

#' Detects outliers
#'
#' This function is based on Leys, C. e.a., Detecting outliers, 2013.
detect.outliers <- function(y, p.value = 0.0005, verbose = FALSE) {
  M <- median(y, na.rm = TRUE)
  MAD <- mad(y, na.rm = TRUE)
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

qqnorm(tmp$DRME_DRU)
qqline(tmp$DRME_DRU, col = 2)

qqnorm(tmp$DRME_DRU_DIFF)
qqline(tmp$DRME_DRU_DIFF, col = 2)

hist(tmp$DRME_DRU, breaks = 100)
hist(tmp$DRME_DRU_DIFF, breaks = 100)
sum(detect.outliers(tmp$DRME_DRU_DIFF))
tmp[detect.outliers(tmp$DRME_DRU_DIFF),]
ggplot(data = tmp, mapping = aes(x = DRME_OCR_UTC_DTE, DRME_DRU)) + geom_line() +
  geom_point(mapping = aes(color = detect.outliers(DRME_DRU)), show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"))


local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_v0.01.pdf', width = 14, height = 7)
  for (f in list.files(folder, pattern = ".*\\.csv")) {
    df_raw <- data.table::fread(paste0(folder, f), dec = ",")
    df_raw[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                            format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
    df_raw[, SEQUENCE := 1:.N]
    #if (f == "BAOL006X_179843.csv") browser()
    p <- ggplot(data = df_raw, mapping = aes(x = if (all(is.na(DRME_OCR_UTC_DTE))) SEQUENCE
                                                 else DRME_OCR_UTC_DTE, DRME_DRU)) +
      geom_line() +
      geom_point(mapping = aes(color = detect.outliers(DRME_DRU)), show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) + ggtitle(f)

    print(p)
  }
  dev.off()
})

