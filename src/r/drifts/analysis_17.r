# WMO papers on air pressure reduction suggest an influence of temperature
# change on air pressure. This analysis is meant to verify that.

logger.names <- grep('barodata/', gwloggeR.data::enumerate(), value = TRUE)

logger.names <- setdiff(logger.names, 'barodata/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barodata/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

read.baro <- function(logger.name) {
  df <- gwloggeR.data::read(logger.name)$df

  df <- df[!is.na(TIMESTAMP_UTC), ]
  df <- df[!is.na(TEMPERATURE_VALUE), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!duplicated(TIMESTAMP_UTC), ]

  if (nrow(df) == 0L) return(df)

  # df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE),
  #              'TEMPERATURE_VALUE' = mean(TEMPERATURE_VALUE)),
  #          by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]

  # Filter
  df <- df[PRESSURE_VALUE < 1100,]
  df <- df[PRESSURE_VALUE > 975,]

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)

  # Changes in TEMP and PRESSURE
  df[, 'TEMPERATURE_VALUE_DIFF' := TEMPERATURE_VALUE - data.table::shift(TEMPERATURE_VALUE)]
  df[, 'PRESSURE_VALUE_DIFF' := PRESSURE_VALUE - data.table::shift(PRESSURE_VALUE)]
  df[, 'TIMESTAMP_DIFF_SEC' := as.numeric(TIMESTAMP_UTC - data.table::shift(TIMESTAMP_UTC), units = 'secs')]
  df <- df[-1L, ]

  data.table::setkey(df, TIMESTAMP_UTC)

  structure(df, 'logger.name' = logger.name)
}

df.list <- sapply(logger.names, read.baro)

df <- data.table::rbindlist(df.list, use.names = TRUE, fill = TRUE)
df <- df[TIMESTAMP_DIFF_SEC <= 12*3600]
df <- df[abs(TEMPERATURE_VALUE_DIFF) <= 70]
df <- df[abs(PRESSURE_VALUE_DIFF) <= 100]

ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TEMPERATURE_VALUE_DIFF, y = PRESSURE_VALUE_DIFF)) +
  ggplot2::geom_point(pch = '.') +
  #ggplot2::stat_smooth(method = 'gam', color = 'yellow', se = FALSE) +
  #ggplot2::stat_smooth(method = 'gam', formula = y ~ s(x, k = 5), color = 'yellow', se = FALSE) +
  #ggplot2::geom_smooth(method = 'lm', formula = y ~ x + I(x^2), col = 'yellow', size = 1.2, alpha = 0.9) +
  ggplot2::geom_quantile(quantiles = 0.5, formula = y ~ x + I(x^2), col = 'blue', size = 1.2, alpha = 0.9) +
  ggplot2::coord_cartesian(xlim = quantile(df$TEMPERATURE_VALUE_DIFF, probs = c(0.0001, 0.9999)),
                           ylim = quantile(df$PRESSURE_VALUE_DIFF, probs = c(0.0001, 0.9999))) +
  ggplot2::theme_light()

summary((fit.lm <- lm(data = df, formula = PRESSURE_VALUE_DIFF ~ TEMPERATURE_VALUE_DIFF + I(TEMPERATURE_VALUE_DIFF^2))))
summary((fit.rq <- quantreg::rq(data = df, formula = PRESSURE_VALUE_DIFF ~ TEMPERATURE_VALUE_DIFF +  + I(TEMPERATURE_VALUE_DIFF^2))))
