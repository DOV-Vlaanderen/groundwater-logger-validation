# Monthly averages of KNMI air pressure
# To compare with:
# https://github.com/DOV-Vlaanderen/groundwater-logger-validation/issues/61#issuecomment-708963119

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  #as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
  ts
}

read.baro <- function(logger.name) {
  df <- gwloggeR.data::read(logger.name)$df
  if (nrow(df) == 0L) return(df)
  df <- df[!is.na(TIMESTAMP_UTC), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)
  data.table::setattr(df, 'logger.name', logger.name)

  df
}

df <- read.baro('KNMI_20200312_hourly')
df[, YEAR_MONTH := as.POSIXct(format(TIMESTAMP_UTC, '%Y-%m-01 00:00:00'))]

df.monthly <- df[, .(PRESSURE_VALUE = mean(PRESSURE_VALUE)), by = YEAR_MONTH]
df.monthly[, MONTH := as.integer(data.table::month(YEAR_MONTH))]
df.monthly

ggplot2::ggplot(data = df.monthly, mapping = ggplot2::aes(x = MONTH, y = PRESSURE_VALUE)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::ggtitle('KNMI data: monthly averaged observations') +
  ggplot2::scale_x_continuous(breaks = 1:12)
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_31/knmi_monthly_averages.png', width = 1280/96, height = 720/96, dpi = 96)
