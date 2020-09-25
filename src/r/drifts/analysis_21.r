# Availability of barometers

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

read.baro <- function(logger.name) {
  df <- gwloggeR.data::read(logger.name)$df
  if (nrow(df) == 0L) return(df)
  df <- df[!is.na(TIMESTAMP_UTC), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!duplicated(TIMESTAMP_UTC), ]

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)
  data.table::setattr(df, 'logger.name', logger.name)

  df
}

df.list <- lapply(logger.names, read.baro)
df <- data.table::rbindlist(df.list, use.names = TRUE, fill = TRUE)
data.table::setkey(df, FILE, TIMESTAMP_UTC)
df

dir.create('./drifts/analysis_21', showWarnings = FALSE, recursive = TRUE)

ggplot2::ggplot() +
  ggplot2::geom_line(
    data = df[,.(N = length(unique(FILE))), keyby = .(TIMESTAMP_UTC = round_timestamp(TIMESTAMP_UTC, scalefactor.sec = 3600*12))],
    mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = N)
  ) +
  ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(n = 20)) +
  ggplot2::xlab('TIMESTAMP_UTC (grouped per 12h)') +
  ggplot2::ggtitle('Number of barometers available at 24h intervals') +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_21/baro_counts_12hinterval.png', width = 1280/96, height = 720/96, dpi = 96)

ggplot2::ggplot() +
  ggplot2::geom_line(
    data = df[,.(N = length(unique(FILE))), keyby = .(TIMESTAMP_UTC = round_timestamp(TIMESTAMP_UTC, scalefactor.sec = 3600*24))],
    mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = N)
  ) +
  ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(n = 20)) +
  ggplot2::xlab('TIMESTAMP_UTC (grouped per 24h)') +
  ggplot2::ggtitle('Number of barometers available at 24h intervals') +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_21/baro_counts_24hinterval.png', width = 1280/96, height = 720/96, dpi = 96)


ggplot2::ggplot() +
  ggplot2::geom_line(
    data = df[TIMESTAMP_UTC >= as.POSIXct('2019-01-01'),
              .(N = length(unique(FILE))),
              keyby = .(TIMESTAMP_UTC = round_timestamp(TIMESTAMP_UTC, scalefactor.sec = 3600*24))],
    mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = N)
  ) +
  ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(n = 20)) +
  ggplot2::xlab('TIMESTAMP_UTC (grouped per 24h)') +
  ggplot2::ggtitle('Number of barometers available at 24h intervals') +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_21/baro_counts_24hinterval_2019-2020.png', width = 1280/96, height = 720/96, dpi = 96)
