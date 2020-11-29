# Analysis of barometers based on the KNMI series v01.

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
  df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'logger.name' = logger.name)
}

data.ref <- read.baro('KNMI_20200312_hourly')
with(data.ref, plot(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, type = 'l'))

compare <- function(df, df.ref, make.plot = TRUE) {
  if (nrow(df) == 0L || nrow(df.ref) == 0L) return(data.table::data.table())
  df <- data.table::copy(df)
  df.ref <- data.table::copy(df.ref)

  # Center based on median
  df[, 'PRESSURE_VALUE' := PRESSURE_VALUE - median(PRESSURE_VALUE)]
  df.ref[, 'PRESSURE_VALUE' := PRESSURE_VALUE - median(PRESSURE_VALUE)]

  df.diff <- df[J(df.ref), .(
    'PRESSURE_VALUE' = x.PRESSURE_VALUE,
    'PRESSURE_VALUE_REF' = i.PRESSURE_VALUE,
    'PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE,
    TIMESTAMP_UTC)
    ][!is.na(PRESSURE_DIFF), ]

  if (nrow(df.diff) < 100L) return(data.table::data.table())

  wiskers <- boxplot.stats(df.diff[, PRESSURE_DIFF])$stats[c(1, 5)]
  wisker.diff <- wiskers[2L] - wiskers[1L]
  stats <- sprintf(paste0(
    'IRQ: %0.3f\n',
    'WISK: %0.3f\n',
    '95 %% RANGE: %0.3f\n',
    'SD: %0.3f'
    ),
    IQR(df.diff[, PRESSURE_DIFF]),
    wisker.diff,
    quantile(df.diff[, PRESSURE_DIFF], 0.975) - quantile(df.diff[, PRESSURE_DIFF], 0.025),
    sd(df.diff[, PRESSURE_DIFF])
  )

  p.both <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = PRESSURE_VALUE_REF), col = 'red') +
    ggplot2::geom_line(mapping = ggplot2::aes(y = PRESSURE_VALUE), col = 'black') +
    ggplot2::coord_cartesian(ylim = quantile(with(df.diff, c(PRESSURE_VALUE, PRESSURE_VALUE_REF)),
                                             probs = c(0.001, 0.999))) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  p.diff <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC)) +
    ggplot2::geom_hline(yintercept = median(df.diff[1L:as.integer(.N*0.25), PRESSURE_DIFF]), alpha = 0.5) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = PRESSURE_DIFF), col = 'black') +
    ggplot2::coord_cartesian(ylim = quantile(with(df.diff, PRESSURE_DIFF),
                                             probs = c(0.001, 0.999))) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  binwidth <- 1.5 * IQR(df.diff$PRESSURE_DIFF) / nrow(df.diff) ^ (1/3)
  p.hist <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = PRESSURE_DIFF)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = 'black') +
    ggplot2::theme_light() +
    ggplot2::coord_cartesian(xlim = quantile(with(df.diff, PRESSURE_DIFF),
                                             probs = c(0.001, 0.999))) +
    ggplot2::ylab('FREQUENCY')

  p.txt <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, size = 8, label = stats) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  layout_matrix <- rbind(c(1, 1, 1, 4),
                         c(2, 2, 2, 3))

  grob.title <- grid::textGrob(sprintf('%s (#%s observations compared with %s)',
                                       basename(attr(df, 'logger.name')),
                                       nrow(df.diff),
                                       basename(attr(df.ref, 'logger.name'))),
                               x = 0.05, hjust = 0)

  filename <- sprintf('./drifts/analysis_12/WISK%08.3f_%s.png', wisker.diff,
                      tools::file_path_sans_ext(basename(basename(attr(df, 'logger.name')))))

  local({
    if (!make.plot) return()
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(gridExtra::grid.arrange(p.both, p.diff, p.hist, p.txt,
                                  layout_matrix = layout_matrix,
                                  top = grob.title))
  })

  structure(df.diff, 'logger.name' = attr(df, 'logger.name'))
}

#compare(read.baro(logger.names[1]),  data.ref)
results <- lapply(logger.names, function(name) compare(read.baro(name), data.ref))


