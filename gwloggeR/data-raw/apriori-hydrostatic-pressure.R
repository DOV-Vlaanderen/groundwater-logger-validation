## code to prepare `apriori-hydrostatic-pressure` dataset goes here

options(logger.root.data.path = './../data/raw')
devtools::load_all('./../gwloggeR.data', export_all = FALSE)
detach('package:gwloggeR.data')

logger_selection <- c('born_pb 112o_151202090851_T4179',
                      'born_pb94d_170124133301_T4175',
                      'born_pb94d_171009123814_T4175',
                      'born_pb94d_171213132622_T4175',
                      'born_pb94d_180213134811_T4175',
                      'born_pb94d_180315155306_T4175',
                      'born_pb94o_170403160106_T4184',
                      'born_pb94o_170623135603_T4184',
                      'born_pb94o_170727122028_T4184',
                      'born_pb94o_171009123814_T4184',
                      'born_pb94o_171213132623_T4184',
                      'born_pb94o_180111133149_T4184',
                      'born_pb94o_180315155143_T4184',
                      'born_pb112d_171009123816_T4180',
                      'born_pb112d_180111134538_T4180',
                      'born_pb112d_180213134811_T4180',
                      'born_pb112d_180315160037_T4180',
                      'born_pb112o_170623134158_T4179',
                      'born_pb112o_170727122029_T4179',
                      'born_pb112o_170829130424_T4179',
                      'born_pb112o_171009123815_T4179',
                      'born_pb112o_171213132623_T4179',
                      'born_pb112o_180213134812_T4179',
                      'ff_b29d_090429160100_C4080',
                      'ff_b51d_090305163025_C4089',
                      'pp01-1_101104155528_F5468',
                      'pp09-1_101104150356_F5474',
                      'pp09-2_101104150902_F8511',
                      'DYLP023A_S3804' # small piece of 1 min data
                      )

hydropressure <- lapply(logger_selection, function(file.name) {
  df <- gwloggeR.data::read(file.name)$df

  data.table::setkey(df, TIMESTAMP_UTC)

  # 1 min data
  if (file.name == 'DYLP023A_S3804')
    df <- df[TIMESTAMP_UTC >= '2018-03-16 06:59:00' & TIMESTAMP_UTC <= '2018-03-17 11:59:00',]

  # No NA timestamps allowed
  if (any(is.na(df[, TIMESTAMP_UTC])))
    stop('ERROR: all timestamps must be non-NA.')

  # Equal spacing is not a requirement, but it is
  # exploited to speed up sample generation of differences.
  if (length(unique(as.numeric(diff(df[, TIMESTAMP_UTC]), 'secs'))) != 1L)
    stop('ERROR: all differences must be equaly spaced.')

  # Meta data
  df[, FILE := basename(file.name)]
  df[, N := .N]

  data.table::setkey(df, TIMESTAMP_UTC)

  df
})

# intervals.sec table
table(sapply(hydropressure, function(df) unique(as.numeric(diff(df$TIMESTAMP_UTC), units = 'secs'))))

# plots for reference
local({
  pdf('./data-raw/apriori-hydrostatic-pressure.pdf', width = 14, height = 6)
  on.exit(dev.off())

  invisible(lapply(hydropressure, function(df) {
    print(ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x = "TIMESTAMP_UTC", y = "PRESSURE_VALUE")) +
            ggplot2::geom_line(alpha = .2) + ggplot2::geom_point(shape = 16, size = 0.01) +
            ggplot2::ylab('x') + ggplot2::xlab('timestamp') +
            ggplot2::ggtitle(paste(df$FILE[1L], ', DIFF.SEC = ', as.numeric(diff(df$TIMESTAMP_UTC), units = 'secs'))) +
            ggplot2::theme_light())
  }))
})

hydropressure <- data.table::rbindlist(hydropressure, use.names = TRUE)
data.table::setkey(hydropressure, FILE, TIMESTAMP_UTC)
