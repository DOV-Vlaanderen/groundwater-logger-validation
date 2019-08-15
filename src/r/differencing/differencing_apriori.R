source('./differencing/functions.R')

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
                      'pp09-2_101104150902_F8511')

df.list <- sapply(logger_selection, function(name) Logger(name)$df, simplify = FALSE, USE.NAMES = TRUE)

saveRDS(df.list, './differencing/apriori_hydrostatic_pressure_data_selection.rds')

differentiate(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 1800, diag_plots = TRUE)

tmp <- differentiate(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 2700)
tmp2 <- differentiate2(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 2700)
identical(tmp, tmp2)

tdiffs.sec <- c(5, 10, 15, 30, 45, 60, 120, 240, 360, 720, 1080, 1440, 2880)*60 # sec
#set.seed(2019)
#tdiffs.sec <- sample(seq(5, 2880, by = 5), 200)*60

df.diffs <- list()
df.diffs[[as.character(30*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 30*60, simplify = FALSE, USE.NAMES = TRUE)
df.diffs[[as.character(5*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 5*60, simplify = FALSE, USE.NAMES = TRUE)
df.diffs[[as.character(1440*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 1440*60, simplify = FALSE, USE.NAMES = TRUE)

#sapply(df.diffs[['300']], function(diffs) if (!is.null(diffs)) plot.hist(diffs))

tmp3 <- unlist(unname(df.diffs[['86400']]))
plot.hist(tmp3)


local({
  print(Sys.time())
  cl <- parallel::makeCluster(7, outfile = './log/parallel.log')
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl = cl, varlist = c('differentiate2', 'diffx2', 'df.list'))

  df.diffs <<- parallel::clusterApplyLB(cl = cl, x = tdiffs.sec, fun = function(interval.sec) {
    sapply(df.list, FUN = differentiate2, interval.sec = interval.sec, simplify = FALSE, USE.NAMES = TRUE)
  })
  names(df.diffs) <<- tdiffs.sec

  print(Sys.time())
})

df <- data.table::rbindlist(lapply(names(df.diffs), function(tdiff.name) {
  data.table::data.table('TIMEDIFF' = as.numeric(tdiff.name),
                         'VALUE' = unlist(unname(df.diffs[[tdiff.name]])))
}), use.names = TRUE, idcol = FALSE)

ggplot2::ggplot(data = df[, .(MEDIAN = median(abs(VALUE)),
                              N = length(VALUE),
                              Q.025 = quantile(abs(VALUE), 0.025),
                              Q.975 = quantile(abs(VALUE), 0.975),
                              Q.99 = quantile(abs(VALUE), 0.99)),
                          by = TIMEDIFF/60/60],
                mapping = ggplot2::aes(x = TIMEDIFF)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = MEDIAN)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = Q.975), col = 'red') +
  ggplot2::ylab('VALUEDIFF')


names(df.diffs)
saveRDS(df.diffs, './differencing/df.diffs.rds')
