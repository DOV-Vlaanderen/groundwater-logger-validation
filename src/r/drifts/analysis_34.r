# Test for gwloggeR detect_drift() with multiple KNMI series as reference.
# This analysis is based on 32 where only 1 reference is used.

logger.names <- tools::file_path_sans_ext(grep('barometer/', gwloggeR.data::enumerate(), value = TRUE))
ref.logger.names <- tools::file_path_sans_ext(grep('20201103', gwloggeR.data::enumerate(partner = 'knmi'), value = TRUE))

ref <- sapply(ref.logger.names, function(name) {
  df.ref <- gwloggeR.data::read(name)$df
  list('x' = df.ref[!is.na(PRESSURE_VALUE), PRESSURE_VALUE],
       'timestamps' = df.ref[!is.na(PRESSURE_VALUE), TIMESTAMP_UTC])
}, simplify = FALSE, USE.NAMES = TRUE)


ggplot2::ggplot(data = data.table::rbindlist(ref, use.names = TRUE, fill = TRUE, idcol = 'file'),
                mapping = ggplot2::aes(x = timestamps, y = x, col = file)) +
  ggplot2::geom_line(alpha = 0.5) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.position='top',
                 axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                 axis.title.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank())

dir.create('./drifts/analysis_34', recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave('./drifts/analysis_34/knmi_all.png', width = 1280/96, height = 720/96, dpi = 96)


save.output <- function(f) {

  print(Sys.time())
  print(basename(f))

  ROOT.PATH <- './drifts/analysis_34/'

  df <- gwloggeR.data::read(f)$df
  df <- df[!is.na(TIMESTAMP_UTC),]
  df <- df[!is.na(PRESSURE_VALUE),]
  data.table::setkey(df, TIMESTAMP_UTC)

  if (nrow(df) == 0L) {
    print('Skipping...')
    return()
  }

  gwloggeR:::test.detect_function(
    fun = gwloggeR::detect_drift,
    x = df$PRESSURE_VALUE,
    timestamps = df$TIMESTAMP_UTC,
    reference = ref,
    apriori = gwloggeR::Apriori(data_type = 'air pressure', units = 'cmH2O'),
    verbose = TRUE,
    plot = TRUE,
    title = paste0(basename(f), ' - v0.02'),
    alpha = 1,
    RESULT.PATH = paste0(ROOT.PATH, basename(f), '.result'),
    ATTRIB.PATH = paste0(ROOT.PATH, basename(f), '.attribs'),
    IMG.PATH = paste0(ROOT.PATH, basename(f), '.png'),
    LOG.PATH = paste0(ROOT.PATH, basename(f), '.log')
  )

}

# parallel::setDefaultCluster(parallel::makeCluster(spec = 5L))
# parallel::clusterExport(varlist = c('ref'))
# results <- setNames(parallel::clusterApplyLB(x = logger.names, fun = save.output), logger.names)
# names(results) <- logger.names
# parallel::stopCluster(cl = parallel::getDefaultCluster())

results <- sapply(logger.names, save.output, USE.NAMES = TRUE, simplify = FALSE)

results.df <- data.table::rbindlist(lapply(names(results), function(ln) {
  c(list('name' = basename(ln)),
    attributes(results[[ln]])[c('mu', 'timestamp', 'rate', 'significance')])
}), use.names = TRUE, fill = TRUE)

write.csv(results.df, file = './drifts/analysis_34/results.csv', row.names = FALSE)

plot(x = results.df$significance, y = results.df$rate,
     ylim = quantile(results.df$rate, probs = c(0.025, 0.975), na.rm = TRUE))

plot(x = results.df$significance, y = results.df$rate,
     ylim = quantile(results.df$rate, probs = c(0.025, 0.975), na.rm = TRUE),
     xlim = c(0, 1/1000))

hist(results.df$significance, breaks = 100)
hist(results.df$significance[results.df$significance < 0.05], breaks = 100)
quantile(results.df$significance, probs = c(0.05, 0.1, 0.2, 0.5), na.rm = TRUE)
plot(ecdf(results.df$significance))
