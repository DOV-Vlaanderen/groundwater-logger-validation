# Analysis of differences between two barometers: mainly their variance.

locations <- read.csv('./../../data/meta/inbo/Baro_Position_Lambert72.csv',
                      dec = ',', sep = ';', na.strings = 'NULL', stringsAsFactors = FALSE)
data.table::setDT(locations)
data.table::setkey(locations, ppnt_cde)

d <- function(xy1, xy2) sqrt(sum((xy1 - xy2)^2))
d.vec <- function(xy.from, xy.to.df) {
  stopifnot(length(xy.from) == 2L)
  stopifnot(!is.null(nrow(xy.to.df)))

  apply(xy.to.df, 1, d, xy1 = xy.from)
}

read.full <- function(location) {
  loggers <- grep(paste0('barodata/', location), gwloggeR.data::enumerate(), value = TRUE)
  df <- data.table::rbindlist(lapply(loggers, function(logger) gwloggeR.data::read(logger)$df), use.names = TRUE)
  if (nrow(df) == 0L) return(df)
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'location' = location)
}

compare <- function(df1, df2, distance = NA, save.plot = FALSE) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(NA)
  df1.name <- attr(df1, 'location')
  df2.name <- attr(df2, 'location')

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]
  if (nrow(diff.df) < 250L) return(NA)

  ap <- gwloggeR::apriori('air pressure')
  df1.operc <- sum(gwloggeR::detect_outliers(df1[J(diff.df), PRESSURE_VALUE], apriori = ap))/nrow(diff.df)
  df2.operc <- sum(gwloggeR::detect_outliers(df2[J(diff.df), PRESSURE_VALUE], apriori = ap))/nrow(diff.df)
  if (df1.operc > 0.1 || df2.operc > 0.1) return(NA)

  filename <- sprintf('./drifts/analysis_01/%s.png', paste0(sort(c(df1.name, df2.name)), collapse = '-'))

  on.exit({if (save.plot) dev.off(); par(mfrow = c(1, 1))})
  if (save.plot) png(filename = filename, width = 768L, height = 480L)
  par(mfrow = c(2, 2))

  mad <- mad(diff.df$PRESSURE_DIFF)
  with(df1[J(diff.df),], plot(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, type = 'l', xlab = df1.name))
  with(df2[J(diff.df),], plot(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, type = 'l', xlab = df2.name))
  with(diff.df, plot(x = TIMESTAMP_UTC, y = PRESSURE_DIFF, type = 'l'))
  with(diff.df, hist(PRESSURE_DIFF, breaks = 50, main = ''))
  mtext(sprintf('%s vs. %s SD: %.3f (%.0f km)', df1.name, df2.name, mad, distance),
        outer = TRUE, line = -2.5, cex = 1.5)

  mad
}

compute <- function(logger.loc) {
  logger.xy <- unlist(locations[J(logger.loc), .(Lambert72_X, Lambert72_Y)])
  logger.df <- read.full(logger.loc)

  distances <- d.vec(logger.xy, locations[, .(Lambert72_X, Lambert72_Y)])/1000
  names(distances) <- locations$ppnt_cde
  distances <- sort(distances, na.last = TRUE)

  sd <- sapply(names(distances), function(other.loc) {
    if(logger.loc > other.loc) return(NA)
    other.df <- read.full(other.loc)
    compare(logger.df, other.df, distances[other.loc], save.plot = TRUE)
  })

  data.frame('A' = logger.loc, 'B' = names(distances), 'dist' = distances, 'sd' = sd,
             stringsAsFactors = FALSE)
}

# A, B, dist, sd
# tmp <- compute(locations$ppnt_cde[15L])
# tmp <- lapply(locations$ppnt_cde, compute)

parallel::setDefaultCluster(parallel::makeCluster(spec = 7L))
parallel::clusterExport(varlist = c('d', 'd.vec', 'compare', 'read.full', 'locations'))
results <- parallel::clusterApplyLB(x = locations$ppnt_cde, fun = compute)
parallel::stopCluster(cl = parallel::getDefaultCluster())

results <- data.table::rbindlist(results, use.names = TRUE)
results[, key := paste0(ifelse(A>B,B,A), '#', ifelse(A>B,A,B))]
data.table::setkey(results, key)
results <- results[!is.na(dist) & !is.na(sd), ] # before duplicate key removal due to: if(logger.loc > other.loc) return(NA)
results <- results[!duplicated(key), ]
results <- results[dist != 0, ]
with(results, plot(x = dist, y = sd))

results[order(sd, decreasing = TRUE),][1:10]

summary((fit.rq.5 <- quantreg::rq(data = results, formula = sd ~ dist)))
summary((fit.rq.1 <- quantreg::rq(data = results, formula = sd ~ dist, tau = 0.1)))

ggplot2::ggplot(data = results, mapping = ggplot2::aes(x = dist, y = sd)) +
  ggplot2::geom_point() +
  # ggplot2::geom_smooth(method = 'lm', formula = y ~ x, col = 'yellow', size = 1.2, alpha = 0.9) +
  ggplot2::geom_quantile(quantiles = c(0.1, 0.5), formula = y ~ x, size = 1.2, alpha = 0.9) +
  ggplot2::coord_cartesian(ylim = quantile(results$sd, probs = c(0.01, 0.99)),
                           xlim = quantile(results$dist, probs = c(0.01, 0.99))) +
  ggplot2::xlab('Distance (km)') + ggplot2::ylab('Standard deviation (MAD, cmH2O)') +
  ggplot2::ggtitle('Air pressure difference SD in function of barometer distance') +
  ggplot2::theme_light()
