# Analysis of differences between one barometer and many others in its neighbourhood.

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
  loggers <- grep(paste0('barometer/', location), gwloggeR.data::enumerate(), value = TRUE)
  df <- data.table::rbindlist(lapply(loggers, function(logger) gwloggeR.data::read(logger)$df), use.names = TRUE)
  if (nrow(df) == 0L) return(df)
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'location' = location, 'loggers' = basename(loggers))
}

compare <- function(df1, df2, distance = NA) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())
  df1.name <- attr(df1, 'location')
  df2.name <- attr(df2, 'location')

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]
  if (nrow(diff.df) < 250L) return(data.table::data.table())

  ap <- gwloggeR::apriori('air pressure')
  df1.operc <- sum(gwloggeR::detect_outliers(df1[J(diff.df), PRESSURE_VALUE], apriori = ap))/nrow(diff.df)
  df2.operc <- sum(gwloggeR::detect_outliers(df2[J(diff.df), PRESSURE_VALUE], apriori = ap))/nrow(diff.df)
  if (df1.operc > 0.1 || df2.operc > 0.1) return(data.table::data.table())

  diff.df
}

compute <- function(logger.loc, nr_surrounding_barros = length(distances)) {
  logger.xy <- unlist(locations[J(logger.loc), .(Lambert72_X, Lambert72_Y)])
  logger.df <- read.full(logger.loc)

  distances <- d.vec(logger.xy, locations[, .(Lambert72_X, Lambert72_Y)])/1000
  names(distances) <- locations$ppnt_cde
  distances <- sort(distances, na.last = TRUE)[-1L]

  df.diff <- lapply(names(distances[1L:nr_surrounding_barros]), function(other.loc) {
    other.df <- read.full(other.loc)
    compare(logger.df, other.df, distances[other.loc])
  })
  df.diff <- data.table::rbindlist(df.diff, use.names = TRUE, fill = TRUE)

  if (nrow(df.diff) == 0L) return()

  df.quant <- df.diff[
    , .(Q.025 = quantile(PRESSURE_DIFF, 0.025),
        Q.5 = quantile(PRESSURE_DIFF, 0.5),
        Q.975 = quantile(PRESSURE_DIFF, 0.975)),
    by = TIMESTAMP_UTC]

  plt <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF)) +
    ggplot2::geom_point(pch = '.') +
    ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.5) +
    ggplot2::stat_smooth(method = 'gam', formula = y ~ s(x, k = 40), color = 'yellow', se = FALSE) +
    ggplot2::coord_cartesian(ylim = quantile(df.diff$PRESSURE_DIFF, probs = c(0.025, 0.975))) +
    ggplot2::scale_x_datetime(date_breaks = '1 year', date_labels = '%Y') +
    ggplot2::ggtitle(sprintf('%s (%s) #%s', logger.loc, paste0(attr(logger.df, 'loggers'), collapse = ', '), nrow(df.diff))) +
    ggplot2::theme_light()

  filename <- sprintf('./drifts/analysis_02/%s.png', logger.loc)

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(plt)
  })

}

# A, B, dist, sd
# tmp <- compute(locations$ppnt_cde[15L])
# tmp <- lapply(locations$ppnt_cde, compute)

parallel::setDefaultCluster(parallel::makeCluster(spec = 7L))
parallel::clusterExport(varlist = c('d', 'd.vec', 'compare', 'read.full', 'locations'))
results <- parallel::clusterApplyLB(x = locations$ppnt_cde, fun = compute)
parallel::stopCluster(cl = parallel::getDefaultCluster())

