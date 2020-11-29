# Behaviour of median in function of x observations.

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

df.knmi <- read.baro('KNMI_20200312_hourly')

boot.sample <- function(n.obs) {
  N <- nrow(df.knmi)
  n.omit.first <- sample.int(n.obs, 1L)
  n.omit.last <- (N - n.omit.first) %% n.obs
  i.omit <- c(1L:n.omit.first, if (n.omit.last == 0L) NULL else (N-n.omit.last+1L):N)
  i.groups <- rep(1L:((N - n.omit.first - n.omit.last) %/% n.obs), each = n.obs)
  df.knmi[-i.omit, .(VAL = median(PRESSURE_VALUE)), by = i.groups]$VAL
}

boot <- function(n.obs, n.samples.min = 1000L) {
  n.samples <- 0L
  samples <- list()
  repeat({
    if (sum(vapply(samples, length, 1L)) >= n.samples.min) break()
    samples[[length(samples) + 1L]] <- boot.sample(n.obs = n.obs)
  })
  unlist(samples)
}

hist(boot(100), breaks = 100)

n.obs <- seq(from = 10, to = 3500, by = 10)
parallel::setDefaultCluster(parallel::makeCluster(spec = 7L))
parallel::clusterExport(varlist = c('boot.sample', 'df.knmi'))
results <- parallel::clusterApplyLB(x = n.obs, fun = boot)
parallel::stopCluster(cl = parallel::getDefaultCluster())

stats <- data.table::data.table(n.obs = n.obs)
stats[, sd := vapply(results, sd, 1)]
stats

ggplot2::ggplot(data = stats, mapping = ggplot2::aes(x = n.obs, y = sd)) +
  ggplot2::geom_line() +
  ggplot2::theme_light() +
  ggplot2::ylab('Standard deviation of the median pressure (chH2O)') +
  ggplot2::xlab('Number of observations (12h interval)') +
  ggplot2::ggtitle('KNMI Westdorpe bootstrapped sample from 2010-2020')
