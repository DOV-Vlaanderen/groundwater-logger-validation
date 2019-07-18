local({
  print(Sys.time())
  cl <- parallel::makeCluster(7, outfile = './log/parallel.log')
  on.exit(parallel::stopCluster(cl))

  fn <- function(f) {
    print(Sys.time())
    print(basename(f))
    df <- Logger(f)$df

    # remove no-timestamp
    df <- df[!is.na(TIMESTAMP_UTC),]
    data.table::setkey(df, TIMESTAMP_UTC)

    N <- nrow(df)
    if (N == 0L) next()

    y <- ts(gwloggeR:::aggregate(x = df[,PRESSURE_VALUE], ts = df[, TIMESTAMP_UTC], by = 'hours'))
    tso <- tsoutliers::tso(y = y
                           #, types = 'LS'
                           #, discard.method = 'bottom-up'
                           , cval = gwloggeR:::c.optimal(alpha = 0.0005, n = N, type = 'two.sided')
                           #,tsmethod = 'arima'
                           #, args.tsmethod = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 0)))
                           )

    local({
      png(paste0('./tsoutliers/tsoutliers_geotech/', basename(f), '.png'), width = 1920, height = 1080)
      on.exit(dev.off())
      if (length(tso$outliers$ind) > 0L) plot(tso) else plot.new()
      mtext(paste0(basename(f), ' - ', forecast:::arima.string(tso$fit), ', N = ', length(y)), side = 3, line = -2, outer = TRUE)
    })
  }

  parallel::clusterApplyLB(cl = cl, x = Logger::enumerate(partner = 'geotech'), fun = fn)

  print(Sys.time())
})

