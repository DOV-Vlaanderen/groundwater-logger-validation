local({
  print(Sys.time())
  root.dir <- './tsoutliers/tsoutliers_inbo'
  cl <- parallel::makeCluster(7, outfile = './log/parallel.log')
  on.exit(parallel::stopCluster(cl))

  fn <- function(f) {
    loadNamespace('tsoutliers')

    print(Sys.time())
    print(basename(f))
    df <- gwloggeR.data::read(f)$df

    # remove no-timestamp
    df <- df[!is.na(TIMESTAMP_UTC),]
    data.table::setkey(df, TIMESTAMP_UTC)

    N <- nrow(df)

    y <- if (N != 0L) ts(gwloggeR:::aggregate(x = df[,PRESSURE_VALUE], ts = df[, TIMESTAMP_UTC], by = 'days'))

    tsoutliers.file <- paste0(root.dir, '/', basename(f), '.tsoutliers')
    if (!file.exists(tsoutliers.file)) {
      tso <- try(tsoutliers::tso(y = y
                                 #, types = 'LS'
                                 #, discard.method = 'bottom-up'
                                 , cval = gwloggeR:::c.norm.optimal(alpha = 0.0005, n = N, type = 'two.sided')
                                 #,tsmethod = 'arima'
                                 #, args.tsmethod = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 0)))
      ))

      saveRDS(tso, file = tsoutliers.file, ascii = TRUE, compress = FALSE)
    }

    tso <- readRDS(tsoutliers.file)
    error <- inherits(tso, 'try-error')

    local({
      png(paste0(root.dir, '/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())
      if (!error && length(tso$outliers$ind) > 0L) plot(tso) else plot.new()
      mtext(paste0(if (error) attr(tso, 'condition')$message,
                   basename(f), ' - ',
                   if (!error) forecast:::arima.string(tso$fit) else '/',
                   ', N = ', length(y)), side = 3, line = -2, outer = TRUE)
    })
  }

  parallel::clusterApplyLB(cl = cl, x = gwloggeR.data::enumerate(partner = 'inbo'), fun = fn)
  #lapply(gwloggeR.data::enumerate(partner = 'inbo'), fn)

  print(Sys.time())
})
