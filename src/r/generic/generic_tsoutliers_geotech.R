local({
  print(Sys.time())
  for (f in gwloggeR.data:::enumerate('geotech')) {
    print(basename(f))

    #if (basename(f) == 'ff_b29d_091125120713_C4080.MON') browser()

    tso.path <- paste0('./tsoutliers/tsoutliers_geotech/', basename(f), '.tsoutliers')
    tso <- readRDS(tso.path)
    error <- inherits(tso, 'try-error')

    if (error) {
      y <- as.vector(as.list(attr(tso, 'condition')$call)$x)
      df.types <- data.table::data.table('type' = character(), 'index' = integer())
    } else {
      y <- as.vector(tso$y)
      df.types <- data.table::as.data.table(tso$outliers)
      data.table::setnames(df.types, 'ind', 'index')
    }

    tt <- paste0(if (error) paste(attr(tso, 'condition')$message, ', '),
                  basename(f), ' - ',
                  if (!error) forecast:::arima.string(tso$fit) else '/',
                  ', N = ', length(y))

    df.plot <- gwloggeR:::plot.data(x = y, df.types = df.types)
    plt <- gwloggeR:::plot.base(data = df.plot)
    plt <- gwloggeR:::plot.add.levelshifts(plt)
    plt <- gwloggeR:::plot.add.outliers(plt)
    plt <- plt + ggplot2::ggtitle(tt)

    local({
      png(paste0('./generic/generic_tsoutliers_geotech/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())
      print(plt)
    })
  }
  print(Sys.time())
})
