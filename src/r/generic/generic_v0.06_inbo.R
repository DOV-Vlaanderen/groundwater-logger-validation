options(warn=2)

local({
  for (f in  gwloggeR.data:::enumerate(partner = 'inbo')) {
    print(Sys.time())
    print(basename(f))

    #if (basename(f) == 'DYLP029A_S4054.csv') browser()

    point_sample_type <- substr(basename(f), 4L, 4L)
    if (point_sample_type == 'L') next()

    df <- gwloggeR.data::read(f)$df

    # remove no-timestamp
    df <- df[!is.na(TIMESTAMP_UTC),]
    data.table::setkey(df, TIMESTAMP_UTC)

    df.types <- gwloggeR:::detect(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, types = c('AO', 'LS', 'TC'))
    df.plot <- gwloggeR:::plot.data(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, df.types = df.types)
    plt <- gwloggeR:::plot.base(data = df.plot)
    plt <- gwloggeR:::plot.add.levelshifts(plt)
    plt <- gwloggeR:::plot.add.tempchanges(plt)
    plt <- gwloggeR:::plot.add.outliers(plt)
    plt <- plt + ggplot2::ggtitle(paste0(basename(f), ' - v0.06'))

    local({
      png(paste0('./generic/generic_v0.06_inbo/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())
      print(plt)
    })

  }
  print(Sys.time())
})

