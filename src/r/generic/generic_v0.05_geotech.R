options(warn=2)

local({
  for (f in  gwloggeR.data:::enumerate(partner = 'inbo')) {
    print(Sys.time())
    print(basename(f))

    point_sample_type <- substr(basename(f), 4L, 4L)

    if (point_sample_type == 'L') next()

    #if (basename(f) == 'DYLP161A_B5260.csv') browser()

    df <- gwloggeR.data::read(f)$df

    # remove no-timestamp
    df <- df[!is.na(TIMESTAMP_UTC),]
    data.table::setkey(df, TIMESTAMP_UTC)

    df.types <- gwloggeR:::detect(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC)
    df.plot <- gwloggeR:::plot.data(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, df.types = df.types)
    plt <- gwloggeR:::plot.base(data = df.plot)
    plt <- gwloggeR:::plot.add.levelshifts(plt)
    plt <- gwloggeR:::plot.add.outliers(plt)
    plt <- plt + ggplot2::ggtitle(paste0(basename(f), ' - v0.05'))

    local({
      png(paste0('./generic/generic_v0.05_inbo/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())
      print(plt)
    })

  }
  print(Sys.time())
})

