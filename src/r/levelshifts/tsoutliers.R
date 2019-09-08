source("./data.R")

local({
  print(Sys.time())
  pdf(file = './levelshifts/tsoutliers_agg_auto.pdf', width = 14, height = 7, compress = FALSE)
  for (f in gwloggeR.data:::enumerate((partner = 'inbo')) {
    print(basename(f))
    df <- gwloggeR.data::read(f)$df

    # remove no-timestamp
    df <- df[!is.na(DRME_OCR_UTC_DTE),]
    data.table::setkey(df, DRME_OCR_UTC_DTE)

    N <- nrow(df)
    if (N == 0L) next()

    tso <- tsoutliers::tso(y = ts(aggregate_ts(df[,DRME_DRU], df[, DRME_OCR_UTC_DTE])),
                           #types = 'LS',
                           #discard.method = 'bottom-up',
                           cval = qnorm(p = (1 - 0.0005)^(1 / (N*2)))#,
                           #tsmethod = 'arima',
                           #args.tsmethod = list(order = c(0, 1, 0), seasonal = list(order = c(0, 0, 0)))
                           )

    if (length(tso$outliers$ind) > 0L) plot(tso) else plot.new()
    mtext(paste0(f, ' - ', forecast:::arima.string(tso$fit)), side = 3, line = -2, outer = TRUE)

  }
  dev.off()
  print(Sys.time())
})

