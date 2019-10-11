local({

  ROOT.PATH <- './temporalchanges/temporalchanges_v0.06_inbo/'

  print(Sys.time())
  for (f in gwloggeR.data::enumerate(partner = 'inbo')) {
    print(Sys.time())
    print(basename(f))

    df <- gwloggeR.data::read(f)$df
    df <- df[!is.na(TIMESTAMP_UTC),]

    point_sample_type <- substr(basename(f), 4L, 4L)
    ap <- switch (point_sample_type,
                  "L" = gwloggeR::apriori("air pressure", "cmH2O"),
                  "P" = gwloggeR::apriori("hydrostatic pressure", "cmH2O"),
                  "S" = gwloggeR::apriori("hydrostatic pressure", "cmH2O")
    )

    if (point_sample_type == 'L') next()

    local({
      png.path <- paste0(ROOT.PATH, basename(f), '.png')
      result.path <- paste0(ROOT.PATH, basename(f), '.output')

      png(png.path, width = 1280, height = 720)
      on.exit(dev.off())

      r <- gwloggeR::detect_temporalchanges(x = df$PRESSURE_VALUE, apriori = ap,
                                            timestamps = df$TIMESTAMP_UTC,
                                            plot = TRUE, title = paste0(basename(f), ' - v0.06'),
                                            verbose = TRUE)

      saveRDS(r, result.path, ascii = TRUE, compress = FALSE)
    })
  }

  print(Sys.time())
})
