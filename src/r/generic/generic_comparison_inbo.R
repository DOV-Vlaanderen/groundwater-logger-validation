local({
  print(Sys.time())
  folder <- c("./../r/tsoutliers/tsoutliers_inbo/",
              "./../r/generic/generic_tsoutliers_inbo/",
              "./../r/generic/generic_v0.05_inbo/",
              "./../r/generic/generic_v0.06_inbo/")
  pdf(file = './generic/generic_comparison_inbo.pdf', width = 16, height = 9, compress = TRUE)
  on.exit(dev.off())

  for (f in Logger::enumerate('inbo')) {
    print(basename(f))
    file.png <- list.files(folder, pattern = paste0(basename(f), '.*\\.png'), ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    png.list <- lapply(file.png, function(fn) grid::rasterGrob(png::readPNG(fn)))
    do.call(what = gridExtra::grid.arrange, args = c(png.list, list('ncol' = 2)))
  }

  print(Sys.time())
})
