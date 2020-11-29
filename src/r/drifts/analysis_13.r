# Analysis 04 (including 10) and 12 together

analysis_10 <- read.csv(file = './drifts/analysis_10/baro_median_errors.csv')
analysis_10 <- analysis_10[order(analysis_10$WISK, decreasing = TRUE),]

local({
  print(Sys.time())
  folder <- c("./drifts/analysis_04/",
              "./drifts/analysis_12/")
  pdf(file = './drifts/analysis_13/comparison.pdf', width = 16, height = 9, compress = TRUE)
  on.exit(dev.off())

  for (f in tools::file_path_sans_ext(analysis_10$FILE)) {
    print(basename(f))
    file.png <- list.files(folder, pattern = paste0(f, '.*\\.png'), ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    png.list <- lapply(file.png, function(fn) grid::rasterGrob(png::readPNG(fn)))
    do.call(what = gridExtra::grid.arrange, args = c(png.list, list('ncol' = 2)))
  }

  print(Sys.time())
})
