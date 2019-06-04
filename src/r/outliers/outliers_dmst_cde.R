source('outliers/outliers_dmst_cde.plot.R')

# ENT (entered) is the basic status meaning that these measurements are used
# in the workflow; DEL (deleted) and INV (invalid) are measurements that were
# visually detected by users as being suspicious, hence those that have to be
# detected by the algorithm.

local({
  folder <- "./../../data/raw/inbo/"
  pdf(file = './outliers/outliers_dmst_cde.pdf', width = 14, height = 7, compress = FALSE)
  for (f in list.files(folder, full.names = TRUE, pattern = ".*\\.csv")) {
    print(plot.dmst_cde(f))
  }
  dev.off()
})

