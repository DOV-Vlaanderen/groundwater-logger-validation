## code to export all dara-raw.

source('./data-raw/apriori-air-pressure.R')
source('./data-raw/apriori-hydrostatic-pressure.R')

usethis::use_data(airpressure, hydropressure, internal = TRUE, overwrite = TRUE)
