# Barometric formula: altitude analysis
# https://en.wikipedia.org/wiki/Barometric_formula
# Ideal law: P = rho * R/M * T

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

logger.names <- setdiff(logger.names, 'barometer/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barometer/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

df.list <- sapply(logger.names, function(name) {
  df <- gwloggeR.data::read(name)$df

  # Filter
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!is.na(TIMESTAMP_UTC), ]

  # Aggregate
  df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]

  # Filter
  df <- df[PRESSURE_VALUE < 1100,]
  df <- df[PRESSURE_VALUE > 975,]

  # Meta data
  df[, FILE := basename(name)]
  df[, N := .N]

  data.table::setkey(df, TIMESTAMP_UTC)

  df
}, simplify = FALSE, USE.NAMES = TRUE)

df <- data.table::rbindlist(df.list, use.names = TRUE)

medians <- sapply(df.list, function(df) {
  median(df$PRESSURE_VALUE, na.rm = TRUE)
}, simplify = TRUE, USE.NAMES = TRUE)

# Load functions from analysis 07.
sys.source('./drifts/analysis_07.r', envir = (bf <- new.env()))

# This function computes the altitude in function of pressure.
# LR stands for lapse rate: the drop in temperature in function of altitude is
# here taken into consideration. For altitude between 0 and 11 km, the drop
# in temperature is taken as a linear function: T = T_b + L_b*h.
# See: https://en.wikipedia.org/wiki/Barometric_formula#Derivation
# Other assumption here is that the air pressure is hydrostatic (i.e. "fluid" is not moving).
bf$h.lr <- function(P.Pa) {
  A <- g0 * M / (R * L_b)
  B <- log(P_b / P.Pa)/A + log(T_b)
  (exp(B) - T_b)/L_b + h_b
}
environment(bf$h.lr) <- bf

bf$h.lr(bf$P_b)
bf$h.lr(bf$P.lr(50)) # Test, should be the same as input: 50

medians.Pa <- sapply(medians, bf$P.cmH2O_to_Pa, simplify = TRUE, USE.NAMES = TRUE)

altitude.m.bf <- sapply(medians.Pa, bf$h.lr, simplify = TRUE, USE.NAMES = TRUE)

hist(altitude.m.bf, breaks = 50, main = 'Altitudes (m) based on barometric formula', xlab = 'Altitude (m)')

df <- merge(x = df,
            y = data.frame(FILE = basename(names(altitude.m.bf)), ALTITUDE = as.vector(altitude.m.bf)),
            all.x = TRUE, by = 'FILE')

data.table::setkey(df, FILE, TIMESTAMP_UTC)

ggplot2::ggplot(data = df, mapping = ggplot2::aes(y = PRESSURE_VALUE, x = sprintf('%s (#%i)', FILE, N))) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip() +
  ggplot2::ggtitle(label = 'Barometer boxplots (2 obs/day)') +
  ggplot2::xlab(NULL) +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_08/baro_hist_uhd-1.png', width = 2160/96, height = 3840/96, dpi = 96)

ggplot2::ggplot(data = df, mapping = ggplot2::aes(y = PRESSURE_VALUE, x = sprintf('%s (#%i) [%.2fm]', FILE, N, ALTITUDE))) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(0)), size = 1.5, col = 'darkblue') +
  ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(50*c(-1,1))), size = 1.5, col = 'green') +
  ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(100*c(-1,1))), size = 1.5, col = 'red') +
  ggplot2::coord_flip() +
  ggplot2::ggtitle(label = 'Barometer boxplots (2 obs/day) with altitudes (barometric formula) based on medians',
                   subtitle = 'Blue: 0m, green: ±50m and red: ±100m') +
  ggplot2::xlab(NULL) +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_08/baro_hist_altitudes_uhd-1.png', width = 2160/96, height = 3840/96, dpi = 96)

write.csv(as.data.frame(altitude.m.bf), file = './drifts/analysis_08/baro_barometric_formula_altitudes.csv')
