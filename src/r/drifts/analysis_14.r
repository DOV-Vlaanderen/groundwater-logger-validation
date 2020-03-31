# Altitude analysis in continuation with v08, but now we also have the real
# altitudes to compare with.

source('./drifts/analysis_08.r', encoding = 'UTF-8')

df.altitude <- data.table::data.table('location' = sub('[^/]*/([^_]+).*', '\\1', names(altitude.m)),
                                      'altitude.m' = altitude.m)
altitude.location.diff <- df.altitude[, .(altitude.diff.m = max(altitude.m) - min(altitude.m)), by = location][altitude.diff.m != 0, ]
altitude.location.diff[, sd(altitude.diff.m)]
hist(altitude.location.diff[, altitude.diff.m], breaks = 20, main = '',
     xlab = 'Barometric altitude (m) differences between barometers on the same location')

height <- read.csv('./../../data/meta/inbo/baro_height_dtm.csv',
                   dec = ',', sep = ';', na.strings = 'NULL', stringsAsFactors = FALSE)

height <- merge(height, df.altitude, all = TRUE, by.x = 'Peilpunt', by.y = 'location')
hist(height$altitude.m - height$RASTERVALU, breaks = 100, main = '',
     xlab = 'Differences between barometric altitude (m) and TAW')
plot(height$RASTERVALU, height$altitude.m, xlab = 'TAW (m)', ylab = 'Barometric altitude (m)')
summary(fit.lm <- lm(formula = altitude.m ~ RASTERVALU, data = height))
summary(fit.qr <- quantreg::rq(formula = altitude.m ~ RASTERVALU, data = height))

