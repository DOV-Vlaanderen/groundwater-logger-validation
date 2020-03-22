# Barometric formula
# https://en.wikipedia.org/wiki/Barometric_formula
# Ideal law: P = rho * R/M * T
# See also:
# * https://physics.stackexchange.com/questions/532903/air-pressure-dependence-on-temperature
# * https://en.wikipedia.org/wiki/Density_of_air

g0 <- 9.80665 # m/s2 -- gravitational acceleration
R <- 8.3144598 # J/(mol·K) -- universal gas constant
M <- 0.0289644 # kg/mol -- molar mass of Earth's air
M_v <- 0.018016 # kg/mol -- molar mass of water vapor

# Constants taken from Barometric formula
# https://en.wikipedia.org/wiki/Barometric_formula
# for altitude between 0 and 11 km.

h_b <- 0 # m
P_b <- 101325.00 # Pa
T_b <- 288.15 # K
L_b <- -0.0065 # K/m
rho_b <- 1.2250 # kg/m3

# This function computes the pressure in function of altitude.
# LR stands for lapse rate: the drop in temperature in function of altitude is
# here taken into consideration. For altitude between 0 and 11 km, the drop
# in temperature is taken as a linear function: T = T_b + L_b*h.
# See: https://en.wikipedia.org/wiki/Barometric_formula#Derivation
# Other assumption here is that the air pressure is hydrostatic (i.e. "fluid" is not moving).
P.lr <- function(h.meters) {
  base <- T_b / (T_b + L_b * (h.meters - h_b))
  exp <- g0 * M / (R * L_b)
  P_b * base^exp
}

P.lr(10)

# PRESSURE
# Here I define a few pressure conversion functions.

P.cmH2O_to_Pa <- gwloggeR.data:::P.cmH2O_to_Pa

P.Pa_to_cmH2O <- gwloggeR.data:::P.Pa_to_cmH2O

P.Pa_to_cmH2O(P.lr(0))
P.Pa_to_cmH2O(P.lr(100))

# Conclusion here is that the drop in air pressure is expected to drop per 100 m:
P.Pa_to_cmH2O(P.lr(0)) - P.Pa_to_cmH2O(P.lr(100))

curve(P.Pa_to_cmH2O(P.lr(x)), xlim = c(0, 11000), xlab = 'Altitude (m)', ylab = 'Air pressure (cmH2O)')

# DENSITY
# This is the same as P.lr, but for the density.
# Not important I think.

# rho.lr <- function(h.meters) {
#   base <- T_b / (T_b + L_b * (h.meters - h_b))
#   exp <- 1 + g0 * M / (R * L_b)
#   rho_b * base^exp
# }
#
# rho.lr(0)
# rho.lr(100)
#
# curve(rho.lr(x), xlim = c(0, 11000))

# TEMPERATURE EFFECT

# This is a very naive calculation of temperature effect based on the ideal law
# because the density of air is assumed not to change in function of temperature.
P.Pa_to_cmH2O(rho_b*R/M*(T_b + 1) - rho_b*R/M*T_b)

## TEMPERATURE EFFECT ON DENSITY OF DRY AIR

# Here we focus on the following:
# https://en.wikipedia.org/wiki/Density_of_air

# This function computes the density (rho) of air based on the assumption of
# dry air.
rho_da <- function(temp.K) {
  P_b*M/(R*temp.K)
}

rho_da(T_b)
rho_da(15 + 273.15) # 15 °C
rho_da(20 + 273.15)

# Less naive calculation of temperature effect assuming dry air.
P.Pa_to_cmH2O(rho_da(T_b + 1)*R/M*(T_b + 1) - rho_da(T_b)*R/M*T_b)

## TEMPERATURE EFFECT ON DENSITY OF HUMID AIR

# Saturation vapor pressure
# See: https://en.wikipedia.org/wiki/Vapour_pressure_of_water
# The Buck equation is the best one.

P_sat.Pa <- function(temp.K, type = c('Buck', 'Tetens')) {
  type <- match.arg(type)
  temp.C <- temp.K - 273.15
  switch (type,
    'Buck' = 0.61121*exp( (18.678 - temp.C/234.5)*(temp.C/(257.14 + temp.C)) ),
    'Tetens' = 0.61078*exp(17.2*temp.C/(temp.C + 237.3))
  )
}

P_sat.Pa(T_b)
P_sat.Pa(T_b, type = 'Tetens')

# This function computes the density (rho) of air based on the assumption of
# humid air.

rho_ha <- function(temp.K, humidity.perc) {
  P_v <- humidity.perc/100 * P_sat.Pa(temp.K)
  P_d <- P_b - P_v

  (P_d*M + P_v*M_v)/(R*temp.K)
}

rho_da(T_b)
rho_ha(T_b, 0)
rho_ha(T_b, 100)

# Less naive calculation of temperature effect assuming humid air.
P.Pa_to_cmH2O(rho_ha(T_b + 1, 90)*R/M*(T_b + 1) - rho_ha(T_b, 90)*R/M*T_b)

# HUMIDITY
# The bulge during non-summer can not be explained by drop in air pressure density.
# https://nl.wikipedia.org/wiki/Relatieve_luchtvochtigheid#Relatieve_luchtvochtigheid_en_klimaat
P.Pa_to_cmH2O(rho_ha(T_b, 75)*R/M*T_b - rho_ha(T_b, 90)*R/M*T_b)
