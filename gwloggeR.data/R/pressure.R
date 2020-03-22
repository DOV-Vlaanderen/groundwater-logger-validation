P.cmH2O_to_Pa <- function(P.cmH2O) {
  # https://en.wikipedia.org/wiki/Centimetre_of_water
  P.cmH2O * 98.063754138
}

P.Pa_to_cmH2O <- function(P.Pa) {
  P.Pa * 1/P.cmH2O_to_Pa(1)
}
