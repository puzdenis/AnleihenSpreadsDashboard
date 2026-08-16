# Daycount aus RQuantLib
yearfrac_icma <- function(start, end, freq) {
  # Variante über RQuantLib:
  dc <- DayCounter("ActualActual", "ICMA")  # ICMA
  yearFraction(as.Date(start), as.Date(end), dc)
}
