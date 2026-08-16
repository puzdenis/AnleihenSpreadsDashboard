#Taget-Kalender Funktion ausRQuantLib
ql_cal <- function() {
  # TARGET-Kalender (ältere RQuantLib-Builds)
  RQuantLib::Calendar("TARGET")
}

adjust_bday <- function(date, convention = "Following") {
  RQuantLib::adjust(
    as.Date(date),
    cal = ql_cal(),
    bdc = convention
  )
}

advance_date <- function(date, n, unit = c("Days","Weeks","Months","Years"),
                         convention = "Following") {
  unit <- match.arg(unit)
  RQuantLib::advance(
    as.Date(date),
    n        = n,
    timeUnit = unit,
    cal      = ql_cal(),
    bdc      = convention
  )
}
