chkQhourlyHomeUsage <- function(usage) {
  stopifnot(isHomeUsageHeaderFine(names(usage)))
  stopifnot(isTimestampFine(usage$timestamp))
  stopifnot(isUsageFine(usage$unitPeriodUsage))
  return(TRUE)
}

chkQhourlyPlugUsage <- function(usage) {
  stopifnot(isPlugUsageHeaderFine(names(usage)))
  stopifnot(isTimestampFine(usage$timestamp))
  stopifnot(isUsageFine(usage$unitPeriodUsage))
  return(TRUE)
}

chkQhourlyNilmUsage <- function(usage) {
  stopifnot(isNilmUsageHeaderFine(names(usage)))
  stopifnot(isTimestampFine(usage$timestamp))
  stopifnot(isUsageFine(usage$unitPeriodUsage))
  return(TRUE)
}

CommonHeader <- c("serialNumber", "timestamp", "unitPeriodUsage")

isHomeUsageHeaderFine <- function(header) {
  identical(header, CommonHeader)
}

isPlugUsageHeaderFine <- function(header) {
  identical(header, c(CommonHeader, "typeid"))
}

isNilmUsageHeaderFine <- function(header) {
  identical(header, c(CommonHeader, "typeid"))
}
