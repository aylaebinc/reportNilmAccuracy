library(reportNilmAccuracy)
library(tidyverse)

sampleDataSetPath <-
  "~/dev_R/custom_packages/encored_package/reportNilmAccuracy/inst/extdata/tohoku_sample"

sampleHomeSn <- "F30200C8"

convertUsageUnit <- function(filepath) {
  convertedUsage <-
    read_csv(filepath, col_names = TRUE) %>%
    mutate(unitPeriodUsage = convMillWhToWh(unitPeriodUsage),
           timestamp = as.character(timestamp)) %>%
    select(serialNumber, timestamp, unitPeriodUsage, typeid)

  stopifnot(reportNilmAccuracy:::chkQhourlyNilmUsage(convertedUsage))
  write_csv(convertedUsage, path = filepath, col_names = TRUE)
}

convMillWhToWh <- function(x) {
  round(x / 1000, digits = 2)
}

monthList <- 4:5
map_chr(
  monthList,
  reportNilmAccuracy:::getQhourlyPlugUsageFolderPath,
  rootPath = sampleDataSetPath,
  year = 2017,
  serialNumber = "F30200C8"
) %>%
  dir(full.names = TRUE) %>%
  map(convertUsageUnit)

map_chr(
  monthList,
  reportNilmAccuracy:::getQhourlyNilmUsageFolderPath
  rootPath = sampleDataSetPath,
  year = 2017,
  serialNumber = "F30200C8"
) %>%
  dir(full.names = TRUE) %>%
  map(convertUsageUnit)
