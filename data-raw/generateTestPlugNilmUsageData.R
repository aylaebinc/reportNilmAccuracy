library(reportNilmAccuracy)

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"
testYear <- 2017
testMonth <- 4
testApp <- "aircon"

loadPlugUsage(
  rootPath = sampleDataSetPath,
  homeSn = sampleHomeSn,
  year = testYear,
  month = testMonth,
  applianceName = testApp
) -> plugUsagesForTest

loadNilmUsage(
  rootPath = sampleDataSetPath,
  homeSn = sampleHomeSn,
  year = testYear,
  month = testMonth,
  applianceName = testApp
) -> nilmUsagesForTest

library(usethis)
use_data(plugUsagesForTest, overwrite = TRUE)
use_data(nilmUsagesForTest, overwrite = TRUE)
