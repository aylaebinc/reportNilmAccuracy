context("test-getNilmAccuracySummary.R")

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"
testYear <- 2017
testMonth <- 4

test_that("", {
  testApp <- "refrigerator"

  map_df(
    4:5,
    getNilmAccuracy,
    rootPath = sampleDataSetPath,
    homeSn = sampleHomeSn,
    year = testYear,
    applianceName = testApp
  ) %>%
    getNilmAccuracySummary() -> accSummary

  expect_identical(accSummary$ID, AccuracyListForSummary)
})
