context("getNilmAccuracy")

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"
testYear <- 2017
testMonth <- 4

test_that("raise an error if given applianceName is NOT allowed to get the accuracy",
          {
            expect_error(
              getNilmAccuracy(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "heavyload"
              )
            )

            expect_error(
              getNilmAccuracy(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "microwave"
              )
            )

            expect_error(
              getNilmAccuracy(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "iPhone"
              )
            )
          })

test_that("get nilm accuracy for given home SN, month, and appliance", {
  testApp <- "aircon"

  acc <- getNilmAccuracy(
    rootPath = sampleDataSetPath,
    homeSn = sampleHomeSn,
    year = testYear,
    month = testMonth,
    applianceName = testApp
  )

  expect_identical(names(acc),
                   c("serialNumber",
                     "applianceName",
                     HeaderOfAccuracyTable))
})

test_that("get nilm accuracy for two month", {
  testApp <- "aircon"

  acc <- map_df(
    4:5,
    getNilmAccuracy,
    rootPath = sampleDataSetPath,
    homeSn = sampleHomeSn,
    year = testYear,
    applianceName = testApp
  )

  expect_identical(nrow(acc), 2L)
})

test_that("get nilm accuracy for given home SN, month, and appliance", {
  testApp <- "washer"

  acc <- getNilmAccuracy(
    rootPath = sampleDataSetPath,
    homeSn = sampleHomeSn,
    year = testYear,
    month = testMonth,
    applianceName = testApp
  )

  expect_identical(acc, tibble())
})
