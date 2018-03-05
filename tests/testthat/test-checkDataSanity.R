context("test-check_data_schema.R")

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"

test_that("check unitPeriodUsage column requirements", {
  expect_true(isUsageFine(c(100.13, 49.5)))
  expect_true(isUsageFine(c(15.8, 43.2)))
  expect_true(isUsageFine(c(132, 41)))
  expect_false(isUsageFine(c(3.529, 4.531)))
})

test_that("check timestamp column requirements", {
  expect_true(isTimestampFine(c(1493541900000, 1493542800000)))
  expect_false(isTimestampFine(c(1493541900, 1493542800)))
})

test_that("check home quarter hourly usage data", {
  folderPath <-
    getQhourlyHomeUsageFolderPath(rootPath = sampleDataSetPath,
                                  year = 2017,
                                  month = 4,
                                  serialNumber = "F30200C8")

  dir(folderPath, full.names = TRUE) %>%
    head(1L) %>%
    readr::read_csv(col_names = TRUE) %>%
    chkQhourlyHomeUsage() %>%
    expect_true()
})

# test_that("check plug quarter hourly usage data", {
#   folderPath <-
#     getQhourlyPlugUsageFolderPath(rootPath = sampleDataSetPath,
#                                   serialNumber = "F30200C8")
#
#   dir(folderPath, full.names = TRUE) %>%
#     head(1L) %>%
#     read_csv(col_names = TRUE) -> wrongData
#
#   expect_error(chkQhourlyPlugUsage(wrongData))
# })
