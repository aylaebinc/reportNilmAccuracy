context("test-match_nilm_plug_files.R")

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"
testYear <- 2017
testMonth <- 4

test_that("check building file pattern to filter",
          {
            expect_equal(buildFilePattern("washer", testYear, testMonth),
                         "*_washer.*_2017_04\\.csv")
          })

test_that("filter file by year, month and appliance name",
          {
            getQhourlyPlugUsageFolderPath(
              rootPath = sampleDataSetPath,
              year = testYear,
              month = testMonth,
              serialNumber = sampleHomeSn
            ) %>%
              filterUsageFilePath(applianceName = "washer",
                                  year = testYear,
                                  month = testMonth) -> foundPath

            expectedPath <-
              file.path(
                sampleDataSetPath,
                sprintf("%d%02d", as.integer(testYear), as.integer(testMonth)),
                paste0("E001_", sampleHomeSn),
                "PLUG_15m",
                sprintf(
                  "F3FFD302_PLUG_15m_67_washer_%d_%02d.csv",
                  testYear,
                  testMonth
                )
              )

            expect_equal(foundPath, expectedPath)
          })

test_that("find plug file by home SN, year, month and appliance name",
          {
            foundPath <- findPlugUsageFilePath(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = "washer"
            )

            expectedPath <-
              file.path(
                sampleDataSetPath,
                sprintf("%d%02d", as.integer(testYear), as.integer(testMonth)),
                paste0("E001_", sampleHomeSn),
                "PLUG_15m",
                sprintf(
                  "F3FFD302_PLUG_15m_67_washer_%d_%02d.csv",
                  testYear,
                  testMonth
                )
              )

            expect_equal(foundPath, expectedPath)
          })

test_that("find NILM file by home SN, year, month and appliance name",
          {
            foundPath <- findNilmUsageFilePath(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = "ricecooker"
            )

            expectedPath <-
              file.path(
                sampleDataSetPath,
                sprintf("%d%02d", as.integer(testYear), as.integer(testMonth)),
                paste0("E001_", sampleHomeSn),
                "NILM_15m",
                sprintf(
                  "F30200C8_NILM_15m_66_ricecooker_%d_%02d.csv",
                  testYear,
                  testMonth
                )
              )

            expect_equal(foundPath, expectedPath)
          })

test_that("raise error when input applianceName is undefined",
          {
            expect_error(
              findPlugUsageFilePath(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "iMac"
              )
            )

            expect_error(
              findNilmUsageFilePath(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "iMac"
              )
            )
          })

test_that("return empty string when applianceName is valid but file does not exist",
          {
            expect_identical(
              findPlugUsageFilePath(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "heavyload"
              ),
              character()
            )

            expect_identical(
              findNilmUsageFilePath(
                rootPath = sampleDataSetPath,
                homeSn = sampleHomeSn,
                year = testYear,
                month = testMonth,
                applianceName = "washer"
              ),
              character()
            )
          })
