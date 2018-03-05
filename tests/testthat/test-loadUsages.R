context("test-loadUsages.R")

sampleDataSetPath <- system.file("extdata/tohoku_sample",
                                 package = "reportNilmAccuracy")
sampleHomeSn <- "F30200C8"
testYear <- 2017
testMonth <- 4
testApp <- "aircon"

test_that("loaded plug usage and loaded nilm usage have the same row count and schema",
          {
            loadPlugUsage(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = testApp
            ) -> plugUsages

            loadNilmUsage(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = testApp
            ) -> nilmUsages

            expect_identical(names(plugUsages), names(nilmUsages))
            expect_identical(nrow(plugUsages), nrow(nilmUsages))
          })

test_that("return empty tibble if loading fails",
          {
            loadPlugUsage(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = "heavyload"
            ) -> plugUsages

            expect_identical(plugUsages, tibble())

            loadNilmUsage(
              rootPath = sampleDataSetPath,
              homeSn = sampleHomeSn,
              year = testYear,
              month = testMonth,
              applianceName = "microwave"
            ) -> nilmUsages

            expect_identical(nilmUsages, tibble())
          })
