context("calculateAccuracy")

library(dplyr)
library(magrittr)

testPlugThreshold <- 10
testNilmThreshold <- 2

test_that("inner join plug and nilm usage", {
  joinedPlugNilmUsage <-
    joinPlugNilmUsage(plugUsagesForTest, nilmUsagesForTest)

  expect_identical(names(joinedPlugNilmUsage),
                   c("timestamp", "nilmUpu", "plugUpu"))
})

test_that("set on/off with given usage threshold", {
  onOffSet <-
    setOnOff(
      plugUsagesForTest,
      nilmUsagesForTest,
      plugThreshold = testPlugThreshold,
      nilmThreshold = testNilmThreshold
    )

  expect_identical(names(onOffSet),
                   c("timestamp", "nilmUpu", "plugUpu", "plugOn", "nilmOn"))
})

test_that("calculate accuracy of given plug and nilm usage", {
  acc <- calculateAccuracy(
    plugUsage = plugUsagesForTest,
    nilmUsage = nilmUsagesForTest,
    plugThreshold = testPlugThreshold,
    nilmThreshold = testNilmThreshold
  )

  expect_identical(names(acc),
                   HeaderOfAccuracyTable)
})

test_that("raise an error if plug or nilm is empty", {
  expect_error(
    calculateAccuracy(
      plugUsage = tibble(),
      nilmUsage = nilmUsagesForTest,
      plugThreshold = testPlugThreshold,
      nilmThreshold = testNilmThreshold
    )
  )

  expect_error(
    calculateAccuracy(
      plugUsage = plugUsagesForTest,
      nilmUsage = tibble(),
      plugThreshold = testPlugThreshold,
      nilmThreshold = testNilmThreshold
    )
  )

  expect_error(
    calculateAccuracy(
      plugUsage = tibble(),
      nilmUsage = tibble(),
      plugThreshold = testPlugThreshold,
      nilmThreshold = testNilmThreshold
    )
  )
})
