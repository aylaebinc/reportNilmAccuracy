context("getOnOffThreshold")

test_that("", {
  appName <- "aircon"
  type <- "plug"
  thres <- getOnOffThreshold(appName, type)
  expect_equal(thres, 25)

  appName <- "refrigerator"
  type <- "plug"
  thres <- getOnOffThreshold(appName, type)
  expect_equal(thres, 1)

  appName <- "refrigerator"
  type <- "nilm"
  thres <- getOnOffThreshold(appName, type)
  expect_equal(thres, 0.01)

  appName <- "iMac"
  type <- "plug"
  expect_error(getOnOffThreshold(appName, type))

  appName <- "tv"
  type <- "unplug"
  expect_error(getOnOffThreshold(appName, type))
})
