TargetAppliancesToGetAccuracy <- c("tv",
                                   "refrigerator",
                                   "washer",
                                   "ricecooker",
                                   "aircon")

#' @importFrom dplyr bind_cols
#' @export
getNilmAccuracy <- function(rootPath,
                            homeSn,
                            year,
                            month,
                            applianceName) {
  applianceName <-
    match.arg(applianceName, choices = TargetAppliancesToGetAccuracy)
  loadPlugUsage(
    rootPath = rootPath,
    homeSn = homeSn,
    year = year,
    month = month,
    applianceName = applianceName
  ) -> plugUsage

  loadNilmUsage(
    rootPath = rootPath,
    homeSn = homeSn,
    year = year,
    month = month,
    applianceName = applianceName
  ) -> nilmUsage

  if (identical(plugUsage, tibble()) |
      identical(nilmUsage, tibble())) {
    return(tibble())
  }

  plugThreshold <-
    getOnOffThreshold(applianceName, type = "plug")
  nilmThreshold <-
    getOnOffThreshold(applianceName, type = "nilm")

  accuracy <- calculateAccuracy(plugUsage,
                                nilmUsage,
                                plugThreshold,
                                nilmThreshold)
  keys <- tibble(serialNumber = homeSn,
                 applianceName = applianceName)

  bind_cols(keys, accuracy)
}
