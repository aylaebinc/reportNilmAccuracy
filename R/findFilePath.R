ApplianceNameList <- c("tv",
                       "refrigerator",
                       "washer",
                       "ricecooker",
                       "heavyload",
                       "microwave",
                       "aircon")

#' @export
findPlugUsageFilePath <- function(rootPath,
                                  homeSn,
                                  year,
                                  month,
                                  applianceName) {
  applianceName <-
    match.arg(applianceName, choices = ApplianceNameList)
  getQhourlyPlugUsageFolderPath(rootPath, year, month, homeSn) %>%
    filterUsageFilePath(applianceName, year, month)
}

#' @export
findNilmUsageFilePath <- function(rootPath,
                                  homeSn,
                                  year,
                                  month,
                                  applianceName) {
  applianceName <-
    match.arg(applianceName, choices = ApplianceNameList)
  getQhourlyNilmUsageFolderPath(rootPath, year, month, homeSn) %>%
    filterUsageFilePath(applianceName, year, month)
}

filterUsageFilePath <-
  function(folderPath, applianceName, year, month) {
    dir(
      path = folderPath,
      pattern = buildFilePattern(applianceName, year, month),
      full.names = TRUE
    )
  }

buildFilePattern <- function(applianceName, year, month) {
  sprintf("*_%s.*_%d_%02d\\.csv",
          applianceName,
          as.integer(year),
          as.integer(month))
}
