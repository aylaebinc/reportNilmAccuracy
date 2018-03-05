getQhourlyHomeUsageFolderPath <-
  function(rootPath, year, month, serialNumber) {
    getTypedFolderPath(rootPath, year, month, serialNumber, "HOME_15m")
  }

getQhourlyPlugUsageFolderPath <-
  function(rootPath, year, month, serialNumber) {
    getTypedFolderPath(rootPath, year, month, serialNumber, "PLUG_15m")
  }

getQhourlyNilmUsageFolderPath <-
  function(rootPath, year, month, serialNumber) {
    getTypedFolderPath(rootPath, year, month, serialNumber, "NILM_15m")
  }

#' @importFrom dplyr %>%
getTypedFolderPath <- function(rootPath,
                               year,
                               month,
                               serialNumber,
                               dataType) {
  file.path(rootPath,
            sprintf("%d%02d", as.integer(year), as.integer(month))) %>%
    dir(pattern = paste0("*", serialNumber),
        full.names = TRUE) %>%
    file.path(dataType)
}

#' @importFrom stringr str_count
isTimestampFine <- function(timestampMilliSec) {
  all(str_count(timestampMilliSec) == 13L)
}

isUsageFine <- function(unitPeriodUsage) {
  upu <- as.numeric(unitPeriodUsage)
  round(upu, digits = 2L) %>%
    identical(upu)
}

CommonValidTypeIdSet <- c(12, 62, 65, 66, 67)

isPlugTypeIdFine <- function(typeId) {
  all(typeId %in% c(CommonValidTypeIdSet, 68))
}

isNilmTypeIdFine <- function(typeId) {
  all(typeId %in% c(CommonValidTypeIdSet, 69))
}

#' @importFrom stringr str_split
#' @importFrom purrr map_chr
extractDecimals <- function(x) {
  as.character(x) %>%
    str_split(pattern = '\\.') %>%
    map_chr(`[`, 2L)
}

getOnOffThreshold <- function(appName, type) {
  appName <- match.arg(appName, choices = ApplianceNameList)
  type <- match.arg(type, choices = c("nilm", "plug"))

  switch(
    appName,
    "aircon" = 25,
    "ricecooker" = 0.75,
    "washer" = 1,
    "refrigerator" = ifelse(type == "plug", 1, 0.01),
    "tv" = 4
  )
}
