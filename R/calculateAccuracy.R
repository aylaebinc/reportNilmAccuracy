HeaderOfAccuracyTable <- c(
  "TP",
  "FN",
  "FP",
  "TN",
  "nilmUsage",
  "plugUsage",
  "usageRatio",
  "accuracy",
  "recall",
  "precision",
  "F1",
  "MR"
)

calculateAccuracy <- function(plugUsage,
                              nilmUsage,
                              plugThreshold,
                              nilmThreshold) {
  if (identical(plugUsage, tibble()) |
      identical(nilmUsage, tibble())) {
    stop("Empty usage is NOT allowed!")
  }

  setOnOff(plugUsage, nilmUsage, plugThreshold, nilmThreshold) %>%
    filter(!is.na(nilmUpu) & !is.na(plugUpu)) %>%
    mutate(minValue = pmin(nilmUpu, plugUpu),
           maxValue = pmax(nilmUpu, plugUpu)) %>%
    summarise(
      TP = sum(nilmOn & plugOn, na.rm = TRUE),
      FN = sum(!nilmOn & plugOn, na.rm = TRUE),
      FP = sum(nilmOn & !plugOn, na.rm = TRUE),
      TN = sum(!nilmOn & !plugOn, na.rm = TRUE),
      nilmUsage = sum(nilmUpu, na.rm = TRUE),
      plugUsage = sum(plugUpu, na.rm = TRUE),
      minValueSum = sum(minValue, na.rm = TRUE),
      maxValueSum = sum(maxValue, na.rm = TRUE),
      usageRatio = max(0, 1 - abs(nilmUpu - plugUpu) /
                         plugUpu, na.rm = TRUE)
    ) %>%
    mutate(
      accuracy = (TP + TN) / (TP + TN + FP + FN),
      recall = TP / (TP + FN),
      precision = TP / (TP + FP),
      F1 = 2 * TP / (2 * TP + FP + FN),
      MR = minValueSum / maxValueSum
    ) %>%
    select(-minValueSum, -maxValueSum)
}

#' @importFrom dplyr mutate
setOnOff <- function(plugUsage,
                     nilmUsage,
                     plugThreshold,
                     nilmThreshold) {
  joinedUsage <-
    joinPlugNilmUsage(plugUsage, nilmUsage)

  joinedUsage %>%
    mutate(plugOn = plugUpu > plugThreshold,
           nilmOn = nilmUpu > nilmThreshold)
}

#' @importFrom magrittr %<>%
#' @importFrom dplyr rename inner_join
joinPlugNilmUsage <- function(plugUsage, nilmUsage) {
  plugUsage %<>% rename(plugUpu = unitPeriodUsage)
  nilmUsage %<>% rename(nilmUpu = unitPeriodUsage)

  nilmUsage %>%

    inner_join(plugUsage, by = "timestamp")
}
