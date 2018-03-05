AccuracyListForSummary <- c("usageRatio",
                            "accuracy",
                            "recall",
                            "precision",
                            "F1",
                            "MR")

#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
getNilmAccuracySummary <- function(accuracyTable) {
  accuracySummary <-
    map(AccuracyListForSummary,
        getAccuracySummary,
        accuracyTable = accuracyTable)

  names(accuracySummary) <- AccuracyListForSummary

  accuracySummary %>%
    bind_rows(.id = "ID")
}

getAccuracySummary <- function(accuracyTable, type) {
  if (type %in% names(accuracyTable)) {
    accSummary <- accuracyTable[, type] %>%
      unlist() %>%
      summaryAsVector()
    return(accSummary)
  } else {
    accSummary <- NULL
  }
  accSummary
}

summaryAsVector <- function(x, digits = 2L) {
  na.omit(x) %>%
    summary(digits = digits) %>%
    as.list() %>%
    as.data.frame()
}
