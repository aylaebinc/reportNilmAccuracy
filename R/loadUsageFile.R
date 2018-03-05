#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom dplyr group_by summarise_at vars
#' @export
loadPlugUsage <- function(rootPath,
                          homeSn,
                          year,
                          month,
                          applianceName) {
  paths <-
    findPlugUsageFilePath(rootPath,
                          homeSn,
                          year,
                          month,
                          applianceName)

  if (length(paths) == 0L)
    return(tibble())

  aggregateUsages(paths)
}

#' @importFrom readr read_csv cols col_character col_double col_integer
aggregateUsages <- function(paths) {
  map_df(
    paths,
    read_csv,
    col_types = cols(
      serialNumber = col_character(),
      timestamp = col_double(),
      unitPeriodUsage = col_double(),
      typeid = col_integer()
    )
  ) %>%
    group_by(timestamp) %>%
    summarise_at(vars(unitPeriodUsage), sum)
}

#' @importFrom purrr map_df
#' @importFrom dplyr group_by summarise_at vars
#' @export
loadNilmUsage <- function(rootPath,
                          homeSn,
                          year,
                          month,
                          applianceName) {
  paths <-
    findNilmUsageFilePath(rootPath,
                          homeSn,
                          year,
                          month,
                          applianceName)

  if (length(paths) == 0L)
    return(tibble())

  aggregateUsages(paths)
}
