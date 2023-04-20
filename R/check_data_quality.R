#' Check Data Quality
#'
#' This function performs a series of data quality checks on a given dataframe, including checking the data structure, missing values, data accuracy, negative values, outliers, sample size, duplicate rows, and duplicate columns.
#'
#' @param df A dataframe.
#'
#' @return A message indicating the results of each data quality check.
#'
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, NA, -6, 4), z = c(7, 8, 180, 7))
#'
#' # Check the data quality of the example dataframe
#' check_data_quality(df)
#'
#' @export
check_data_quality <- function(df) {
  df <- as.data.frame(df)
  check_data_structure(df)
  cat("\n")
  check_missing_values(df)
  cat("\n")
  check_for_negative_values(df)
  cat("\n")
  check_outliers(df)
  cat("\n")
  check_sample_size(df)
  cat("\n")
  find_duplicate_rows(df)
  cat("\n")
  find_duplicate_cols(df)
}
