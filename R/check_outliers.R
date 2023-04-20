#' Check for Outliers or Extreme Values in Data
#'
#' This function checks for outliers or extreme values in a given dataframe.
#'
#' @param df A dataframe.
#'
#' @return A message indicating whether or not extreme values were found.
#'
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, NA, 7))
#'
#' check_outliers(df)
#'
#' @export
#'
check_outliers <- function(df) {
  # Get the names of numerical columns
  num_cols <- sapply(df, is.numeric)

  # Check for outliers or extreme values only in numerical columns
  if (any(abs(df[, num_cols]) > 100)) {
    message("Extreme values found in the numerical columns of the data.")
  } else {
    message("No extreme values found in the numerical columns of the data.")
  }
}

