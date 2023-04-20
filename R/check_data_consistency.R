#' Check Data Consistency Between Two Data Frames
#'
#' This function compares the column names and number of rows in two data frames and returns a message indicating whether the data is consistent or not.
#'
#' @param df1 First data frame to compare
#' @param df2 Second data frame to compare
#' @return A message indicating whether the data is consistent or not.
#' @examples
#' df1 <- data.frame(x = c(1,2,3), y = c(4,5,6))
#' df2 <- data.frame(x = c(1,2,3), y = c(4,5,6))
#' check_data_consistency(df1, df2)
#' # Data is consistent across the two sources.
#' df3 <- data.frame(a = c(1,2,3), b = c(4,5,6))
#' check_data_consistency(df1, df3)
#' # Data is not consistent across the two sources.
#' @export
check_data_consistency <- function(df1, df2) {
  # Compare the column names and number of rows in the two data frames
  if (all(colnames(df1) == colnames(df2)) && nrow(df1) == nrow(df2)) {
    message("Data is consistent across the two sources.")
  } else {
    message("Data is not consistent across the two sources.")
  }
}
