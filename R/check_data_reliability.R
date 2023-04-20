#' Check inter-rater or test-retest reliability between numeric columns
#'
#' This function checks for inter-rater or test-retest reliability between all
#' pairs of numeric columns in a data frame by computing the correlation between
#' each pair and reporting if it is less than 0.8.
#'
#' @param df A data frame
#'
#' @return A message indicating whether the data is reliable or not between each
#'         pair of columns.
#'
#' @examples
#' df <- data.frame(x = c("a", "b", "c"), y = c(4, 5, 6), z = c(7, 8, 180))
#' check_data_reliability(df)
#'
#' @importFrom stats cor
#' @export
check_data_reliability <- function(df) {
  # Check for inter-rater or test-retest reliability between all pairs of numeric columns in the data frame
  cols <- which(sapply(df, is.numeric))
  num_cols <- length(cols)
  for (i in 1:(num_cols - 1)) {
    for (j in (i + 1):num_cols) {
      if (cor(df[, cols[i]], df[, cols[j]]) < 0.8) {
        message(paste0("Data may not be reliable between columns ", cols[i], " and ", cols[j], "."))
      } else {
        message(paste0("Data is reliable between columns ", cols[i], " and ", cols[j], "."))
      }
    }
  }
}
