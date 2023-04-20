#' Check if sample size is adequate
#'
#' This function checks if the sample size of a data frame is adequate for statistical analysis.
#'
#' @param df A data frame to be checked
#' @return A message indicating if the sample size is adequate or too small
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, 18, 7))
#' check_sample_size(df)
#'
#' @export
check_sample_size <- function(df) {
  # Check for missing values
  if (anyNA(df)) {
    message("Missing values detected in the data frame.")
    # Handle missing values appropriately
  } else {
    # Calculate the threshold based on the number of columns
    threshold <- 10 * ncol(df)
    # Check if the sample size is adequate
    if (nrow(df) < threshold) {
      message("Sample size may be too small.")
    } else {
      message("Sample size is adequate.")
    }
  }
}
