#' Check for Missing Values in Data Frame
#'
#' This function checks for missing values in a data frame and prints out the names of the columns with missing values and their counts.
#'
#' @param df A data frame to check for missing values.
#'
#' @return A message indicating if missing values were found or not.
#'
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, NA, 7))
#' check_missing_values(df)
#'
#' @export
check_missing_values <- function(df) {
  # Convert df to a matrix
  df_mat <- as.matrix(df)

  # Count the number of missing values in each column
  missing_values <- colSums(is.na(df_mat))

  # Check if any columns have missing values
  if (sum(missing_values) > 0) {
    # Print the names of the columns with missing values and their counts
    message("Missing values found in the following columns:")
    for (i in 1:length(missing_values)) {
      if (missing_values[i] > 0) {
        message(paste0(names(missing_values)[i], ": ", missing_values[i]))
      }
    }
  } else {
    message("No missing values found.")
  }
}

