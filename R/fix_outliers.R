#' Remove Outliers from a Data Frame
#'
#' This function removes outlier rows from a data frame by identifying rows with
#' values that are more than 2 standard deviations away from the mean in any column.
#'
#' @param df A data frame to clean
#'
#' @return A cleaned data frame with outlier rows removed
#'
#' @importFrom stats sd
#'
#' @examples
#' df <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
#'                  y = c(1,1,1,1,1,1,1,100,1,1))
#' fix_outliers(df)
#'
#' @export
# Define a function to remove outliers from a dataframe
fix_outliers <- function(df) {
  # Check for outliers or extreme values in the dataframe
  check_outliers(df)

  # Identify the numeric columns
  numeric_cols <- sapply(df, is.numeric)

  # Calculate the mean and standard deviation for each numeric column
  means <- sapply(df[, numeric_cols], mean, na.rm = TRUE)
  sds <- sapply(df[, numeric_cols], sd, na.rm = TRUE)

  # Identify the rows with values more than 2 SDs away from the mean
  outlier_rows <- apply(abs(t(t(df[, numeric_cols]) - means)) > 2 * sds, 1, any)

  # Remove the outlier rows from the data frame
  df <- df[!outlier_rows, ]

  # Return the cleaned data frame
  return(df)
}


