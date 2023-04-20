#' Fill missing values in alphanumeric columns
#'
#' This function imputes missing values in alphanumeric columns of a data frame. If a column
#' is numeric, missing values are imputed with the column mean. Otherwise, missing values
#' are imputed with the column mode (most common value).
#'
#' @param df A data frame with missing values.
#' @return A data frame with imputed missing values.
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", NA),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, NA, 7))
#' fix_missing_alphanumeric_values(df)
#'
#' @export
fix_missing_alphanumeric_values <- function(df) {
  # Find the columns with missing values
  missing_cols <- sapply(df, function(x) sum(is.na(x))) > 0

  # Loop through the columns with missing values
  for (col in names(df)[missing_cols]) {
    # Check if the column is numeric
    if (is.numeric(df[[col]])) {
      # Impute the missing values with the column mean
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    } else {
      # Impute the missing values with the column mode
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
    }
  }
  # Return the imputed data frame
  return(df)
}
