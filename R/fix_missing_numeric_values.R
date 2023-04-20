#' fill_missing_numeric_values
#'
#' A function to fill missing values in numeric columns of a data frame with the mean of the column.
#'
#' @param df A data frame with missing values.
#'
#' @return A data frame with missing numeric values filled with the column mean.
#'
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "d"),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, NA, 7))
#' fix_missing_numeric_values(df)
#'
#' @export
fix_missing_numeric_values <- function(df) {
  # Find the columns with missing values
  missing_cols <- sapply(df, function(x) sum(is.na(x))) > 0

  # Loop through the columns with missing values
  for (col in names(df)[missing_cols]) {
    # Check if the column is numeric
    if (is.numeric(df[[col]])) {
      # Impute the missing values with the column mean
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    }
  }
  # Return the imputed data frame
  return(df)
}
