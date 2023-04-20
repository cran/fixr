#' Remove duplicate rows from a data frame
#'
#' This function removes duplicate rows from a data frame.
#'
#' @param df A data frame
#'
#' @return A data frame with duplicate rows removed
#'
#' @examples
#' df <- data.frame(a = c(1, 1, 2), b = c(2, 2, 3))
#' fix_duplicate_rows(df)
#'
#'
#' @export
fix_duplicate_rows <- function(df) {
  # Remove rows with no values in all cells
  df <- df[rowSums(is.na(df)) != ncol(df), ]

  # Identify duplicate rows
  duplicate_rows <- duplicated(df)

  # Remove duplicate rows
  df <- df[!duplicate_rows, ]

  # Return the cleaned data frame
  return(df)
}
