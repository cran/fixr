#' Remove duplicate columns from a data frame
#'
#' This function removes duplicate columns from a data frame.
#'
#' @param df A data frame
#'
#' @return A data frame with duplicate columns removed
#'
#' @examples
#' df <- data.frame(a = c(1, 1, 2), b = c(2, 2, 3))
#' fix_duplicate_cols(df)
#'
#'
#' @export
fix_duplicate_cols <- function(df) {
  # Remove columns with no values in all cells
  df <- df[, colSums(is.na(df)) != nrow(df)]

  # Identify duplicate columns
  duplicate_cols <- duplicated(t(df))

  # Remove duplicate columns
  df <- df[, !duplicate_cols]

  # Return the cleaned data frame
  return(df)
}
