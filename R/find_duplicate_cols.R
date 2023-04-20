#' Find Duplicate Columns
#'
#' This function takes a data frame as input and checks for duplicate columns.
#' A column is considered a duplicate of another column if all values in both columns are the same.
#' If any duplicate columns are found, the function prints a message indicating which columns are duplicates of which other columns.
#' If no duplicate columns are found, the function prints a message indicating that no duplicates were found.
#'
#' @param df A data frame
#' @return A message indicating which columns are duplicates of which other columns
#' @examples
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, NA, -6, 4), z = c(7, 8, 180, 7))
#' find_duplicate_cols(df)
#' # Column 'c' is a duplicate of column 'a'
#' @export
find_duplicate_cols <- function(df) {
  # Remove columns with no values in all cells
  df <- df[, colSums(is.na(df)) != nrow(df)]

  # Initialize empty vectors to store duplicate and original column names
  duplicate_cols <- c()
  original_cols <- c()

  # Loop through each column in the data frame
  for (i in 1:(ncol(df)-1)) {
    # Check if the column is a duplicate of any subsequent column
    for (j in (i+1):ncol(df)) {
      if (all(df[,i] == df[,j])) {
        # Store the names of the duplicate and original columns
        duplicate_cols <- c(duplicate_cols, names(df)[j])
        original_cols <- c(original_cols, names(df)[i])
      }
    }
  }

  # Check if any duplicates were found
  if (length(duplicate_cols) > 0) {
    # Print the names of the duplicate and original columns
    message("Duplicate columns found:")
    for (k in 1:length(duplicate_cols)) {
      message(paste0("Column '", duplicate_cols[k], "' is a duplicate of column '", original_cols[k], "'"))
    }
  } else {
    message("No duplicate columns found.")
  }
}
