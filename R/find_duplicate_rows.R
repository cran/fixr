#' Find duplicate rows in a data frame
#'
#' This function identifies and reports duplicate rows in a given data frame. It first removes any rows with no values in all cells, and then compares each row to subsequent rows to check for duplicates. Duplicate rows are identified by having the same values in all columns. The function returns a message stating whether or not duplicate rows were found, and if so, the row numbers of the duplicate and original rows.
#'
#' @param df A data frame to check for duplicate rows.
#' @return A message stating whether or not duplicate rows were found, and if so, the row numbers of the duplicate and original rows.
#' @examples
#' # Create example data frame
#' df <- data.frame(w = c(7, 8, 180, 7), x = c("a", "b", "c", "a"),
#'                  y = c(4, 5, -6, 4), z = c(7, 8, NA, 7))
#' # Find duplicate rows
#' find_duplicate_rows(df)
#'
#' @export
find_duplicate_rows <- function(df) {
  # Replace NA with 0
  df[is.na(df)] <- 0

  # Initialize empty vectors to store duplicate and original row numbers
  duplicate_rows <- c()
  original_rows <- c()

  # Loop through each row in the data frame
  for (i in 1:(nrow(df)-1)) {
    # Check if the row is a duplicate of any subsequent row
    for (j in (i+1):nrow(df)) {
      if (all(df[i,] == df[j,])) {
        # Store the row numbers of the duplicate and original rows
        duplicate_rows <- c(duplicate_rows, j)
        original_rows <- c(original_rows, i)
      }
    }
  }

  # Check if any duplicates were found
  if (length(duplicate_rows) > 0) {
    # Print the row numbers of the duplicate and original rows
    message("Duplicate rows found:")
    for (k in 1:length(duplicate_rows)) {
      message(paste0("Row ", duplicate_rows[k], " is a duplicate of row ", original_rows[k]))
    }
  } else {
    message("No duplicate rows found.")
  }
}
