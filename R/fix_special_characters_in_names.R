#' Remove Special Characters from Data Frame Column and Row Names
#'
#' This function removes any non-alphanumeric characters from both the row and
#' column names of a given data frame.
#'
#' @param df A data frame with non-alphanumeric characters in the column or row
#' names.
#'
#' @return A data frame with all non-alphanumeric characters removed from the
#' column and row names.
#'
#' @examples
#' df <- data.frame("Col1!" = c(1, 2, 3), "Col2?" = c(4, 5, 6))
#' rownames(df) <- c("Row1@", "Row2#", "Row3$")
#' fix_special_characters_in_names(df)
#'
#' @export
fix_special_characters_in_names <- function(df) {
  # Convert to data frame
  df <- as.data.frame(df)

  # Remove non-alphanumeric characters from row names
  rownames(df) <- gsub("[^[:alnum:]]", "", rownames(df))

  # Remove non-alphanumeric characters from column names
  colnames(df) <- gsub("[^[:alnum:]]", "", colnames(df))

  # Return the modified data frame
  return(df)
}
