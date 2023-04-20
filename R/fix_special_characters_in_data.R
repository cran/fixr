#' Remove Non-Alphanumeric Characters from Data Frame
#'
#' This function removes non-alphanumeric characters from all non-numeric columns in a data frame. The columns are modified in-place.
#'
#' @param df A data frame.
#'
#' @return A modified data frame where all non-numeric columns have had non-alphanumeric characters removed.
#'
#' @examples
#' df <- data.frame(a = c("A*B", "C&D"), b = c("1.2", "3.4"))
#' fix_special_characters_in_data(df)
#' # Output:
#' #   a    b
#' # 1 AB  1.2
#' # 2 CD  3.4
#'
#' @export
fix_special_characters_in_data <- function(df) {
  df <- as.data.frame(df)
  # Loop through each column in the data frame
  for (i in 1:ncol(df)) {
    # Check if the column is numeric
    if (is.numeric(df[[i]])) {
      # If the column is numeric, skip it
      next
    }

    # Replace non-alphanumeric characters with an empty string, except for decimal points and negative signs
    df[[i]] <- gsub("[^[:alnum:].-]|(?<=\\d)-", "", df[[i]], perl = TRUE)

    # Convert the column to numeric if possible
    if (!any(is.na(as.numeric(df[[i]])))) {
      df[[i]] <- as.numeric(df[[i]])
    }
  }

  return(df)
}
