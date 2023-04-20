#' Replace spaces in column names with underscores
#'
#' This function takes a data frame as an argument and replaces all spaces in
#' the column names with underscores.
#'
#' @param df A data frame
#'
#' @return A modified data frame with spaces in column names replaced by underscores.
#'
#' @examples
#' my_data <- data.frame("Column Name 1" = c(1, 2, 3), "Column Name 2" = c(4, 5, 6))
#'
#' fix_col_spaces(my_data)
#' # Returns a data frame with column names where spaces are replaced by underscores.
#'
#' @export
#'
fix_col_spaces <- function(df) {
  df <- as.data.frame(df)
  names(df) <- gsub(" ", "_", names(df))
  return(df)
}
