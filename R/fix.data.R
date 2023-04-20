#' Fix data frame column and row names and remove symbols and blanks
#'
#' This function applies several data cleaning functions from the \code{fixr} package to a given data frame. The \code{fix_data_names}, \code{remove_spaces}, \code{remove_symbols_data}, and \code{replace_blanks_with_na} functions are used to add "X_" before column and row names that start with a number, remove leading/trailing spaces, remove non-alphanumeric characters from the data, replace spaces with underscores in column and row names, and replace empty string values with NAs, respectively.
#'
#' @param df A data frame to be processed.
#'
#' @return The cleaned data frame.
#'
#' @examples
#' df <- data.frame(" 1st col " = c("", "foo", ""), "2nd col" = c(" ", " ", "bar"),
#'                  "3rd col" = c(1, 2, 3))
#' fix.data(df)
#'
#' @export
fix.data <- function(df) {
  df <- fix_special_characters_in_names(df)
  df <- fix_data_names(df)
  df <- fix_special_characters_in_data(df)
  df <- fix_blanks_with_na(df)
  return(df)
}
