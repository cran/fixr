#' Fix row and column names of a data frame
#'
#' This function fixes the row and column names of a data frame by removing leading and trailing spaces, replacing spaces with underscores, and modifying duplicate names.
#'
#' @param df A data frame to be fixed
#' @return A fixed data frame with modified row and column names
#' @examples
#' my_data <- data.frame(" Col1" = c(1, 2, 3), "Col.2" = c(4, 5, 6), check.names = FALSE)
#' rownames(my_data) <- c(" Row1", " Row.2", "Row.3 ")
#' fix_column_names(fix_row_names(my_data))
#'
#' @export
fix_data_names <- function(df) {
  df <- fix_row_names(df)
  df <- fix_column_names(df)
  return(df)
}
