#'
#' Check the structure of the data
#'
#' This function checks the structure of the given data frame and prints the number of rows,
#' number of columns, column names, column data types, and number of missing values.
#'
#' @param df The data frame to be checked.
#'
#' @return None
#'
#' @examples
#' df <- data.frame(id = 1:10,
#' gender = c("male", "female", "male", "male", "male", "male", "male", "male", "female", "female"),
#' age = c(25, 32, 45, 19, 27, 56, 38, 42, 33, NA),
#' salary = c(50000, 60000, 75000, 45000, 55000, 90000, NA, 80000, 65000, 70000))
#'
#' # Check the data structure of the example dataframe
#' check_data_structure(df)
#'
#' @export
check_data_structure <- function(df) {
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  column_names <- names(df)
  column_data_types <- sapply(df, class)
  missing_values <- sum(is.na(df))
  message(paste0("Number of rows: ", n_rows))
  cat("\n")  # add an empty line
  message(paste0("Number of columns: ", n_cols))
  cat("\n")  # add an empty line
  message(paste0("Column names: ", paste(column_names, collapse=", ")))
  cat("\n")  # add an empty line
  message(paste0("Column data types: ", paste(column_data_types, collapse=", ")))
  cat("\n")  # add an empty line
  message(paste0("Number of missing values: ", missing_values))
}
