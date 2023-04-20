#' Fix Column Names
#'
#' This function removes "X." or "X" from the beginning of column names and replaces any "." with "_". It also removes leading/trailing symbols and spaces, and ensures that there is only one underscore between two words. If there are duplicate column names, it appends a number to each duplicate column name to make it unique.
#'
#' @param data A data frame with improperly formatted column names.
#'
#' @return The modified data frame with fixed column names.
#'
#' @importFrom stats ave
#'
#' @examples
#' my_data <- data.frame(" Col1" = c(1, 2, 3), "Col.2" = c(4, 5, 6), check.names = FALSE)
#' fix_column_names(my_data)
#'
#' @export
fix_column_names <- function(data){
  # remove "X." or "X" from the beginning of column names
  colnames(data) <- gsub("^X[.]?\\s*", "", colnames(data))
  # replace "." with "_" and remove leading/trailing symbols and spaces
  colnames(data) <- gsub("[^[:alnum:]_]+", "_", colnames(data))
  colnames(data) <- gsub("^_+|_+$", "", colnames(data))
  colnames(data) <- gsub("_{2,}", "_", colnames(data))
  # add number to duplicated column names
  if(any(duplicated(colnames(data)))) {
    nums <- ave(seq_along(colnames(data)), colnames(data), FUN=seq_along)
    colnames(data) <- paste0(colnames(data), "_", nums)
  }
  return(data)
}
