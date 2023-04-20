#' Fix row names of a data frame
#'
#' This function removes any leading "X." or "X" from the row names of a data frame, replaces any "." with "_", removes any leading or trailing symbols and spaces, and ensures that there is only one underscore between two words. Additionally, if there are duplicate row names, the function appends a number to each duplicate row name to make it unique.
#'
#' @param data a data frame with improperly formatted row names
#'
#' @return a modified data frame with fixed row names
#'
#' @importFrom stats ave
#'
#' @examples
#' my_data <- data.frame(" Col1" = c(1, 2, 3), "Col.2" = c(4, 5, 6), check.names = FALSE)
#' rownames(my_data) <- c(" Row1", " Row.2", "Row.3 ")
#' fix_row_names(my_data)
#'
#' @export
fix_row_names <- function(data){
  # remove "X." or "X" from the beginning of row names
  rownames(data) <- gsub("^X[.]?\\s*", "", rownames(data))
  # replace "." with "_" and remove leading/trailing symbols and spaces
  rownames(data) <- gsub("[^[:alnum:]_]+", "_", rownames(data))
  rownames(data) <- gsub("^_+|_+$", "", rownames(data))
  rownames(data) <- gsub("_{2,}", "_", rownames(data))
  # add number to duplicated row names
  if(any(duplicated(rownames(data)))) {
    nums <- ave(seq_along(rownames(data)), rownames(data), FUN=seq_along)
    rownames(data) <- paste0(rownames(data), "_", nums)
  }
  return(data)
}
