#' Check if a data frame contains negative values.
#'
#' This function checks if a data frame contains negative values and returns their indices if any are found.
#'
#' @param df The data frame to check for negative values.
#'
#' @return If negative values are found, the function returns their indices as an array index object.
#' If no negative values are found, NULL is returned.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c(-1, 0, 1))
#' check_for_negative_values(df)
#' # [1] "Data frame contains negative values."
#' #      row col
#' # [1,]   2   1"
#'
#' @export
check_for_negative_values <- function(df) {
  neg_values <- which(df < 0, arr.ind = TRUE)
  if (length(neg_values) > 0) {
    message("Data frame contains negative values.")
    return(neg_values)
  } else {
    message("Data frame does not contain negative values.")
    return(NULL)
  }
}
