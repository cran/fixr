#' Check the data distribution of a data frame
#'
#' This function checks if the data is normally distributed for each numeric column in a data frame.
#'
#' @param df A data frame
#'
#' @return This function does not return anything, it only prints messages to the console.
#'
#' @examples
#' df <- data.frame(x = c("a", "b", "c"), y = c(4, 5, 6), z = c(7, 8, 9))
#'
#' check_data_distribution(df)
#'
#' @importFrom stats shapiro.test
#'
#' @export
check_data_distribution <- function(df) {
  # Check if the data is normally distributed for each column
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      if (shapiro.test(df[[col]])$p.value < 0.05) {
        message(paste0(col, " column is not normally distributed."))
      } else {
        message(paste0(col, " column is normally distributed."))
      }
    } else {
      message(paste0(col, " column is not numeric."))
    }
  }
}
