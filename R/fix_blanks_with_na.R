#' Replace blanks with NA in a data frame
#'
#' This function replaces all empty string values ("") in a given data frame with NA values.
#'
#' @param df A data frame to be processed.
#'
#' @return The data frame with empty string values replaced with NAs.
#'
#' @examples
#' df <- data.frame(x = c("", "foo", ""), y = c("", "", "bar"), z = c(1, 2, 3))
#' fix_blanks_with_na(df)
#'
#' @export
fix_blanks_with_na <- function(df) {
  df[df == ""] <- NA
  return(df)
}
