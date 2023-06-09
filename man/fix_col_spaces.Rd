% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_column_spaces.R
\name{fix_col_spaces}
\alias{fix_col_spaces}
\title{Replace spaces in column names with underscores}
\usage{
fix_col_spaces(df)
}
\arguments{
\item{df}{A data frame}
}
\value{
A modified data frame with spaces in column names replaced by underscores.
}
\description{
This function takes a data frame as an argument and replaces all spaces in
the column names with underscores.
}
\examples{
my_data <- data.frame("Column Name 1" = c(1, 2, 3), "Column Name 2" = c(4, 5, 6))

fix_col_spaces(my_data)
# Returns a data frame with column names where spaces are replaced by underscores.

}
