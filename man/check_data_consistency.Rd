% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data_consistency.R
\name{check_data_consistency}
\alias{check_data_consistency}
\title{Check Data Consistency Between Two Data Frames}
\usage{
check_data_consistency(df1, df2)
}
\arguments{
\item{df1}{First data frame to compare}

\item{df2}{Second data frame to compare}
}
\value{
A message indicating whether the data is consistent or not.
}
\description{
This function compares the column names and number of rows in two data frames and returns a message indicating whether the data is consistent or not.
}
\examples{
df1 <- data.frame(x = c(1,2,3), y = c(4,5,6))
df2 <- data.frame(x = c(1,2,3), y = c(4,5,6))
check_data_consistency(df1, df2)
# Data is consistent across the two sources.
df3 <- data.frame(a = c(1,2,3), b = c(4,5,6))
check_data_consistency(df1, df3)
# Data is not consistent across the two sources.
}
