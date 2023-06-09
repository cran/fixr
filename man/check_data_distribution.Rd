% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data_distribution.R
\name{check_data_distribution}
\alias{check_data_distribution}
\title{Check the data distribution of a data frame}
\usage{
check_data_distribution(df)
}
\arguments{
\item{df}{A data frame}
}
\value{
This function does not return anything, it only prints messages to the console.
}
\description{
This function checks if the data is normally distributed for each numeric column in a data frame.
}
\examples{
df <- data.frame(x = c("a", "b", "c"), y = c(4, 5, 6), z = c(7, 8, 9))

check_data_distribution(df)

}
