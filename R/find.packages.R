#' Find R packages that can import a file format
#'
#' This function searches the CRAN repository for R packages that can be used to import a file format
#'
#' @param file_extension The file extension for the file format to search for packages to import
#'
#' @return A character vector of package names that can be used to import the file format
#' @export
find.packages <- function(file_extension) {

  # Check if the internet connection is available
  if (!RCurl::url.exists("https://cran.r-project.org")) {
    return("The internet connection is not available. Please check your internet connection.")
  }

  # Search the CRAN repository for packages that can import the file format
  search_url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
  search_params <- list(pkg_name = file_extension)
  search_result <- httr::GET(search_url, query = search_params)

  # Check if there are any suggested packages
  if (grepl("No packages found matching", httr::content(search_result, encoding = "UTF-8"))) {
    return(paste("Sorry, no packages found that can be used to import the file format", file_extension))
  }

  # Extract the suggested packages from the search result
  packages <- gsub(".*<pre>", "", httr::content(search_result, encoding = "UTF-8"))
  packages <- gsub("</pre>.*", "", packages)
  packages <- strsplit(packages, "\n")[[1]]
  packages <- packages[grep(file_extension, packages, fixed = TRUE)]
  packages <- gsub("^ +| +$", "", packages)

  # input string
  string <- packages

  # extract the words between /packages/ and /index.html\
  result <- gsub('.*packages/(.*)/index.html.*', '\\1', string)

  # remove any other lines from result
  result <- result[grep("^[[:alnum:]]*$", result)]

  return(paste("The following R packages can be used to import the file format", file_extension, ": ",
               paste(result, collapse = ", ")))
}
