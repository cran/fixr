find_duplicate_cols <- function(df) {
  # Remove columns that are entirely NA
  df <- df[, colSums(!is.na(df)) > 0]
  
  # Store duplicate pairs
  duplicates <- list()
  
  # Compare columns efficiently
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i + 1):ncol(df)) {
      if (identical(df[[i]], df[[j]])) {  # `identical()` properly handles NA values
        duplicates[[names(df)[j]]] <- names(df)[i]
      }
    }
  }
  
  # Print and return structured output
  if (length(duplicates) > 0) {
    message("Duplicate columns found:")
    for (dup in names(duplicates)) {
      message(paste0("Column '", dup, "' is a duplicate of column '", duplicates[[dup]], "'"))
    }
    return(as.data.frame(duplicates))
  } else {
    message("No duplicate columns found.")
    return(NULL)
  }
}

