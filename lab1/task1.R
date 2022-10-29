fix_data <- function(df) {
  for (i in colnames(df)) {
    updated_string <- gsub(" ", "", df[[i]])
    if (length(updated_string) > 0) {
      for (j in 1:length(updated_string)) {
        if (!is.na(as.numeric(updated_string[j]))) {
          df[[i]][j] <- as.numeric(updated_string[j])
        }
      }
    }
  }
  return(df)
}

fix_data(read.csv("data/lab1_e1.csv"))
