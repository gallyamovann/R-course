# task 1
fix_data <- function(df) {
  for (i in colnames(df)) {
    updated_string <- gsub(" ", "", df[[i]])
    for (j in seq_along(updated_string)) {
      if (!is.na(as.numeric(updated_string[j]))) {
        df[[i]][j] <- as.numeric(updated_string[j])
      }
    }
  }
  return(df)
}

fix_data(read.csv("data/lab1_e1.csv"))

# task 2
get_id <- function(df) {
  new_df <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "id"), df)
  result <- data.frame(id=new_df[, 1], mean=rowMeans(new_df[, -1]))
  return (result[complete.cases(result), ])
}

load("data/lab1_e2.Rdata")
get_id(all_data)