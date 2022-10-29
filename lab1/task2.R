get_id <- function(df) {
  new_df <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "id"), df)
  result <- data.frame(id=new_df[, 1], mean=rowMeans(new_df[, -1]))
  return (result[complete.cases(result), ])
}
load("data/lab1_e2.Rdata")
get_id(all_data)