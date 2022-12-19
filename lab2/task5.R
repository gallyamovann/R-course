getChangedData <- function() {
  new_tmp <- ExpImp
  for (i in 2:ncol(new_tmp)) {
    str <- new_tmp[[i]]
    for (j in seq_along(str)) {
      if (is.na(str[j])) {
        new_tmp[[i]][j] <- 0
      } else if (str[j] == "-") {
        new_tmp[[i]][j] <- 0
      }
      new_tmp[[i]] <- as.numeric(new_tmp[[i]])
    }
  }
  data_col_imp <- new_tmp[,grepl("Импорт", colnames(new_tmp))]
  data_col_exp <- new_tmp[,grepl("Экспорт", colnames(new_tmp))]
  tmp <- cbind(new_tmp[, 1], data_col_exp)
  full_table <- cbind(tmp, data_col_imp)  
  full_table['СуммаИмпорта'] <- rowSums(data_col_imp, na.rm = TRUE)
  full_table['СуммаЭкспорта'] <- rowSums(data_col_exp, na.rm = TRUE)
  return (full_table)
}

load("data/ExpImp.RData")
expimp <- getChangedData()
print(expimp)

getGreaterExp <- function(data) {
  rows <- which(data$СуммаЭкспорта > data$СуммаИмпорта)
  return (data[rows, 1])
}

data <- getGreaterExp(expimp)
print(data)



