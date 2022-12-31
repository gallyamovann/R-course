# task 5
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
  data_col_imp <- new_tmp[, grepl("Импорт", colnames(new_tmp))]
  data_col_exp <- new_tmp[, grepl("Экспорт", colnames(new_tmp))]
  tmp <- cbind(new_tmp[, 1], data_col_exp)
  full_table <- cbind(tmp, data_col_imp)
  full_table['СуммаИмпорта'] <- rowSums(data_col_imp, na.rm = TRUE)
  full_table['СуммаЭкспорта'] <- rowSums(data_col_exp, na.rm = TRUE)
  return(full_table)
}

load("data/ExpImp.RData")
expimp <- getChangedData()

getGreaterExp <- function(data) {
  rows <- which(data$СуммаЭкспорта > data$СуммаИмпорта)
  return(data[rows, 1])
}

data <- getGreaterExp(expimp)
print(data)

# task 20
library(dplyr)

getList <- function(data, state, procedure, money) {
  rows <- which(data$State == state & data$Payment.Measure.Name == paste("Payment for", procedure, "patients"))
  data <- data[rows,]
  data$Lower.EstimateTmp <- as.numeric(gsub("[$,]", "", data$Lower.Estimate))
  data$Higher.EstimateTmp <- as.numeric(gsub("[$,]", "", data$Higher.Estimate))
  data <- data %>% select(Facility.Name,
                          City,
                          Address,
                          Lower.Estimate,
                          Higher.Estimate,
                          Lower.EstimateTmp,
                          Higher.EstimateTmp)
  data$AvgPayment <- rowMeans(data[, c('Lower.EstimateTmp', 'Higher.EstimateTmp')], na.rm = TRUE)
  data <- data[order(data['AvgPayment'], na.last = TRUE, decreasing = FALSE),]
  rows <- which(data$Lower.EstimateTmp < money)
  if (length(rows) == 0) {
    return(paste("You need - ", min(data$Lower.Estimate)))
  } else {
    data <- data[rows,]
    data <- data %>% select(Facility.Name, City, Address, Lower.Estimate, Higher.Estimate, AvgPayment)
    return(data)
  }
}

data <- read.csv('data/Payment_and_Value_of_Care-Hospital.csv')
result <- getList(data, "AL", "pneumonia", 2000)
print(result)


