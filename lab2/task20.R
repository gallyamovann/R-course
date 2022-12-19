#Напишите функцию, 
#которая на вход принимает название Штата, 
#название медицинской процедуры/заболевания 
#и максимальную сумму, которую пациент готов потратить на лечение ( в долларах).
#На выход функция должна выдавать список, 
#в котором указаны название больницы, город и адрес больницы 
#и диапазон стоимости необходимой услуги. 
#Список должен быть упорядочен по возрастанию среднего значения стоимости услуги. 
#Если на данную сумму невозможно получить данную услугу, 
#то функция должна выдать сообщение, 
#в котором указана минимальная стоимость услуги в штате.
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
    return (paste("You need - ", min(data$Lower.Estimate)))
  } else {
    data <- data[rows,]
    data <- data %>% select(Facility.Name, City, Address, Lower.Estimate, Higher.Estimate, AvgPayment)
    return(data)
  }
}

data <- read.csv('data/Payment_and_Value_of_Care-Hospital.csv')
result <- getList(data, "AL", "pneumonia", 2000)
print(result)


