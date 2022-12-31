# task5
library(ggplot2)
library(dplyr)

# получаем слитые данные
get_merged_data <- function(data) {
  df <- data.frame(Reduce(function(x, y) rbind(x, y), data))
  df <- df[,!names(df) %in% c("geo")]
  return(df)
}

# загружаем данные
load('data/trades.RData')
data <- get_merged_data(trades)

# фильтруем по импорту / экспорту
import <- data %>% filter(indic_et == 'Imports in million of ECU/EURO')
export <- data %>% filter(indic_et=='Exports in million of ECU/EURO')

# находим основных партнеров
main_partners <- export[export$values >= 20000, ]
main_partners <- main_partners %>%
  group_by(partner, sitc06) %>%
  summarise(values = sum(values, na.rm = TRUE))

# строим график для основных партнеров
ggplot(main_partners, aes(x = partner, y = values, fill = sitc06)) +
  geom_col(width = 1) +
  xlab('Страны-партнеры') +
  ylab('Объем экспорта') +
  ggtitle('Ведущие страны по экспорту') +
  geom_text(aes(label = values), size=2.5, position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name = 'Экспортируемые товары') +  coord_polar("y", 0)

# находим доли экспортируемых товаров
share <- data %>% filter(indic_et == 'Share of exports by partner (%)')
share_data <- cbind(export, share = share$values)
dates <- unique(share_data$time)

# строим график
plot_percent <- function(data) {
  return (ggplot(data, aes(x = partner, y = values, fill = sitc06)) +
    geom_col(width = 1) +
    xlab('Страны-партнеры') +
    ylab('Объем экспорта') +
    ggtitle(paste('Ведущие страны по экспорту в Европе ', substring(dates[i],1,4), ' г.')) +
    scale_fill_discrete(name = 'Экспортируемые товары') +
    geom_text(aes(label = share), size=2.5, position = position_stack(vjust = 0.5)) + coord_polar("y", 0))
}

for (i in length(dates)) {
  export_data = subset(share_data, share_data$time == dates[i] & share_data$values >= 20000)
  plot_percent(export_data)  
}

# task 10
library(tidyr)
library(ggplot2)
library(dplyr)

# считываем данные
data <- read.csv("data/reforest.csv")

# переводим "численные строки" в численные значения
for (i in 2:length(names(data))) {
  data[[i]] <- as.numeric(data[[i]])
}

# убираем пропущенные значения
data_complete_cases <- data[complete.cases(data),]

# находим строки в колонке Регион, в которых есть подстрока "федерельный округ"
num_start_fed <- grepl('федеральный округ', data_complete_cases$Region)

# получаем индексы, с которых начинается каждый федеральный округ в таблице
numbers <- which(num_start_fed == "TRUE")

# выберем федеральный округ
num <- 3

# получим начало строк и конец строк для выбранного федерального округа
start <- numbers[num] + 1
end <- numbers[num + 1] - 1

# составим новую таблицу из строк, выбранных для определенного федерального округа
new_tb <- data_complete_cases[start:end, ]

# переведем данные в формат
new_data <- new_tb %>%
  pivot_longer(cols = !Region, names_to = "Year", values_to = "Value")

# получим суммарные значения
new <- new_data %>% group_by(Region) %>% mutate(Sum = sum(Value))

# получим value с минимальным суммарным значением
region <- new[which.min(new$Sum),]
name_of_region <- region$Region
new <-new[order(new$Region, decreasing=FALSE),]

# создадим цветовую палитру
dfcolors <- data.frame(color='yellow', vec = unique(new$Region), stringsAsFactors=F)

# поменяем цвет у минимального суммарного значения на красный
dfcolors$color[dfcolors$vec==name_of_region] <-'red'

values <- new[which(new$Region == region$Region), ]

# строим график
ggplot(new, aes(x=Year, y=Value, color = Region)) +
  geom_line(aes(group = Region), size=1) +
  geom_point(aes(group = Region), size=1) +
  xlab("Год") +
  ylab("Площадь, га") +
  scale_color_manual(values = dfcolors$color, labels=dfcolors$vec)+
  geom_text(aes(label = Value), show_guide=F, vjust=-0.4,
            position = position_dodge(width = 0.7),size=3, color = "black")