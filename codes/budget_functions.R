# Функции для обработки данных Федерального казначейства (roskazna.ru)

library(rvest)
library(readxl)
library(XLConnect)
library(stringr)
library(zoo)
library(ggplot2)

months <- c('января', 'февраля', 'марта', "апреля", "мая", "июня", "июля", "августа", "сентября", "октября", "ноября", "декабря")
years <- as.character(1990:2020)

# преобразовать стоку в число, убрав пробели и заменив точку на запятую 
ConvertToNumeric <- function(str) {
   str <- gsub(',', '.',str)
   str <- gsub('\\s', '', str)
   str <- as.numeric(str)
   return(str)
}


# преобразовать текстовое название даты в объект yearmon (пред месяц)
GetDate <- function(date){
   mm <- which(str_detect(date, pattern=months)) # определить номер в векторе месяцев
   yy <- as.numeric(years[(str_detect(date, pattern=years))]) # получить год
   date<- as.Date(ISOdate(yy, mm, "01"))
   date <- as.yearmon(date)-1/12 # так как значение на 1 число, необходимо вернуться на месяц назад 
   return(date)
}

# Функция получить данные из file и листа sheet, поделить на dim (млрд по)
GetBudgetFromFile <- function(file, sheet, dim = 10^9){
   
   df <- readWorksheetFromFile(file, sheet = sheet, endCol =3,
                               colTypes = c('character', 'character', 'numeric'))
   df[,3] <- (df[,3])/10^9
   date <- GetDate(df[2,1]) #преобразовать текстовую ячейку в дату
   
   if (length(date) == 0L) {
      stop("Error with configuring date of ", file)
   }
   
   names(df) <- col_names
   df$date <- date
   df <- df[-(1:which(df[,1] =='1')),] # удалить верхнюю часть таблицы
   return(df)
}


# cобрать бюджетные данные из файлы в папке 
ArrangeBudgetData <- function(path, years, sheet){
   col_names <- c('name', 'kbk', 'value')
   buff2 <- as.data.frame(matrix(nrow =0, ncol = length(col_names), dimnames = list(NULL, col_names)))
   
   for (year in years){
      folder <- paste0(path, year, '/')
      files <- list.files(folder)
      files <- files[!str_detect(files,pattern = '~')] # убрать временные файлы из списка
      for (file in files){
         ff <- paste0(folder, file)
         buff2 <- rbind(buff2, GetBudgetFromFile(file = ff, sheet))
         #GetBudgetFromFile(ff, sheet)
         
      }
   }
   
   buff2$date <- as.yearmon(buff2$date)
   return(buff2)
}

# пересчитать из "накопленным итогом с начала год" в месячные данные

AccumToMonthly <- function(data){
   dates <- (levels(as.factor(data$date)))
   print(dates)   
   col_names <- c('name', 'kbk', 'value')
   buff2 <- as.data.frame(matrix(nrow =0, ncol = length(col_names), dimnames = list(NULL, col_names)))

   for (i in 2:length(dates)) {
      
      dt <- as.yearmon(dates[i])
      # для первого месяца оставить как есть 
      if (as.numeric(format(dt, '%m'))==1){
         current_month <- data[data$date == dt,]
         buff2 <- rbind(buff2, current_month)
         
      }
      # для последующих - вычесть из предыдущего
      else{
         current_month <- data[data$date == dt,]
         last_month <- data[data$date == dt-1/12,]
         df <- merge(current_month, last_month, all.x= TRUE,all.y=TRUE, by = 'kbk')
         df$value <- df$value.x - df$value.y
         df <- df[,c('kbk', 'name.x', "value")]
         df <- df[!is.na(df$value),]
         names(df) <- c('kbk', 'name', 'value')
         df$date <- dt
         buff2 <- rbind(buff2, df)
      }
   }
   return(buff2)
}