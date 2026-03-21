library(tidyverse)

# Wczytanie danych

HSI <- read.csv("data/HSI.csv") 
N225 <-read.csv("data/N225.csv")

# Daty występujące w obu plikach

HSI <- HSI[HSI$date %in% N225$date, ]
N225 <- N225[N225$date %in% HSI$date, ]

# Obliczenie dziennych stóp zwrotu

HSI$return <- c(NA, diff(HSI$close) / HSI$close[-length(HSI$close)])
N225$return <- c(NA, diff(N225$close) / N225$close[-length(N225$close)])

HSI <- na.omit(HSI) %>% select(date, close, return)
N225 <- na.omit(N225) %>% select(date, close, return)