library(tidyverse)

# Wczytanie danych

RUT <- read.csv("data/RUT.csv") 
IXIC <-read.csv("data/IXIC.csv")

# Daty występujące w obu plikach

RUT <- RUT[RUT$date %in% IXIC$date, ]
IXIC <- IXIC[IXIC$date %in% RUT$date, ]

# Obliczenie dziennych stóp zwrotu

RUT$return <- c(NA, diff(RUT$close) / RUT$close[-length(RUT$close)])
IXIC$return <- c(NA, diff(IXIC$close) / IXIC$close[-length(IXIC$close)])

RUT <- na.omit(RUT) %>% select(date, close, return)
IXIC <- na.omit(IXIC) %>% select(date, close, return)