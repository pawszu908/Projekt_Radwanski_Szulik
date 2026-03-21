library(tidyverse)

HSI <- read.csv("data/HSI.csv")
N225 <-read.csv("data/N225.csv")

HSI <- HSI[HSI$date %in% N225$date, ]
N225 <- N225[N225$date %in% HSI$date, ]


