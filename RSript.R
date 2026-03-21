library(tidyverse)

HSI <- read.csv("data/HSI.csv")
N225 <-read.csv("data/N225.csv")

HSI <- inner_join(HSI, N225, by = "date")

N225 <- inner_join(N255, HSI, by = "date")