# bootstrap
# Monte Carlo

data <- read.csv("dane3.csv")

weekly_sales <- data %>% group_by(week) %>% 
  summarise(sales=sum(units_sold))

mean(weekly_sales$sales)
hist(weekly_sales$sales)

# 99%
quantile(weekly_sales$sales, 0.99)

s <- sample(weekly_sales$sales, 100, replace = T)
hist(s)

# 99,9%
quantile(weekly_sales$sales, c(0.0005, 0.9995))

# Value at Risk
quantile(weekly_sales$sales, c(0.05))

# testy

a <- data[data$region == "PL-Central", "units_sold"]
b <- data[data$region == "PL-North", "units_sold"]

mean(a)
mean(b)


t.test(a, b)

m <- vector()

for(i in 1:1000){
  a_boot<-sample(a, 150, replace = T)
  b_boot<-sample(b, 150, replace = T)
 
   m[i]<- mean(a_boot)-mean(b_boot)
}

hist(m)
sum(m<0)/1000


# Monte Carlo

# osoby startują z punktu A o losowej porze (rozkład jednostajny)
# czas dotarcia do przystanku ma rozkład normalny (m = 10, s = 3)
# autobus przyjeżdża co 15 min (oo, 15, 30, 45) z opóźnieniem o rozkładzie poissona z lambda = 2
# podróż autobusem do punktu B ma rozkład normalny (m = 22, s = 2)
# ile średnio czasu trwa podróż z A do B [w minutach]

time <- vector()

set.seed(2345)
for(i in 1:1000){
  t0 <- round(runif(1, 0, 59))
  t1 <- t0 + round(rnorm(1, 10, 3))
  
  a <- floor(t1/60)
  b <- t1 %% 60
  
  bus <- rpois(1, 2)
  
  if(bus >= b %% 15){
    t2 <- a*60 + floor(b/15)*15 + bus
  } else{
    t2 <- a*60 + (floor(b/15)+1)*15 + bus
  }
  
  
  t3 <- t2 + round(rnorm(1, 22, 2))
  time[i] <- t3-t0
  
}

mean(time)
sqrt(var(time)/1000) # sd
hist(time)

quantile(time, 0.9)
quantile(time, 0.1)
quantile(time, c(0.05, 0.95))

# redukcja wariancji
# losowanie ujemnie skorelowanych wartości

u <- runif(100)
a <- qunif(u, 0, 59)
b <- qunif(1-u, 0, 59)
cor(a,b)

time <- vector()

set.seed(2345)
for(i in 1:500){
  # losowanie z unif(0,1)
  # rzutowanie do właściwego rozkładu dwóch przeciwnych kwantyli
  
  u <- runif(1)
  t_a <- qunif(u, 0, 59)
  t_b <- qunif(1-u, 0, 59)
  
  t_a1 <- t_a + round(rnorm(1, 10, 3))
  t_b1 <- t_b + round(rnorm(1, 10, 3))
  
  a1x <-floor(t_a1/60)
  a1y <- t_a1 %% 60
  
  b1x <- floor(t_b1/60)
  b1y <- t_b1 %% 60
  
  bus_a <- rpois(1, 2)
  bus_b <- rpois(1, 2)
  
  if(bus_a >= a1y %% 15){
    t_a2 <- a1x*60 + floor(a1y/15)*15 + bus_a
  } else{
    t_a2 <- a1x*60 + (floor(a1y/15)+1)*15 + bus_a
  }
  
  t_a3 <- t_a2 + round(rnorm(1, 22, 2))
  
  if(bus_b >= b1y %% 15){
    t_b2 <- b1x*60 + floor(b1y/15)*15 + bus_b
  } else{
    t_b2 <- b1x*60 + (floor(b1y/15)+1)*15 + bus_b
  }
  
  t_b3 <- t_b2 + round(rnorm(1, 22, 2))
  
  
  # czas wynikowy to średnia tych dwóch czasów
  time[i] <- mean(c(t_a3 - t_a, t_b3 - t_b))
  
  
}
mean(time)

hist(time)
var(time)/500
sqrt(var(time)/500)
