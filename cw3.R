# modele ekonometryczne

data <- read.csv('dane3.csv')

model <- lm(units_sold ~ price_unit + is_holiday_week + is_winter + promo_rate, data)

summary(model)

# library(lmtest)
# install.packages('lmtest')

dwtest(model)
bptest(model)
shapiro.test(model$residuals)
hist(model$residuals)

# losowanie z rozkładu reszt

y_dop <- model$fitted.values
e <- model$residuals
n <- nrow(data)

d <- data[,5:8]


first <- T
for(i in 1:10000) {
  r <- sample(e, n, replace = T)
  d$y <- y_dop + r
  model <- lm(y ~ price_unit + is_holiday_week + is_winter + promo_rate, d)
  B <- model$coefficients
  if(first) {
    Beta <- as.data.frame(t(B))
    first <- F
  } else {
    Beta <- rbind(Beta, t(B))
  }
}


hist(Beta$'(Intercept)')

hist(Beta$price_unit)
sum(Beta$price_unit < 0)/10000
quantile(Beta$price_unit, c (0.025, 0.975))

hist(Beta$promo_rate)

# losowanie z obserwacji
# w kazdym obiegu petli losujemy z oryginalnych danych, zachowujemy to do zmiennnej, do ktorej nalezy
# losujemy odpowiednia liczbe numerow wierszy, wyznaczamy x <- tabelke wierszy, model, parametry, reszta analizy
# wyglada tak jak dalej

first <- T
for (i in 1:1000) {
  indeksy_losowane <- sample(1:n, n, replace = T)
  d_indeksowane <- data[indeksy_losowane,]
  model <- lm(units_sold ~ price_unit + is_holiday_week + is_winter + promo_rate, d_indeksowane)
  B <- model$coefficients
  
  if(first) {
    Beta <- as.data.frame(t(B))
    first <- F
  } else {
    Beta <- rbind(Beta, t(B))
  }
}

hist(Beta$'(Intercept)')

hist(Beta$price_unit)
sum(Beta$price_unit < 0)/1000
quantile(Beta$price_unit, c (0.025, 0.975))

hist(Beta$promo_rate)

# walidacja krzyżowa (cross-validation)

# k-fold: dzielimy na k podzbiorów
k<-10
n<-nrow(data)
grupy <-sample(rep(1:k, length.out = n))
data_k <- data.frame()
bledy_k <- numeric(k)

for (i in 1:k){
  d_train = data[grupy != i, ]
  d_test = data[grupy == i, ]
  
  model_k <- lm(units_sold ~ price_unit + is_holiday_week + is_winter + promo_rate, d_train)
  B <- model_k$coefficients
  Beta_k<- rbind(Beta_k, t(B))
  
  przewidywanie <- predict(model_k, newdata = d_test)
  bledy_k[i] <- mean((d_test$units_sold - przewidywanie)^2)
  
}
MAE = mean(abs(bledy_k))
RMSE = sqrt(mean(bledy_k^2))
MAPE = mean(abs())

# leave one out: estymujemy na n-1 obserwacji i dla jednej prognoza

# MAE = mean(abs(e))
# RMSE = sqrt(mean(e^2))
# MAPE = mean(abs(e/y))*100%


