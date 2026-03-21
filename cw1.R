# rozk statystyczne

# r - losowanie
# p -dystrybuanta
# d - prawdopodobieństwo lub gęstość prawd.
# q - kwantyl

hist(rnorm(100, 0 , 1))
hist(runif(100, 0 , 1))
hist(rt(100, 7))
hist(rlnorm(100, 0 , 1))
hist(rpois(100,3))
hist(rbinom(100, 10, 0.3))

set.seed(1234)
rnorm(1)

pnorm(2)
dnorm(2)
dbinom(4, 10, 0.3)

qnorm(0.9)
qnorm(pnorm(2))

# generator liczb pseudolosowych

A <- 987654321
C <- 123456789
u0 <- as.numeric(Sys.time())

u <- vector()
for(i in 1:100) {
  u0 <- (C*u0 + 436)%%A
  u[i] <- u0/A
}

hist(u)

# testowanie rozkładu

ks.test(u, "punif")
ks.test(u, "pnorm", mean(u), sd(u))

# generowanie wartości z innych rozkładów
hist(qnorm(u))
ks.test(qnorm(u), "pnorm")
hist(qlnorm(u))

# z rozkładu empirycznego
x <- read.csv("dane1.csv")$x
hist(x)

quantile(x, c(0, 0.25, 0.75, 1))
hist(quantile(x, u))

emp <- ecdf(x)
emp(20)


# testy permutacyjne

set.seed(123)
x <- rnorm(15)
y <- rnorm(12)
var.test(x, y)


n = 0
for (i in 1:1000){
  x <- rnorm(15)
  y <- rnorm(12)
  test <- var.test(x, y)
  if(test$p.value < 0.05) n = n+1
}
n

# a) rozklad log normalny
# b) rozkład jednostajny

# a)
n = 0
for (i in 1:1000){
  x <- rlnorm(15)
  y <- rlnorm(12)
  test <- var.test(x, y)
  if(test$p.value < 0.05) n = n+1
}
n

# b)
n = 0
for (i in 1:1000){
  x <- runif(15)
  y <- runif(12)
  test <- var.test(x, y)
  if(test$p.value < 0.05) n = n+1
}
n

# permutacyjny test wariancji

set.seed(123)
x <- rlnorm(15)
y <- rlnorm(12)
S <- var(x)/var(y)
S
z <- c(x, y)

smp <- t(sapply(1:10000, sample, x = z, size =  27))

stat <- vector()
for(i in 1:10000){
  a <- smp[i, 1:15]
  b <- smp [i, 16:27]
  stat[i] <- var(a)/var(b)
  if(stat[i] < 1) stat[i] = 1/stat[i]
}

hist(stat)
sum(stat > S)/10000

# test korelacji

data <- read.csv("dane2.csv")

shapiro.test(data$x)
shapiro.test(data$y)

cor.test(data$x, data$y)


install.packages("gtools")
library(gtools)

per <- permutations(n = 9, r = 9, v = data$y)


r0 <- cor(data$x, data$y)
r <- vector()
for (i in 1:nrow(per)){
  r[i] <- cor(data$x, per[i, ])
  
}
hist(r)
sum(abs(r) > abs(r0))/nrow(per)