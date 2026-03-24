# --- symulacja strategii inwestycyjnej ---
n <- nrow(RUT)
dni_trzymania <- 100
liczba_akcji <- 100

zysk_portfel <- numeric(n - dni_trzymania)

for (i in 1:(n - dni_trzymania)){
  zysk_RUT <- (RUT$close[i + dni_trzymania] - RUT$close[i]) * liczba_akcji
  
  zysk_IXIC <- (IXIC$close[i + dni_trzymania] - IXIC$close[i]) * liczba_akcji
  
  zysk_portfel[i] <- zysk_RUT + zysk_IXIC
}

# --- histogram zysku z portfela --- 
hist(zysk_portfel / 1000, breaks = 30, col = 'lightblue', 
     main = "Histogram zysku z portfela",
     xlab = "Zysk [w tyś. USD]",
     )

# --- test normalności ---
shapiro.test(zysk_portfel)

cat("p-value < 2.2e-16")

cat("WNIOSEK: ROZKŁAD ZYSKU Z PORTFELA NIE JEST ROZKŁADEM NORMALNYM")

# --- VaR ---
# historyczny
poziomy_istotnosci <- c(0.05, 0.01, 0.001)
var_historyczny <- quantile(zysk_portfel, poziomy_istotnosci)

# normalny (parametryczny)
srednia_portfel <- mean(zysk_portfel)
sd_portfel<-sd(zysk_portfel)
var_norm <- qnorm(poziomy_istotnosci, srednia_portfel, sd_portfel)

# porownanie VaR
var_porownanie <- data.frame(
  Historyczny = var_historyczny,
  Normalny = var_norm
)

print(var_porownanie)

cat("Przy poziomie istotności 5%, VaR historyczny jest większy niż normalny,
    jednakże przy 1% i 0.1% VaR historyczny już jest znacznie niższy od normalnego, 
    co pokazuje, że traciliśmy więcej niż zakładał rozkład normalny. ")