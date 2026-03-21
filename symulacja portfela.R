# symulacja strategii inwestycyjnej
n <- nrow(RUT)
dni_trzymanie <- 100
liczba_akcji <- 100

zysk_portfel <- numeric(n - dni_trzymanie)

for (i in 1:(n - dni_trzymanie)){
  zysk_RUT <- (RUT$close[i + dni_trzymanie] - RUT$close[i]) * liczba_akcji
  
  zysk_IXIC <- (IXIC$close[i + dni_trzymanie] - IXIC$close[i]) * liczba_akcji
  
  zysk_portfel[i] <- zysk_RUT + zysk_IXIC
}

hist(zysk_portfel / 1000, breaks = 30, col = 'lightblue', 
     main = "Histogram zysku z portfela",
     xlab = "Zysk [w tyś. USD]",
     )

