# Testy permutacyjne

n <- 10000

# Istotność kowariancji (Zależność między rynkami)

# Test sprawdza, czy zaobserwowana kowariancja między indeksami HSI a N225 jest wynikiem prawdziwej zależności.

# Logika: Obliczamy faktyczną kowariancję z danych. Następnie tysiące razy losujemy nową kolejność dla jednego z wektorów zwrotów.

# Hipoteza zerowa: Stopy zwrotu obu indeksów są od siebie niezależne.

cov_ret <- cov(HSI$return, N225$return)
cov_perm <- replicate(n, cov(HSI$return, sample(N225$return)))

sum(abs(cov_perm) >= abs(cov_ret))

# Obliczenie p-value
p_val_cov <- sum(abs(cov_perm) >= abs(cov_ret)) / n

# Wynik
cat("Obserwowana kowariancja:", cov_ret, "\n")
cat("P-value dla kowariancji:", p_val_cov, "\n")

# Wniosek: Odrzucamy hipotezę zerową



# Równość wariancji i średnich

# Łączenie prób: Łączymy stopy zwrotu obu indeksów w jeden zbiór danych.

# Losowanie: Wielokrotnie dzielimy ten zbiór na dwie nowe, losowe grupy o tych samych liczebnościach co oryginały.

# Porównanie: Obliczamy różnice średnich lub stosunek wariancji dla tych losowych grup. Pozwala to ocenić, czy różnica między HSI a N225 wynika z charakterystyki rynków, czy jest tylko błędem losowym.