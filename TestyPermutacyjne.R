n <- 5000

# --- 1. TEST ISTOTNOŚCI KOWARIANCJI ---
# Cel: Sprawdzenie, czy wartości w dwóch zbiorach danych zmieniają się w sposób powiązany.

cov_ret <- cov(RUT$return, IXIC$return)
cov_perm <- replicate(n, cov(RUT$return, sample(IXIC$return)))
p_val_cov <- sum(abs(cov_perm) >= abs(cov_ret)) / n

cat("--- COVARIANCE SIGNIFICANCE TEST ---")
cat("\nObserved Covariance:", cov_ret)
cat("\nP-value:", p_val_cov)
# Wniosek: Statystycznie istotna kowariancja (p < 0,05) pozwala odrzucić hipotezę o niezależności; 
# zmienne wykazują wyraźne powiązanie liniowe, którego nie da się wyjaśnić przypadkiem.


# --- SEKCJA PRZYGOTOWAWCZA DO TESTÓW DWUPRÓBKOWYCH ---
# Łączymy stopy zwrotu obu indeksów w jeden wektor (pulę)
pool <- c(RUT$return, IXIC$return)
n_rut <- length(RUT$return)
n_total <- length(pool)


# --- 2. TEST RÓWNOŚCI WARIANCJI ---
# Cel: Porównanie rozrzutu (zmienności) danych w obu grupach.

v_rut <- var(RUT$return)
v_ixic <- var(IXIC$return)
obs_var_ratio <- max(v_rut, v_ixic) / min(v_rut, v_ixic)

var_ratios_perm <- replicate(n, {
  shuffled <- sample(pool)
  v1 <- var(shuffled[1:n_rut])
  v2 <- var(shuffled[(n_rut + 1):n_total])
  max(v1, v2) / min(v1, v2)
})

p_val_var <- sum(var_ratios_perm >= obs_var_ratio) / n

cat("\n\n--- VARIANCE EQUALITY TEST ---")
cat("\nObserved Ratio:", obs_var_ratio)
cat("\nP-value:", p_val_var)
# Wniosek: Wynik istotny statystycznie (p < 0,05) wskazuje na brak równości wariancji; 
# stopień rozproszenia danych wokół średniej w obu grupach jest znacząco odmienny.


# --- 3. TEST RÓWNOŚCI ŚREDNICH ---
# Cel: Sprawdzenie, czy przeciętne wartości w obu zbiorach są identyczne.

obs_mean_diff <- abs(mean(RUT$return) - mean(IXIC$return))

mean_diffs_perm <- replicate(n, {
  shuffled <- sample(pool)
  m1 <- mean(shuffled[1:n_rut])
  m2 <- mean(shuffled[(n_rut + 1):n_total])
  abs(m1 - m2)
})

p_val_mean <- sum(mean_diffs_perm >= obs_mean_diff) / n

cat("\n\n--- MEAN EQUALITY TEST ---")
cat("\nObserved Difference:", obs_mean_diff)
cat("\nP-value:", p_val_mean, "\n")
# Wniosek: Brak podstaw do odrzucenia hipotezy o równości średnich (p > 0,05); 
# różnica między wartościami przeciętnymi obu zbiorów jest nieistotna statystycznie.