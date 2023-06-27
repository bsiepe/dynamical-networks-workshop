

# Lösungen ----------------------------------------------------------------
### 1. Fehlende Daten von Personen 2, 7 und 21 imputieren.
# Verwendung von for-loop
# nochmal Variablenvektor von oben verwenden 
# relevante Variablen
rel_vars <- c("Dominance", "Affiliation", "PosAff",
              "NegAff", "Stress", "Functioning")

# Vorsorglich Datenliste duplizieren
data_list_imp <- data_list
for(p in c(2, 7, 21)){
  data_list_imp[[p]] <- imputeTS::na_kalman(data_list_imp[[p]])
}

# Check, ob es funktioniert hat
data_list_imp[[2]]


### 2.  RMSSD berechnen für alle Variablen von Person 2 (TIPP: `psych::rmssd`). 

# Im Tidy-Style
data_list[[2]] |> 
  dplyr::summarize(across(all_of(rel_vars),
                          ~ psych::rmssd(.)))

# Mit Base R
# RMSSD auf die relevanten Spalten anwenden
result <- apply(data_list[[2]][, rel_vars], 2, function(x) psych::rmssd(x))




### 3.  Für Person 2 ein Netzwerk in `graphicalVAR` schätzen und mit dem GIMME-Ergebnis vergleichen.
# graphicalVAR rechnen
gvar_res8 <- graphicalVAR::graphicalVAR(
  data = data_list[[8]],
  nLambda = 50)
plot(gvar_res)

# GIMME-Ergebnis inspizieren
plot(fit, file = 8)

# Ergebnismatrizen vergleichen
# ohne erste Spalte (Intercept)
gvar_res8$beta[,-1]
gimme_res18 <- read.csv("Anwendungs_Workshop/output/individual/subj8Betas.csv")

# Achtung! 
gimme_res18


### 4.  Gruppencutoff für GIMME ändern und Ergebnisse neu inspizieren.
# Achtung: Am besten neuen Ordner erstellen, um das alte nicht zu überschreiben
dir.create("Anwendungs_Workshop/output_new")

# Beispielsweise:
fit_new <- gimmeSEM(
  data = data_list_short,
  out = "Anwendungs_Workshop/output_new",
  ar = TRUE,           # Autoregressive Effekt schätzen (oftmals empfohlen)
  plot = TRUE,         # Plotten?
  subgroup = TRUE,     # Subgruppen schätzen?
  hybrid = FALSE,      # directed & undirected contemporaneous
  groupcutoff = .51,   # Gruppencutoff
  subcutoff = .75,     # Subgruppencutoff,
  paths = NULL         # vorgegebene Gruppenpfade
)

# Beides Plotten
plot(fit)
plot(fit_new)

# Weitere Ergebnisse inspizieren
# ... 

