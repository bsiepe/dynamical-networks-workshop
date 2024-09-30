
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# WORKSHOP: DYNAMISCHE NETZWERKE IN R
# Björn Siepe, 28.06.2023
# Alle Materialien: https://github.com/bsiepe/dynamical-networks-workshop/tree/main

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------






# Packages ---------------------------------------------------------------
# Der folgende Code installiert alle relevanten Packages
pkg_list <- c(
  "tidyverse", "gimme", "graphicalVAR", "here", "imputeTS", "perturbR",
   "DiagrammeR", "Hmisc", "naniar", "visdat", "skimr"
)
new.packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)}


# Relevante Packages laden
library(tidyverse)       # Für alles
library(graphicalVAR)    # graphicalVAR Netzwerke
library(gimme)           # gimme
library(here)            # reproduzierbare Datenstruktur
library(Hmisc)           # Deskription
library(skimr)           # Deskription
library(naniar)          # fehlende Daten 
library(visdat)          # fehlende Daten
library(gt)              # Tabellen







# Datenvorverarbeitung ----------------------------------------------------

### Fehlende Daten
# Wenn wir einen Datensatz namens "data" hätten: 
data$var1_imp <- imputeTS::na_kalman(data$var1)


### Detrending
# Funktion zum Detrending
fn_detrend <- function(x,                    # Datensatz
                       vars,                 # Vektor mit Variablennamen
                       time_var = "time",    # Zeitvariable
                       sig_only = FALSE){    # nur signifikante Trends detrenden?
  for (v in 1:length(vars)){
    # Regress on time
    lm_form <- as.formula(paste0(vars[v], "~", time_var))
    lm_res <- summary(lm(lm_form, data = x))
    # detrend with residuals
    # [,4] accesses p-values
    # [2] p-value of beta of tp
    if(sig_only){
      if(lm_res$coefficients[,4][2] < 0.05){
        x[!is.na(x[vars[v]]),vars[v]] <- residuals(lm_res)
      }
    }
    if(isFALSE(sig_only)){
      x[!is.na(x[vars[v]]),vars[v]] <- residuals(lm_res)
    }
    
  }
  return(x)
}

data$var1_detrend <- fn_detrend(data$var1)




# graphicalVAR ------------------------------------------------------------
### Nur beispielhafter Code hier


fit <- graphicalVAR(
  data = NULL,        # Datensatz
  nLambda = 50,       # Anzahl von LASSO Parametern, die getestet wird
  gamma = 0.5,        # EBIC Hyperparameter
  scale = TRUE,       # z-standardisieren (wichtig für LASSO!)
  vars = NULL,        # Vektor mit Variablennamen
  beepvar = "beep",   # Beepvariable
  dayvar = "day",     # Tagesvariable
  idvar = "id"        # ID der Person

)




# -------------------------------------------------------------------------
# gimme -------------------------------------------------------------------
# -------------------------------------------------------------------------



# Datenvorbereitung -------------------------------------------------------
### nicht auszuführen -----------------------------
# Nur für Interssierte: So wurden die individuellen Datensätze aus dem
# OSF-Link zusammengeführt. Der Ordner "individual_files" enthielt die
# 94 einzelnen Files
file_list <- list.files(here::here("Anwendungs_Workshop/data/individual_files"), 
                        full.names = TRUE)
data_list <- lapply(file_list, read.csv)

# Zeitvariable hinzufügen
data_list <- lapply(data_list, function(x){
  x <- x |> 
    dplyr::mutate(time = dplyr::row_number())
})


saveRDS(data_list, here::here("Anwendungs_Workshop/data/data_list.RDS"))
### ----------------------------------------------


# Einladen der Datenliste
data_list <- readRDS(here::here("Anwendungs_Workshop/data/data_list.RDS"))

# Erstellen des relevanten Ordners für gimme
dir.create(here::here("Anwendungs_Workshop/output"))



# Deskriptivstatistiken & Plots -------------------------------------------
## Über alle Files loopen - Beispiel
# Liste für Output erstellen
mean_dom <- list()

## Iteriere durch die Liste, gebe Mittelwert aus
# Für Person p in 1 bis Länge der Liste
for(p in 1:length(data_list)){
  # Berechne Mittelwert von Dominance, speichere im Element "p"
   mean_dom[[p]] <- mean(data_list[[p]]$Dominance, na.rm = TRUE)
}

### Histogramme
# Für eine Person
Hmisc::hist.data.frame(data_list[[2]])

# Für alle Personen
# starte PDF
pdf("all_histograms.pdf")
# Loope über alle Personen
for(p in 1:length(data_list)){
  Hmisc::hist.data.frame(data_list[[p]])
}
# stoppe PDF
dev.off()

### Deskiriptive Statistiken
# Überblick über Datensatz
skim_df <- skimr::skim(data_list[[2]])
skim_df |> 
  gt()

# relevante Variablen
rel_vars <- c("Dominance", "Affiliation", "PosAff",
              "NegAff", "Stress", "Functioning")

# Berechnen von Mean und SD
desc_list <- list()

# Über alle p Personen loopen
for(p in 1:length(data_list)){
  desc_list[[p]] <- data_list[[p]] |> 
    # fasse zusammen über alle relevanten Variablen
    dplyr::summarize(across(all_of(rel_vars),
                            # verwende mean und sd
                            list(mean = mean, sd = sd), na.rm = TRUE))
  
}
# schreibe Liste als Dataframe
df_desc <- bind_rows(desc_list, .id = "id")

# fasse Deskriptivstatistiken über alle Personen zusammen
df_desc |> 
  dplyr::summarize(across(everything(),
                          ~ round(mean(.), 3))) |> 
  select(-id) |> 
  gt()

# Pro Person
df_desc |> 
  dplyr::group_by(id) |> 
  dplyr::summarize(across(everything(),
                          ~ round(mean(.), 3))) |>
  head(n = 6L) |> 
  gt()



### Fehlende Daten
# Für eine Person visualisieren
visdat::vis_dat(data_list[[6]])


### Detrending
# relevante Variablen
rel_vars <- c("Dominance", "Affiliation", "PosAff",
              "NegAff", "Stress", "Functioning")

# Loopen über alle p Participants
for(p in 1:length(data_list)){
  data_list[[p]] <- fn_detrend(data_list[[p]],
                               vars = rel_vars, 
                               time_var = "time", 
                               sig_only = FALSE)
}

# Zeitvariable wieder löschn
for(p in 1:length(data_list)){
  data_list[[p]] <- subset(data_list[[p]], select = -c(time))
}



# Alternative Codingstrategien für Listen ---------------------------------
## lapply statt for loop
mean_list <- lapply(data_list, function(x){
  # x: einzelnes Element von data_list
  mean(x$Dominance, na.rm = TRUE)

})

## oder als Datensatz verwenden
df_data <- data_list |>
  # mit ID abspeichern
  tibble::enframe(name = "ID") |>
  # in dataframe verwandeln
  tidyr::unnest()

# zurück in Liste verwandeln
data_list_new <- split(df_data, df_data$ID)





# Modellfitting -----------------------------------------------------------
example_data <- gimme::simData
data_list_short <- data_list[1:30]

fit <- gimmeSEM(
  data = data_list_short,
  out = "Anwendungs_Workshop/output",
  ar = TRUE,           # Autoregressive Effekt schätzen (oftmals empfohlen)
  plot = TRUE,         # Plotten?
  subgroup = TRUE,     # Subgruppen schätzen?
  hybrid = FALSE,      # directed & undirected contemporaneous
  groupcutoff = .75,   # Gruppencutoff
  subcutoff = .51,     # Subgruppencutoff,
  paths = NULL         # vorgegebene Gruppenpfade
)

# Ergebnisse speichern
# saveRDS(fit, "Anwendungs_Workshop/output/fit.RDS")

# Ergebnisse wieder laden
fit <- readRDS(here::here("Anwendungs_Workshop/output/fit.RDS"))


### Gruppenergebnisse visualisieren
plot(fit)


# Zusammenfassungsmatrix einlesen
summary_matrix <- read.csv(here("Anwendungs_Workshop/output/summaryPathCountsMatrix.csv"))
summary_matrix 



### Subgruppenergebnisse
library(perturbR)
perturb_gimme <- perturbR(
  sym.matrix = fit$sim_matrix,
  plot = TRUE,
  reps = 100,
  errbars = TRUE
)

perturb_gimme <- readRDS("Anwendungs_Workshop/output/perturb_gimme.RDS")



# Alle Einzelergebnisse laden
ind_ests <- read.csv(here("Anwendungs_Workshop/output/indivPathEstimates.csv"))

# kalkulieren Mittelwerte und Standardabweichungen pro Gruppe
ind_ests |> 
  # gruppieren nach Subgruppe und Variablen (lefthandside, righthandside)
  dplyr::group_by(sub_membership, lhs, rhs) |> 
  # kalkulieren mean und sd der beta-Gewichte
  dplyr::summarize(b_mean = mean(beta, na.rm = TRUE),
                   b_sd = sd(beta, na.rm = TRUE)) |> 
  # Gruppierung auflösen
  dplyr::ungroup() |>
  # Datenstruktur ändern, damit Gruppeneffekte direkt zusehen sind
  tidyr::pivot_wider(names_from = sub_membership,
                     values_from = c(b_mean, b_sd),
                     names_prefix = "g") |> 
  # nur Teile der Ergebnisse anzeigen lassen
  dplyr::select(1:5) |> 
  head(n = 6L) |> 
  gt()



### Individualergebnisse
# Person 2 
plot(fit, file = 2)

# einzelne Ergebnisse anzeigen
ind_ests |> 
  head(n = 6L) |> 
  gt()




# Multiple Solutions GIMME ------------------------------------------------

# 
# fit <- gimme(...,          # rest identisch zu oben
#       ms_allow = TRUE,     # multiple Lösungen zulassen
#       ar = FALSE,          # AR-Effekte nicht sofort schätzen
#       ms_tol = 1e-5)       # Default Toleranz für multiple Lösungen

# schauen es uns einfach mit Beispieldaten aus GIMME an
ms_fit <- gimme::ms.fit
solution.tree(ms_fit, level =  "group", plot.tree = TRUE)



# Aufgaben ----------------------------------------------------------------
### 1.  Fehlende Daten von Personen 2, 7 und 21 imputieren. 
### 2.  RMSSD berechnen für alle Variablen von Person 8 (TIPP: `psych::rmssd`). 
### 3.  Für Person 8 ein Netzwerk in `graphicalVAR` schätzen und mit dem GIMME-Ergebnis vergleichen.
### 4.  Gruppencutoff für GIMME ändern und Ergebnisse neu inspizieren.


