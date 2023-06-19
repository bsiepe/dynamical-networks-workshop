
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# WORKSHOP: DYNAMISCHE NETZWERKE IN R
# Björn Siepe, 28.06.2023
# Alle Materialien: https://github.com/bsiepe/dynamical-networks-workshop/tree/main

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------






# Packages installieren ---------------------------------------------------
# Der folgende Code installiert alle relevanten Packages
pkg_list <- c(
  "tidyverse", "gimme", "graphicalVAR", "here", "imputeTS", "perturbR"
)
new.packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)}



# Daten laden -------------------------------------------------------------




# Datenvorverarbeitung ----------------------------------------------------



### Detrending
# Diese Funktion detrendet eine Variable
fn_detrend <- function(x){
  for (v in 1:length(rel_vars)){
    # Regress on time
    lm_form <- as.formula(paste0(rel_vars[v], "~ time"))
    lm_res <- summary(lm(lm_form, data = x))
    # detrend with residuals
    # [,4] accesses p-values
    # [2] p-value of beta of tp
    # new addition: detrend
    # if(lm_res$coefficients[,4][2] < 0.05){
    # print(paste0("Detrending variable: ", rel_vars[v]))
    x[!is.na(x[rel_vars[v]]),rel_vars[v]] <- residuals(lm_res)
    # }
    
  }
  return(x)
}






# graphicalVAR ------------------------------------------------------------




# GIMME -------------------------------------------------------------------


