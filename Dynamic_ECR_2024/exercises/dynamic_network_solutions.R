# Dynamic Networks Workshop -----------------------------------------------
# This file contains solutions for the workshop on dynamic networks
# held at the Goethe University Frankfurt, Germany, on 2024-10-23.
# Solutions can also be found in the PDF
# version of the same file. 


# Preparation -------------------------------------------------------------
# Load all packages
if (!require("mlVAR")) install.packages("mlVAR")
if (!require("qgraph")) install.packages("qgraph")
library(mlVAR)
library(qgraph)
set.seed(35037)

# Simulation Code ---------------------------------------------------------
# This code shows how data were generated. You can skip this part.
n_id <- 75
n_tp <- 80
n_beep <- 4
n_days <-  n_tp/n_beep

# Simulate the data
sim_obj <- mlVAR::mlVARsim(
  nPerson = n_id,
  nNode = 6,
  nTime = n_tp,
  lag = 1
)


# Load the data
df_data <- sim_obj$Data

# add day variable and beep variable
# per person, and 4 beeps per day
df_data$beep <- rep(rep(rep(1:n_beep, each = 1), n_days), n_id)
df_data$day <- rep(rep(1:n_days, each = n_beep), n_id)

# rename ID to id
df_data$id <- df_data$ID

saveRDS(df_data, file = "Dynamic_ECR_2024/exercises/df_data.RDS")

# Exercise 1: Estimate multilevel network ---------------------------------
# Estimate a multilevel network model using the mlVAR package.

# Task:
# Use the function mlVAR() to estimate a multilevel VAR model.
# Find out what the "beep" and "day" variables do and set them correctly. 
mlvar_fit <- mlVAR::mlVAR(
  data = df_data,
  vars = c("V1", "V2", "V3", "V4", "V5", "V6"),
  idvar = "id",
  lags = 1,
  dayvar = "day",
  beepvar = "beep",
  estimator = "lmer"
)




# Exercise 2: Plot and interpret overall network --------------------------
# Task: Plot the overall network and interpret the connection between
# V5 and V6 in the temporal and between V2 and V3 in the contemporaneous
# network.

plot(mlvar_fit,
     type = "temporal",
     theme = "colorblind",
     layout = "circle",
     nonsig = "hide",
     edge.labels = TRUE)

plot(mlvar_fit,
     type = "contemporaneous",
     theme = "colorblind",
     layout = "circle",
     nonsig = "hide",
     edge.labels = TRUE)


# Exercise 3: Obtain and interpret individual networks --------------------
# Task: Obtain the individual temporal networks for individuals 1 and 2. 
# How different are they? 
# Tip 1: Check out the mlVAR::getNet() function. 
# Tip 2: Use the qgraph() function to plot the networks.

# Get the individual networks
net1 <- mlVAR::getNet(mlvar_fit, 
                      subject = 1,
                      type = "temporal")

net2 <- mlVAR::getNet(mlvar_fit,
                      subject = 2,
                      type = "temporal")


# Plot net1 and net2 in one plot
par(mfrow = c(1, 2))
qgraph(net1,
     theme = "colorblind",
     layout = "circle",
     edge.labels = TRUE,
     title = "Individual 1")
qgraph(net2,
     theme = "colorblind",
     layout = "circle",
     edge.labels = TRUE, 
     title = "Individual 2")

# Reset the plot layout
par(mfrow = c(1, 1))




# Optional: Estimate a GIMME model on the data ----------------------------
# Task: Estimate a GIMME model on the data using the gimme package.
# Tip 1: First get the data into a list for gimme, which requires each 
# individual to be in a separate list element
if(!require("gimme")) install.packages("gimme")
library(gimme)

# Get the data into a list
data_list <- split(df_data, df_data$id)

# Only select relevant columns
data_list <- lapply(data_list, function(x) x[, c("V1", "V2", "V3", "V4", "V5", "V6")])

# Estimate the GIMME model with default settings
gimme_fit <- gimme::gimmeSEM(data_list)

# Plot the resulting model fit
plot(gimme_fit)
