# Dynamic Networks Workshop -----------------------------------------------
# This file contains exercises for the workshop on dynamic networks
# held at the Goethe University Frankfurt, Germany, on 2024-10-23.
# Solutions can be found in "dynamic_network_solutions.qmd" or in the PDF
# version of the same file. 


# Preparation -------------------------------------------------------------
# Load all packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("mlVAR", "qgraph")

# Load the data



# Exercise 1: Estimate multilevel network ---------------------------------
# Estimate a multilevel network model using the mlVAR package.
# Use the function mlVAR() to estimate a multilevel VAR model.


# Task: Find out what the "beep" and "day" variables do and set them correctly.


# Exercise 2: Plot and interpret overall network --------------------------


# Task: Plot the overall network and interpret the connection between
# XXX and YYY in the temporal and between XXX and YYY in the contemporaneous
# network.

# Exercise 3: Obtain and interpret individual networks --------------------

# Task: Obtain the individual networks for individuals 1, 2, and 3. 
# How different are they? How do they compare to the overall network?


# Optional: Estimate a GIMME model on the data ----------------------------


