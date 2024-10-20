
# Idea --------------------------------------------------------------------
# Simulate from a graphicalVAR model with limited time points
# fit a graphicalVAR model to the simulated data
# create a GIF showing the differences between individuals

# Load libraries ---------------------------------------------------------
library(graphicalVAR)
library(tidyverse)
library(animation)
# Simulate data ----------------------------------------------------------
set.seed(35032)


# Taken from Hoekstra et al. (2022)
synth <- list()
synth$beta <- as.matrix(read.table(header = FALSE, colClasses = "numeric", text = "
0.29 0.32 0 0 0 0 0 0
0 0.13 -0.36 0 0 0 0 0
0 0 0.37 -0.47 0 0 0 0
0 0 0 0.32 0.27 0 0 0
0 0 0 0 0.44 -0.24 0 0
0 0 0 0 0 0.44 -0.33 0
0 0 0 0 0 0 0.50 0.24
0.34 0 0 0 0 0 0 0.42"))

synth$beta <- synth$beta[1:6, 1:6]

synth$kappa <- as.matrix(read.table(header = FALSE, colClasses = "numeric", text = "
0 -0.27 0 0 0 0 0 -0.29
-0.27 0 0.49 0 0 0 0 0
0 0.49 0 0.22 0 0 0 0
0 0 0.22 0 -0.36 0 0 0
0 0 0 -0.36 0 0.52 0 0
0 0 0 0 0.52 0 0.29 0
0 0 0 0 0 0.29 0 -0.30
-0.29 0 0 0 0 0 -0.30 0"))

synth$kappa <- synth$kappa[1:6, 1:6]

l_fits <- list()
l_data <- list()

# Simulate data from the same individual 20 times
for(i in 1:20){
  
  l_data[[i]] <- graphicalVAR::graphicalVARsim(nTime = 70,
                                        beta = synth$beta,
                                        kappa = synth$kappa)
  l_fits[[i]] <- graphicalVAR(l_data[[i]],
                              verbose = FALSE)

}


# Create gif of networks ------------------------------------------------
animation::saveGIF({
  for(i in 1:20){
    plot(l_fits[[i]],
         include = "PDC",
         layout = "circle",
         titles = FALSE,
         title = paste0("Individual ", i),
         title.cex = 2.2,
         negDashed = TRUE,
         theme = "colorblind")
  }
}, movie.name = ("false_heterogeneity.gif"), 
ani.height = 550, ani.width = 550, interval = 1, width = 550, height = 550, res = 300)

