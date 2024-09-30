
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

for(i in 1:20){
  l_fits <- list()
  
  model <- graphicalVAR::randomGVARmodel(6)
  data <- graphicalVAR::graphicalVARsim(nTime = 60,
                                        beta = model$beta,
                                        kappa = model$kappa)
  l_fits[[i]] <- graphicalVAR(data,
                              verbose = FALSE)

}


# Create gif of networks ------------------------------------------------
animation::saveGIF({
  for(i in 1:20){
    plot(l_fits[[i]],
         include = "PDC",
         layout = "circle",
         title = paste0("Individual ", i),
         title.cex = 1.5,
         negDashed = TRUE,
         palette = colorblind)
  }
}, movie.name = ("false_heterogeneity_.gif"), 
ani.height = 800, ani.width = 800, interval = 1)