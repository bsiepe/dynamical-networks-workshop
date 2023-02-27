# Typing Speed ------------------------------------------------------------
library(qgraph)

node_names <- c("Geschwin-\ndigkeit", "Fehler")

bw_mat <- matrix(c(0,.9,0,0), nrow = 2)
wi_mat <- matrix(c(0,0,-.9,0), nrow = 2)

svg("typingspeed.svg")
par(mfrow = c(1,2))
qgraph(bw_mat, theme = "colorblind", layout = "spring", labels = node_names,
       vsize = 15, esize = 8, directed = FALSE, title = "Zwischen")

qgraph(wi_mat, theme = "colorblind", layout = "spring", labels = node_names,
       vsize = 15, esize = 8, directed = FALSE, title = "Innerhalb")
dev.off()




# Example network ---------------------------------------------------------
library(mlVAR)
net_sim <- mlVAR::mlVARsim(nNode = 10) 

svg("example_temporal.svg")
qgraph(net_sim$model$Beta$mean[,,1], theme = "colorblind", threshold = 0.17,
       negDashed = TRUE)
dev.off()