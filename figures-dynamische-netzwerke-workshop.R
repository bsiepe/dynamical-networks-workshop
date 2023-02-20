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
