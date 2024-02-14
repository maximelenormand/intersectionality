# Working directory
# setwd("")

# Load Data
tab <- read.csv2(paste("Data/Outputs/Signals.csv", sep = ""), stringsAsFactors = FALSE) # Signals produced at the previous step
n <- dim(tab)[1]

# Normalization 1 (by column for each city and social group)
cit <- names(table(tab[, 2]))
socat <- names(table(tab[, 3]))
for (i in 1:length(cit)) {
  print(i)
  for (j in 1:length(socat)) {
    marg <- apply(tab[tab[, 2] == cit[i] & tab[, 3] == socat[j], -c(1, 2, 3, 4)], 2, sum) # Total by column for a given city and social group
    tab[tab[, 2] == cit[i] & tab[, 3] == socat[j], -c(1, 2, 3, 4)] <- t(t(tab[tab[, 2] == cit[i] & tab[, 3] == socat[j], -c(1, 2, 3, 4)]) / marg) # Normalization
  }
}

# Normalization 2 (by row to sum to 1)
tab[, -c(1, 2, 3, 4)] <- tab[, -c(1, 2, 3, 4)] / apply(tab[, -c(1, 2, 3, 4)], 1, sum)

# Clustering Ward
d <- as.matrix(dist(tab[, -c(1, 2, 3, 4)])) # Dissimilarity matrix based on the Euclidean distance
h <- hclust(as.dist(d), method = "ward.D2") # Clustering Ward D2

# Variance intra / tot
n <- dim(d)[1]
nclustmax <- 40
vars <- rep(0, nclustmax)
vartot <- sum(d^2) / (2 * (n^2)) # Variance total
for (i in 1:nclustmax) {
  clu <- cutree(h, i)
  varintra <- rep(0, i)
  for (k in 1:i) {
    lg <- which(clu == k)
    varintra[k] <- (sum(d[lg, lg]^2) / (2 * sum(clu == k)^2)) * (sum(clu == k) / n) # Variance intra per cluster
  }
  vars[i] <- sum(varintra) / vartot # Ratio
}

pdf("FigS1.pdf", width = 8.322917, height = 5.811220, useDingbats = FALSE)
  par(mar = c(5, 7, 1, 1))
  par(mfrow = c(1, 1))
  plot(1:nclustmax, vars, typ = "b", cex = 2, lwd = 3, pch = 16, col = "steelblue3", axes = FALSE, xlab = "", ylab = "", xlim = c(1, nclustmax), ylim = c(0, 1))
  box(lwd = 1.5)
  axis(1, las = 1, cex.axis = 1.7, lwd = 1.5, padj = 0.2)
  axis(2, las = 1, cex.axis = 1.75, lwd = 1.5, at = seq(0, 1, 0.2))
  mtext("Number of clusters", 1, line = 3.5, cex = 2)
  mtext("Within / total variance", 2, line = 5, cex = 2)
  abline(v = 5, col = "#CC6666", lwd = 3)
dev.off()

# Check clusters
com <- cutree(h, 5)

table(com)
table(com) / sum(table(com))

# Export table
tabp <- data.frame(tab, Profile = com)
write.csv2(tabp, paste("Data/Outputs/Signals_L2.csv", sep = ""), row.names = FALSE)

