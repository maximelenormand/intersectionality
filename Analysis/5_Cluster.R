# Load packages
library(RColorBrewer)
library(sf)

# Working directory
# setwd("")

# Load Data
load("Data/Outputs/Data.Rdata")

shp <- shp[shp$cluster > 0, ] # Filter district without cluster (corresponding to the 11 sociodistricts without information)

mism <- shp[, 14 + c(55, 12, 13, 16, 36, 37, 38), drop = TRUE] # Mismatch with reference category (male for gender, 35-64 years for age and high educ. for education)
clu <- shp$cluster # Cluster
id <- shp$city # City

# Cities
cit <- read.csv2("Data/RAW/Additional Data/Table_Cities.csv") # Info cities
cit <- cit[order(cit[, 1]), ] # Rank city by id
cit <- data.frame(ID = as.character(cit[, 1]), Name = as.character(c("Albi", "Alençon", "Amiens", "Angers", "Angoulême", "Annecy", "Annemasse", "Bayonne", "Besançon", "Béziers", "Bordeaux", "Brest", "Caen", "Carcassonne", "Cherbourg", "Clermont-Ferrand", "Creil", "Dijon", "Douai", "Dunkerque", "Fort-de-France", "Grenoble", "La Rochelle", "Le Havre", "Lille", "Longwy", "Lyon", "Marseille", "Metz", "Montpellier", "Nancy", "Nantes", "Nice", "Nîmes", "Niort", "Paris", "Poitiers", "Quimper", "Rennes", "Rouen", "Saint-Brieuc", "St-Denis (La Réunion)", "Saint-Etienne", "Strasbourg", "Thionville", "Toulouse", "Tours", "Valence", "Valenciennes")), Pop = as.numeric(cit[, 2]), Fig = as.numeric(cit[, 3])) # Add a column with names to display
cit <- cit[order(cit[, 3], decreasing = TRUE), ] # Rank by population in inner cities
indfig <- as.numeric(rownames(cit)[!is.na(cit[, 4])]) # Identify the 23 cities to display in the main text (the six more populated and also those with the more recent data)
indfigsi <- as.numeric(rownames(cit)) # Id of the fig rank by size for the plots in SI
cit <- cit[order(cit[, 1]), ] # Rank again by id

# V-test
nb <- aggregate(mism, list(clu), length) # Number of districts by cluster replicated for the 7 types of mismatch
mu <- aggregate(mism, list(clu), mean) # Average mismatch by cluster and by type of mismatch
sd <- aggregate(mism, list(clu), sd) # Mismatch standard deviation by cluster and by type of mismatch
totnb <- dim(mism)[1] # Total number of districts
totmu <- apply(mism, 2, mean) # Average mismatch by type of mismatch
totsd <- apply(mism, 2, sd) # Mismatch standard deviation y type of mismatch

num <- (mu[, -1] - t(replicate(max(clu), totmu))) # V-test numerator (Average by cluster - average total)
dem <- sqrt(((totnb - nb[, -1]) / (nb[, -1] * (totnb - 1))) * t(replicate(max(clu), totsd^2))) # V-test denominator (standard error of the mean in the case of a sampling without replacement)
rho <- num / dem # V-test

# Export rho for apply shiny
write.csv2(rho,"Vizu/rho.csv",row.names=FALSE) 

# Table S2 (Average mismatch and standard deviation by type of mismatch in total and according to the cluster)
tabmu <- round(rbind(totmu, mu[, -1]), digits = 3)
tabsd <- round(rbind(totsd, sd[, -1]), digits = 3)
idcol <- c("All", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
tab <- paste(idcol, " & ", tabmu[, 1], " (", tabsd[, 1], ")", " & ", tabmu[, 2], " (", tabsd[, 2], ")", " & ", tabmu[, 3], " (", tabsd[, 3], ")", " & ", tabmu[, 4], " (", tabsd[, 4], ")", " & ", tabmu[, 5], " (", tabsd[, 5], ")", " & ", tabmu[, 6], " (", tabsd[, 6], ")", " & ", tabmu[, 7], " (", tabsd[, 7], ")", "\\", sep = "")
print(data.frame(tab), row.names = FALSE)

# Figure 5
pdf("Fig5.pdf", width = 17.3125, height = 10.1192, useDingbats = FALSE)

  colo <- brewer.pal(8, "Set2")

  layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6), 3, 6, byrow = TRUE), , width = c(1.6, 1, 1, 1, 1, 1), height = c(0.2, 1.2, 1))

  # Legend
  propclu <- round(100 * table(clu) / sum(table(clu))) # Percentage of districts per cluster
  propclu[2] <- propclu[2] + 1 # To sum to 100
  propcluleg <- c(paste0("  [", propclu[1], "%]       "), paste0("  [", propclu[2], "%]       "), paste0("  [", propclu[3], "%]       "), paste0("  [", propclu[4], "%]       "), paste0("   [", propclu[5], "%]       "), paste0("   [", propclu[6], "%]       "), paste0("  [", propclu[7], "%]       "), paste0("  [", propclu[8], "%]       "))

  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("bottomleft", inset = c(0.12, 0.2), col = "white", pch = 16, pt.cex = 3, lwd = 1, lty = 2, legend = propcluleg, bty = "n", cex = 2.1, xpd = TRUE, horiz = TRUE)
  legend("bottomleft", inset = c(0.12, 0.5), col = colo, pch = 16, pt.cex = 3, lwd = 1, lty = 2, legend = c("Cluster 1    ", "Cluster 2    ", "Cluster 3    ", "Cluster 4    ", "Cluster 5    ", "Cluster 6    ", "Cluster 7    ", "Cluster 8"), bty = "n", cex = 2.1, xpd = TRUE, horiz = TRUE)
  legend("topleft", inset = c(-0.025, -0.), legend = "A", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  # A
  atick <- c("-30", "", "-20", "", "-10", "", "0", "", "10", "", "20", "", "30")
  par(mar = c(7, 14, 0, 0.5))
  matplot(t(rho[c(1, 2, 3), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = colo[1:3], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
  axis(1, at = seq(-30, 30, 5), labels = atick, cex.axis = 1.8, padj = 0.3)
  axis(2, at = 1:7, labels = rev(c("Women", "65 and more", "25-34 years", "16-24 years", "Middle-high educ.", "Middle-low educ.", "Low educ.")), las = 1, cex.axis = 1.8, font = 2, padj = 0.3)

  segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)

  par(mar = c(7, 0.5, 0, 0.5))
  matplot(t(rho[c(4, 5, 6), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = colo[4:6], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
  axis(1, at = seq(-30, 30, 5), labels = atick, cex.axis = 1.8, padj = 0.3)
  mtext("V-Test", 1, line = 5, cex = 1.75)

  segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)

  par(mar = c(7, 0.5, 0, 1))
  matplot(t(rho[c(7, 8), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = colo[7:8], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
  axis(1, at = seq(-30, 30, 5), labels = atick, cex.axis = 1.8, padj = 0.3)

  segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
  segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)

  # B
  tab <- shp[, c(2, 3, 70), drop = TRUE]
  prop <- table(tab[, 2], tab[, 3])
  prop <- prop[3:1, ]
  prop <- 100 * prop / apply(prop, 1, sum) # Fraction of clusters according to the urban gradient

  par(mar = c(6, 19, 2, 14))
  matplot(prop, type = "b", lty = 1, col = colo, pch = 16, axes = FALSE, xlab = "", ylab = "", xlim = c(0.6, 3.25), ylim = c(0, 30), lwd = 2.5, cex = 2.5, yaxs = "i")
  axis(1, at = c(1, 2, 3), labels = c("Inner cities", "Urban areas", "Peripheral areas"), las = 1, cex.axis = 1.8, padj = -0.05, tick = FALSE)
  axis(2, las = 1, cex.axis = 1.75)
  mtext("Urban gradient", 1, line = 4.5, cex = 1.75)
  mtext("Percentage", 2, line = 4, cex = 1.75)
  legend("topleft", inset = c(-0.39, -0.15), legend = "B", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  segments(1.5, -3, 1.5, 40.5, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(2.5, -3, 2.5, 40.5, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(3.5, -3, 3.5, 40.5, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)

  # C
  tab <- shp[, c(70, 14), drop = TRUE]
  prop <- table(tab[, 1], tab[, 2])
  prop <- 100 * t(prop) / apply(prop, 2, sum) # Percentage of clusters per profile

  par(mar = c(6, 10, 1, 6))
  barplot(t(prop[5:1, ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 1.8, names.arg = rev(c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5")), horiz = TRUE)
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 4.5, cex = 1.75)
  legend("topleft", inset = c(-0.33, -0.11), legend = "C", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

dev.off()

# Figure S6 (plot prop cluster per city in SI)
citnames <- as.character(cit[, 2])

prop <- table(id, clu)
prop <- 100 * prop / apply(prop, 1, sum) # Percentage of clusters per city

pdf("FigS6.pdf", width = 7.843750, height = 9.973753, useDingbats = FALSE)

  par(mar = c(5, 9, 3, 2))
  barplot(t(prop[rev(indfigsi), ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 0.9, names.arg = rev(citnames[indfigsi]), horiz = TRUE)
  axis(1, las = 1, cex.axis = 1.25)
  mtext("Percentage", 1, line = 3.5, cex = 1.5)

  legend("topleft", inset = c(-0.021, -0.07), fill = colo[1:4], border = colo[1:4], legend = c("Cluster 1  ", "Cluster 2  ", "Cluster 3  ", "Cluster 4"), bty = "n", cex = 1.25, xpd = TRUE, horiz = TRUE)
  legend("topleft", inset = c(-0.021, -0.03), fill = colo[5:8], border = colo[5:8], legend = c("Cluster 5  ", "Cluster 6  ", "Cluster 7  ", "Cluster 8"), bty = "n", cex = 1.25, xpd = TRUE, horiz = TRUE)

dev.off()

# Chi2 test
tab <- shp[, c(2, 3, 70), drop = TRUE] # Urban gradient
tab <- table(tab[, 2], tab[, 3])
chisq.test(tab)  

tab <- shp[, c(70, 14), drop = TRUE] # Profiles
tab <- table(tab[, 1], tab[, 2])
chisq.test(tab, simulate.p.value = TRUE) 








