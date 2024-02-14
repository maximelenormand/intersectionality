# Load packages
library(RColorBrewer)

# Working directory
# setwd("")

# Load Data
load("Data/Outputs/Data.Rdata")

# Cities
cit <- read.csv2("Data/RAW/Additional Data/Table_Cities.csv") # Info cities
cit <- cit[order(cit[, 1]), ] # Rank city by id
cit <- data.frame(ID = as.character(cit[, 1]), Name = as.character(c("Albi", "Alençon", "Amiens", "Angers", "Angoulême", "Annecy", "Annemasse", "Bayonne", "Besançon", "Béziers", "Bordeaux", "Brest", "Caen", "Carcassonne", "Cherbourg", "Clermont-Ferrand", "Creil", "Dijon", "Douai", "Dunkerque", "Fort-de-France", "Grenoble", "La Rochelle", "Le Havre", "Lille", "Longwy", "Lyon", "Marseille", "Metz", "Montpellier", "Nancy", "Nantes", "Nice", "Nîmes", "Niort", "Paris", "Poitiers", "Quimper", "Rennes", "Rouen", "Saint-Brieuc", "St-Denis (La Réunion)", "Saint-Etienne", "Strasbourg", "Thionville", "Toulouse", "Tours", "Valence", "Valenciennes")), Pop = as.numeric(cit[, 2]), Fig = as.numeric(cit[, 3])) # Add a column with names to display
cit <- cit[order(cit[, 3], decreasing = TRUE), ] # Rank by population in inner cities
indfig <- as.numeric(rownames(cit)[!is.na(cit[, 4])]) # Identify the 23 cities to display in the main text (the six more populated and also those with the more recent data)
indfigsi <- as.numeric(rownames(cit)) # Id of the fig rank by size for the plots in SI
cit <- cit[order(cit[, 1]), ] # Rank again by id

# Social groups
cat <- c("All", "Men", "Women", "16-24 years", "25-34 years", "35-64 years", "65 and more", "Low educ.", "Middle-low educ.", "Middle-high educ.", "High educ.")

# Normalize data and boxplots
sigmu <- profiles$mu[, -1] # Average signals
sigsd <- profiles$sd[, -1] # Standard devialtion signals

propsoc <- 100 * stats$soc / apply(stats$soc, 1, sum) # Percentage of profiles by social group

round(100 * apply(stats$soc, 2, sum) / sum(stats$soc), digits = 1) # % of sociodistricts per profile

propcit <- 100 * stats$cit / apply(stats$cit, 1, sum) # Percentage of profiles by cities
propcit1 <- 100 * stats$cit1 / apply(stats$cit1, 1, sum) # Percentage of profiles in peripheral areas by cities
propcit2 <- 100 * stats$cit2 / apply(stats$cit2, 1, sum) # Percentage of profiles in urban areas by cities
propcit3 <- 100 * stats$cit3 / apply(stats$cit3, 1, sum) # Percentage of profiles in inner cities by cities

box <- list() # List of values for the boxplots
box[[1]] <- propcit3[, 1]
box[[2]] <- propcit3[, 2]
box[[3]] <- propcit3[, 3]
box[[4]] <- propcit3[, 4]
box[[5]] <- propcit3[, 5]
box[[6]] <- propcit2[, 1][!is.na(propcit2[, 1])]
box[[7]] <- propcit2[, 2][!is.na(propcit2[, 2])]
box[[8]] <- propcit2[, 3][!is.na(propcit2[, 3])]
box[[9]] <- propcit2[, 4][!is.na(propcit2[, 4])]
box[[10]] <- propcit2[, 5][!is.na(propcit2[, 5])]
box[[11]] <- propcit1[, 1]
box[[12]] <- propcit1[, 2]
box[[13]] <- propcit1[, 3]
box[[14]] <- propcit1[, 4]
box[[15]] <- propcit1[, 5]

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { # Use decile
  b$stats[1, i] <- quantile(box[[i]], 0.1)
  b$stats[5, i] <- quantile(box[[i]], 0.9)
}

# Figure 3
colo <- c("#1B7FAD", "#6D5B96", "#26938A", "#F27A2E", "#D64261")
pdf("Fig3.pdf", width = 13.718750, height = 9.163386, useDingbats = FALSE)

  layout(matrix(c(1, 2, 3, 4, 5, 5), 3, 2, byrow = TRUE), width = c(1, 1), height = c(1, 1.2, 0.1))

  # A
  par(mar = c(5.5, 6.2, 2, 2))
  matplot(1:24, t(sigmu), type = "l", lty = 1, col = colo, lwd = 3, axes = FALSE, xlab = "", ylab = "")
  axis(1, at = c(3, 6, 9, 12, 15, 18, 21), las = 1, cex.axis = 1.5)
  axis(2, las = 1, cex.axis = 1.5)
  mtext("Time of day", 1, line = 3.5, cex = 1.5)
  mtext("Normalized volume", 2, line = 4.5, cex = 1.5)
  box(lwd = 1.5)
  legend("topleft", inset = c(-0.23, -0.15), legend = "A", bty = "n", cex = 2.75, xpd = TRUE, text.font = 2)

  # B
  par(mar = c(5.5, 10, 1.2, 2))
  barplot(t(propsoc[c(11, 9, 10, 1:8), ]), horiz = TRUE, col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 1.5, names.arg = cat, space = c(0.1, 0.4, 0.1, 0.4,   0.1, 0.1, 0.1, 0.4, 0.1, 0.1, 0.1))
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 3.5, cex = 1.5)
  legend("topleft", inset = c(-0.35, -0.11), legend = "B", bty = "n", cex = 2.75, xpd = TRUE, text.font = 2)

  # C
  par(mar = c(11.5, 6.2, 2, 0.5))
  barplot(t(propcit[indfig, 5:1]), col = rev(colo), border = NA, ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 1.5, names.arg = cit[indfig, 2])
  axis(2, las = 1, cex.axis = 1.5)
  mtext("Percentage", 2, line = 4.5, cex = 1.5)
  legend("topleft", inset = c(-0.22, -0.15), legend = "C", bty = "n", cex = 2.75, xpd = TRUE, text.font = 2)

  # D
  par(mar = c(11.5, 9.5, 1.3, 2))
  bxp(b, at = c(1:5, 7:11, 13:17), outline = FALSE, boxcol = colo, whiskcol = colo, whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.75, staplecol = colo, medbg = colo, boxfill = colo, cex.axis = 1.5, las = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 50))
  axis(1, at = c(3, 9, 15), labels = c("Inner cities", "Urban areas", "Peripheral areas"), las = 1, cex.axis = 1.5, padj = 1, tick = FALSE)
  axis(2, las = 1, cex.axis = 1.5)
  mtext("Urban gradient", 1, line = 6, cex = 1.5)
  mtext("Percentage", 2, line = 4.5, cex = 1.5)
  legend("topleft", inset = c(-0.34, -0.11), legend = "D", bty = "n", cex = 2.75, xpd = TRUE, text.font = 2)

  segments(6, -10, 6, 53, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(12, -10, 12, 53, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)

  # Legend
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("bottomleft", inset = c(0.25, 0), fill = colo, border = colo, legend = c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5"), bty = "n", cex = 2, xpd = TRUE, horiz = TRUE)

dev.off()

# Figure S2 (plot profiles + SD in SI)
pdf("FigS2.pdf", width = 15.020833, height = 7.584208, useDingbats = FALSE)

  mini <- c(0.02, 0.03, 0.03, 0.025, 0.015)
  maxi <- c(0.08, 0.06, 0.05, 0.055, 0.06)

  mini <- c(0, 0, 0, 0, 0)
  maxi <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0)

  layout(matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE), width = c(1, 1, 1), height = c(1, 1))

  for (i in 1:5) {
    par(mar = c(5.5, 7, 2, 2))
    plot(1:24, t(sigmu[i, ]), type = "l", lty = 1, col = colo[i], lwd = 3, axes = FALSE, xlab = "", ylab = "", ylim = c(mini[i], maxi[i]))
    par(new = TRUE)
    plot(1:24, t(sigmu[i, ] - sigsd[i, ]), type = "l", lty = 2, col = colo[i], lwd = 3, axes = FALSE, xlab = "", ylab = "", ylim = c(mini[i], maxi[i]))
    par(new = TRUE)
    plot(1:24, t(sigmu[i, ] + sigsd[i, ]), type = "l", lty = 2, col = colo[i], lwd = 3, axes = FALSE, xlab = "", ylab = "", ylim = c(mini[i], maxi[i]))
    axis(1, at = c(3, 6, 9, 12, 15, 18, 21), las = 1, cex.axis = 1.5)
    axis(2, las = 1, cex.axis = 1.5)
    mtext("Time of day", 1, line = 3.5, cex = 1.5)
    mtext("Normalized volume", 2, line = 5, cex = 1.5)
    box(lwd = 1.5)
  }

  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("topleft", inset = c(0.25, 0.18), col = colo, lty = 1, lwd = 3, legend = c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5"), bty = "n", cex = 2.5, xpd = TRUE)

dev.off()

# Chi2 test
chisq.test(stats$soc[1:4, ]) # Age
chisq.test(stats$soc[5:8, ]) # Education
chisq.test(stats$soc[9:10, ]) # Gender

pvalcit <- NULL # Cities
for (i in 1:49) {
  pvalcit <- c(pvalcit, chisq.test(rbind(stats$cit[i, ], apply(stats$cit[-i, ], 2, sum)))[[3]])
}
pvalcit <- as.matrix(pvalcit)
rownames(pvalcit) <- rownames(stats$cit)
sum(pvalcit < 0.01)

chisq.test(cbind(apply(stats$cit1, 2, sum), apply(stats$cit2, 2, sum), apply(stats$cit2, 2, sum))) # Urban gradient

# Figure S3 (plot fraction profiles per city according to the distance in SI)
id <- shp[, 2, drop = TRUE]
citnames <- as.character(cit[, 2])
citnames[pvalcit < 0.01] <- paste0(citnames[pvalcit < 0.01], "*")

pdf("FigS3.pdf", width = 18.750000, height = 8.955599, useDingbats = FALSE)

  layout(matrix(c(1, 2, 3, 4, 5, 5, 5, 5), 2, 4, byrow = TRUE), width = c(1.3, 1, 1, 1), height = c(1, 0.07))

  # A
  par(mar = c(6, 12, 0.7, 2))
  barplot(t(propcit[rev(indfigsi), ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 1.3, names.arg = rev(citnames[indfigsi]), horiz = TRUE)
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 4, cex = 1.25)
  legend("topleft", inset = c(-0.55, -0.04), legend = "A", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  # B
  par(mar = c(6, 2, 0.7, 2))
  barplot(t(propcit3[rev(indfigsi), ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, horiz = TRUE, names.arg = rep("", 49))
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 4, cex = 1.25)
  legend("topleft", inset = c(-0.16, -0.04), legend = "B", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  # C
  par(mar = c(6, 2, 0.7, 2))
  barplot(t(propcit2[rev(indfigsi), ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, horiz = TRUE, names.arg = rep("", 49))
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 4, cex = 1.25)
  legend("topleft", inset = c(-0.16, -0.04), legend = "C", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  # D
  par(mar = c(6, 2, 0.7, 2))
  barplot(t(propcit1[rev(indfigsi), ]), col = colo, border = NA, xlim = c(0, 100), axes = FALSE, xlab = "", ylab = "", las = 2, horiz = TRUE, names.arg = rep("", 49))
  axis(1, las = 1, cex.axis = 1.5)
  mtext("Percentage", 1, line = 4, cex = 1.25)
  legend("topleft", inset = c(-0.16, -0.04), legend = "D", bty = "n", cex = 3, xpd = TRUE, text.font = 2)

  # Legend
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("bottomleft", inset = c(0.335, 0), fill = colo, border = colo, legend = c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5"), bty = "n", cex = 2.1, xpd = TRUE, horiz = TRUE)

dev.off()




