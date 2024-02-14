# Load packages
library(RColorBrewer)
library(sf)
library(TeachingDemos)

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


# Format Data (10 columns per district: city, zone, profile all, social category, social group 1, social group 2, profile 1, profile 2, mismatch value, mismatch colnames)
temp <- cbind(shp[, c(2:3, 14, 12:13, 4:11), drop = TRUE], shp[, 14 + c(55, 11, 12, 14, 13, 15, 16, 25, 30, 36, 31, 37, 38), drop = TRUE])
tab <- NULL
for (i in 1:dim(temp)[1]) {
  citi <- as.character(temp[i, 1]) # City
  zoni <- as.numeric(temp[i, 2]) # Zone
  profiall <- as.numeric(temp[i, 3]) # Profile 'All'
  cat1 <- colnames(temp)[c(4, 6, 6, 6, 7, 7, 8, 10, 10, 10, 11, 11, 12)] # Name of first social group ("sex1" "age1" "age1" "age1" "age2" "age2" "age3" "edu1" "edu1" "edu1" "edu2" "edu2" "edu3")
  cat2 <- colnames(temp)[c(5, 7, 8, 9, 8, 9, 9, 11, 12, 13, 12, 13, 13)] # Name of second social group ("sex2" "age2" "age3" "age4" "age3" "age4" "age4" "edu2" "edu3" "edu4" "edu3" "edu4" "edu4")
  profi1 <- as.numeric(temp[i, c(4, 6, 6, 6, 7, 7, 8, 10, 10, 10, 11, 11, 12)]) # Profile of the first social group
  profi2 <- as.numeric(temp[i, c(5, 7, 8, 9, 8, 9, 9, 11, 12, 13, 12, 13, 13)]) # Profile of the second social group
  mismi <- as.numeric(temp[i, 14:26]) # Mismatch value
  catmism <- colnames(temp)[14:26] # Mismatch name to check that everything is ok
  typi <- c(rep("gender", 1), rep("age", 6), rep("edu", 6)) # Social category

  tabi <- data.frame(city = citi, zone = zoni, profileall = profiall, type = typi, cat1 = cat1, cat2 = cat2, profile1 = profi1, profile2 = profi2, mismatch = mismi, catmism = catmism)

  tab <- rbind(tab, tabi)
}

# Number of districts with at least one mismatch per category
t <- as.matrix(shp[, 14 + c(55, 11, 12, 14, 13, 15, 16, 25, 30, 36, 31, 37, 38), drop = TRUE])
t <- t[apply(is.na(t), 1, sum) == 0, ]
tg <- t[, 1]
ta <- t[, 2:7]
te <- t[, 8:13]

100 * sum(tg > 0) / length(tg) # gender
100 * sum(apply(ta > 0, 1, sum) > 0) / length(ta[, 1]) # age
100 * sum(apply(te > 0, 1, sum) > 0) / length(te[, 1]) # education
100 * sum(apply(t > 0, 1, sum) > 0) / length(t[, 1]) # all mismatch

# Process data for panel A (extract the mismatch distribution by social categories)
mulg <- tab[tab$type == "gender", 9]
mulg <- mulg[!is.na(mulg)]
mula <- tab[tab$type == "age", 9]
mula <- mula[!is.na(mula)]
mule <- tab[tab$type == "edu", 9]
mule <- mule[!is.na(mule)]
mul <- tab[, 9]
mul <- aggregate(mul, list(mul), length)[, 1]
mul[1] <- 0.002
mul[11] <- 0.998

# Process data for panel B (extract the average mismatch by city and social categories)
all <- aggregate(tab$mismatch, list(tab$city, tab$type), mean, na.rm = TRUE)
gen <- all[all[, 2] == "gender", 3]
age <- all[all[, 2] == "age", 3]
edu <- all[all[, 2] == "edu", 3]

# Process data for panel C (Extract boxplots of average mismatch per city by zone)
temp <- tab[tab$mismatch >= 0 & tab$zone == 3, ]
z1 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean, na.rm = TRUE)
temp <- tab[tab$mismatch >= 0 & tab$zone == 2, ]
z2 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean, na.rm = TRUE)
temp <- tab[tab$mismatch >= 0 & tab$zone == 1, ]
z3 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean, na.rm = TRUE)

box <- list() # List of values for the boxplots
box[[1]] <- z1[z1[, 2] == "gender", 3]
box[[2]] <- z1[z1[, 2] == "age", 3]
box[[3]] <- z1[z1[, 2] == "edu", 3]
box[[4]] <- z2[z2[, 2] == "gender", 3]
box[[5]] <- z2[z2[, 2] == "age", 3]
box[[6]] <- z2[z2[, 2] == "edu", 3]
box[[7]] <- z3[z3[, 2] == "gender", 3]
box[[8]] <- z3[z3[, 2] == "age", 3]
box[[9]] <- z3[z3[, 2] == "edu", 3]

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { # Use decile
  b$stats[1, i] <- quantile(box[[i]], 0.1)
  b$stats[5, i] <- quantile(box[[i]], 0.9)
}
bc <- b

# Process data for panel D (Extract boxplots of average mismatch per city by social groups)
gen1 <- all[all[, 2] == "gender", 3]
age1 <- tab[tab$cat1 == "age1" & tab$cat2 == "age3", ]
age1 <- aggregate(age1$mismatch, list(age1$city), mean, na.rm = TRUE)
age2 <- tab[tab$cat1 == "age2" & tab$cat2 == "age3", ]
age2 <- aggregate(age2$mismatch, list(age2$city), mean, na.rm = TRUE)
age4 <- tab[tab$cat1 == "age3" & tab$cat2 == "age4", ]
age4 <- aggregate(age4$mismatch, list(age4$city), mean, na.rm = TRUE)
edu1 <- tab[tab$cat1 == "edu1" & tab$cat2 == "edu4", ]
edu1 <- aggregate(edu1$mismatch, list(edu1$city), mean, na.rm = TRUE)
edu2 <- tab[tab$cat1 == "edu2" & tab$cat2 == "edu4", ]
edu2 <- aggregate(edu2$mismatch, list(edu2$city), mean, na.rm = TRUE)
edu3 <- tab[tab$cat1 == "edu3" & tab$cat2 == "edu4", ]
edu3 <- aggregate(edu3$mismatch, list(edu3$city), mean, na.rm = TRUE)

box <- list() # List of values for the boxplots
box[[1]] <- as.numeric(gen1)
box[[2]] <- as.numeric(age1[, 2])
box[[3]] <- as.numeric(age2[, 2])
box[[4]] <- as.numeric(age4[, 2])
box[[5]] <- as.numeric(edu1[, 2])
box[[6]] <- as.numeric(edu2[, 2])
box[[7]] <- as.numeric(edu3[, 2])

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { # Use decile
  b$stats[1, i] <- quantile(box[[i]], 0.1)
  b$stats[5, i] <- quantile(box[[i]], 0.9)
}
bd <- b

# Figure 4
pdf("Fig4.pdf", width = 15.7, height = 10.3, useDingbats = FALSE)

  layout(matrix(c(1, 2), 2, 1, byrow = TRUE), height = c(0.3, 1))

  colo <- brewer.pal(3, "Accent")

  # A
  par(mar = c(0, 11, 1, 1))

  f <- density(mulg, adjust = 0.5)
  plot(f$x, f$y, axes = FALSE, type = "l", lwd = 4, lty = 1, xlab = "", ylab = "", col = colo[1], xlim = c(0, 1), ylim = c(0, 20), xaxs = "i")
  par(new = TRUE)
  f <- density(mula, adjust = 0.5)
  plot(f$x, f$y, axes = FALSE, type = "l", lwd = 4, lty = 1, xlab = "", ylab = "", col = colo[2], xlim = c(0, 1), ylim = c(0, 20), xaxs = "i")
  par(new = TRUE)
  f <- density(mule, adjust = 0.5)
  plot(f$x, f$y, axes = FALSE, type = "l", lwd = 4, lty = 1, xlab = "", ylab = "", col = colo[3], xlim = c(0, 1), ylim = c(0, 20), xaxs = "i")

  rug(mul, col = "grey", lwd = 4)

  axis(2, las = 1, cex.axis = 1.7)
  mtext("PDF", 2, line = 3.5, cex = 2)
  legend("topleft", inset = c(-0.22, -0.21), legend = "A", bty = "n", cex = 2.5, xpd = TRUE, text.font = 2)

  legend("topright", inset = c(0, 0), fill = colo, legend = c("Gender", "Age", "Education"), border = NA, bty = "n", cex = 2, xpd = NA)

  # B
  par(mar = c(5, 11, 1, 1))
  plot(gen[rev(indfig)], 1:length(indfig), type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0.1, 0.4), col = colo[1], pch = 16, cex = 3)
  for (g in 1:length(indfig)) {
    abline(g, 0, col = "grey", lty = 3)
  }
  par(new = TRUE)
  plot(gen[rev(indfig)], 1:length(indfig), axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), col = colo[1], pch = 16, cex = 3, xaxs = "i")
  par(new = TRUE)
  plot(age[rev(indfig)], 1:length(indfig), axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), col = colo[2], pch = 17, cex = 3, xaxs = "i")
  par(new = TRUE)
  plot(edu[rev(indfig)], 1:length(indfig), axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), col = colo[3], pch = 18, cex = 3, xaxs = "i")
  axis(1, las = 1, cex.axis = 1.7)
  axis(2, at = 1:length(indfig), labels = rev(cit[indfig, 2]), las = 2, tick = FALSE, cex.axis = 1.7, font = 2, padj = 0.5)
  mtext("Mismatch values", 1, line = 3.5, cex = 2)
  box(lwd = 1.5)
  legend("topleft", inset = c(-0.22, -0.04), legend = "B", bty = "n", cex = 2.5, xpd = TRUE, text.font = 2)
  legend("topleft", inset = c(0.4, -0.04), legend = "C", bty = "n", cex = 2.5, xpd = TRUE, text.font = 2)
  legend("topleft", inset = c(0.4, 0.36), legend = "D", bty = "n", cex = 2.5, xpd = TRUE, text.font = 2)

  segments(mean(mulg), -1, mean(mulg), 26, lty = 2, lwd = 3, col = colo[1])
  segments(mean(mula), -1, mean(mula), 26, lty = 2, lwd = 3, col = colo[2])
  segments(mean(mule), -1, mean(mule), 26, lty = 2, lwd = 3, col = colo[3])

  # C
  subplot(fun = {
    bxp(bc, notch=TRUE, at = c(1:3, 5:7, 9:11), outline = FALSE, boxcol = colo, whiskcol = colo, whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.75, staplecol = colo, medbg = colo, boxfill = colo, cex.axis = 1.25, las = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.04, 0.3))
    axis(1, at = c(2, 6, 10), labels = c("Inner cities\n ", "Urban\n areas", "Peripheral\n areas"), las = 1, cex.axis = 1.25, padj = 0.4, tick = FALSE)
    axis(2, las = 1, cex.axis = 1.25)
    # mtext("Urban gradient", 1, line=2, cex=1.5)
    mtext("Mismatch", 2, line = 4, cex = 1.5)

    segments(4, 0.02, 4, 0.33, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
    segments(8, 0.02, 8, 0.33, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  }, x = grconvertX(c(0.5, 0.95), from = "npc"), y = grconvertY(c(0.49, 0.98), from = "npc"), type = "fig", pars = list(mar = c(5, 5, 0, 0)))

  # D
  subplot(fun = {
    colob <- colo[c(1, 2, 2, 2, 3, 3, 3)]
    bxp(bd, notch=TRUE, at = c(1, 3:5, 7:9), outline = FALSE, boxcol = colob, whiskcol = colob, whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.6, staplecol = colob, medbg = colob, boxfill = colob, las = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.05, 0.3))
    axis(1, at = 1, labels = c(""), las = 2, cex.axis = 1.25)
    axis(1, at = 3:5, labels = c("", "", ""), las = 2, cex.axis = 1.25)
    axis(1, at = 7:9, labels = c("", "", ""), las = 2, cex.axis = 1.25)

    text(x = 1 + 0.25, par("usr")[3] - 0.024, labels = c("Women"), srt = 45, pos = 2, xpd = TRUE, cex = 1.25)
    text(x = (3:5) + 0.25, par("usr")[3] - 0.024, labels = c("16-24 years", "25-34 years", "65 and more"), srt = 45, pos = 2, xpd = TRUE, cex = 1.25)
    text(x = (7:9) + 0.25, par("usr")[3] - 0.021, labels = c("Low educ.", "Middle-low educ.", "Middle-high educ."), srt = 45, pos = 2, xpd = TRUE, cex = 1.25)

    axis(2, las = 1, cex.axis = 1.25)
    mtext("Mismatch", 2, line = 4, cex = 1.5)
  }, x = grconvertX(c(0.5, 0.95), from = "npc"), y = grconvertY(c(0.01, 0.56), from = "npc"), type = "fig", pars = list(mar = c(7, 5, 0, 0)))

dev.off()

# Figure S4 (Average mismatch per city according to the profile and social group)
temp <- tab[tab$mismatch >= 0 & tab$profileall == 1, ]
p1 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean)
temp <- tab[tab$mismatch >= 0 & tab$profileall == 2, ]
p2 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean)
temp <- tab[tab$mismatch >= 0 & tab$profileall == 3, ]
p3 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean)
temp <- tab[tab$mismatch >= 0 & tab$profileall == 4, ]
p4 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean)
temp <- tab[tab$mismatch >= 0 & tab$profileall == 5, ]
p5 <- aggregate(temp$mismatch, list(temp$city, temp$type), mean)

box <- list() # List of values for the boxplots
box[[1]] <- p1[p1[, 2] == "gender", 3]
box[[2]] <- p1[p1[, 2] == "age", 3]
box[[3]] <- p1[p1[, 2] == "edu", 3]
box[[4]] <- p2[p2[, 2] == "gender", 3]
box[[5]] <- p2[p2[, 2] == "age", 3]
box[[6]] <- p2[p2[, 2] == "edu", 3]
box[[7]] <- p3[p3[, 2] == "gender", 3]
box[[8]] <- p3[p3[, 2] == "age", 3]
box[[9]] <- p3[p3[, 2] == "edu", 3]
box[[10]] <- p4[p3[, 2] == "gender", 3]
box[[11]] <- p4[p3[, 2] == "age", 3]
box[[12]] <- p4[p3[, 2] == "edu", 3]
box[[13]] <- p5[p5[, 2] == "gender", 3]
box[[14]] <- p5[p5[, 2] == "age", 3]
box[[15]] <- p5[p5[, 2] == "edu", 3]

b <- boxplot(box, plot = F) # Boxplots
for (i in 1:length(box)) { # Use decile
  b$stats[1, i] <- quantile(box[[i]], 0.1)
  b$stats[5, i] <- quantile(box[[i]], 0.9)
}

pdf("FigS4.pdf", width = 10.156250, height = 5.329724, useDingbats = FALSE)

  par(mar = c(3, 5.5, 1, 1))
  bxp(b, at = c(1:3, 5:7, 9:11, 13:15, 17:19), outline = FALSE, boxcol = colo, whiskcol = colo, whisklty = "solid", whisklwd = 2, staplelwd = 2, boxwex = 0.75, staplecol = colo, medbg = colo, boxfill = colo, cex.axis = 1.5, las = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 0.4))
  axis(1, at = c(2, 6, 10, 14, 18), labels = c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5"), las = 1, cex.axis = 1.5, padj = 0, tick = FALSE)
  axis(2, las = 1, cex.axis = 1.5)
  mtext("Mismatch", 2, line = 4, cex = 1.75)

  segments(4, -0.06, 4, 0.42, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(8, -0.06, 8, 0.42, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(12, -0.06, 12, 0.42, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)
  segments(16, -0.06, 16, 0.42, lwd = 1.5, xpd = TRUE, col = "grey", lty = 2)

  legend("topright", inset = c(0, 0), fill = colo, legend = c("Gender", "Age", "Education"), border = NA, bty = "n", cex = 1.25, xpd = NA)

dev.off()


