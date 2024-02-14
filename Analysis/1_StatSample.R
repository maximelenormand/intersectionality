# Load packages
library(RColorBrewer)

# Working directory
# setwd("")

# Load Data
gen <- read.csv2(paste("Data/RAW/SAMPLE/data_sex.csv", sep = ""), stringsAsFactors = FALSE) # Info sample gender
age <- read.csv2(paste("Data/RAW/SAMPLE/data_kage.csv", sep = ""), stringsAsFactors = FALSE) # Info sample age
edu <- read.csv2(paste("Data/RAW/SAMPLE/data_educ.csv", sep = ""), stringsAsFactors = FALSE) # Info sample edu

# Normalize to get a fraction
gen <- gen[, 4:5] / sum(gen[, 4:5])
age <- age[, 6:9] / sum(age[, 6:9])
edu <- edu[, 7:11] / sum(edu[, 7:11])

nb <- c(apply(gen, 2, sum), apply(age, 2, sum), apply(edu, 2, sum))

# Figure 1
cat <- c("Men", "Women", "16-24 years", "25-34 years", "35-64 years", "65 and more", "Low educ.", "Middle-low educ.", "Middle-high educ.", "High educ.", "Not Available")

colo <- brewer.pal(3, "Accent")
colo2 <- colo[c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3)]

pdf("Fig1.pdf", width = 6.75, height = 7, useDingbats = FALSE)
  par(mar = c(4.5, 9, 0, 1))
  xpos <- barplot(nb[rev(1:length(nb))], col = colo2[rev(1:length(colo2))], border = NA, names.arg = "", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 0.6), horiz = TRUE)
  axis(1, las = 1, cex.axis = 1.3)
  text(-0.01, xpos, cat[rev(1:length(cat))], xpd = NA, cex = 1.1, pos = 2, font = c(4, rep(2, 10)))
  mtext("Fraction of individuals", 1, line = 3.25, cex = 1.75)
  legend("bottomright", inset = c(-0.03, 0), fill = colo, legend = c("Gender", "Age", "Education"), border = NA, bty = "n", cex = 1.6, xpd = NA)
dev.off()
