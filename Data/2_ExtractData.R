# Load packages
library(DescTools)
library(sf)
sf::sf_use_s2(FALSE)
library(tidyverse)

# Working directory
# setwd("")

# Load Data
tab <- read.csv2(paste("Data/Outputs/Signals_L2.csv", sep = ""), stringsAsFactors = FALSE)[, 1:29] # Signals_L2 produced at the previous step

idtab <- paste0(tab[, 2], "_", as.numeric(tab[, 4])) # ID district
idistrict <- sort(idtab[!duplicated(idtab)])

namcit <- tab[, 2] # ID City
namcit <- sort(namcit[!duplicated(namcit)])

shp <- st_read("Data/RAW/SHP/v4_SEC_49ED_WGS84.shp") # Shapefile
shp <- st_transform(shp, 4326) # Project in lon/lat

# Modify shp
city <- as.character(shp$ENQUETE) # Rename IDF->PARIS, LA REUNION->SAINT DENIS and MARTINIQUE->FORT DE FRANCE
city[city == "IDF"] <- "PARIS"
city[city == "LA REUNION"] <- "SAINT DENIS"
city[city == "MARTINIQUE"] <- "FORT DE FRANCE"

shp <- shp %>% mutate(city = city, zone = shp$Zonage_Sec) # Three attributes: id, city and zone (1=Peripheral areas,2=Urban areas,3=Inner cities)
shp <- shp %>% mutate(id = paste0(as.character(shp$city), "_", as.numeric(as.character(shp$CODE_SEC))))
shp <- shp %>%
  group_by(id, city) %>%
  summarize(zone = mean(zone))

print(c(sum(shp$id == idistrict), length(shp$id), length(idistrict))) # Check

zone <- shp$zone[match(idtab, shp$id)] # Zone

# Average signals + Standard deviation
profiles <- list()
profiles$total <- apply(table(tab[, 2], tab[, 29]), 2, sum)
profiles$mu <- aggregate(tab[, -c(1, 2, 3, 4, 29)], list(tab[, 29]), mean)
colnames(profiles$mu)[1] <- "Profile"
profiles$sd <- aggregate(tab[, -c(1, 2, 3, 4, 29)], list(tab[, 29]), sd)
colnames(profiles$sd)[1] <- "Profile"

# Stats profiles by city (total and urban gradient) and social group
stats <- list()
stats$cit <- table(tab[, 2], tab[, 29])
stats$cit1 <- table(tab[zone == 1, 2], tab[zone == 1, 29])

stats$cit2 <- stats$cit1 # Need to do that because no zone 2 for 2 cities
stats$cit2[stats$cit2 > 0] <- 0
temp <- table(tab[zone == 2, 2], tab[zone == 2, 29])
stats$cit2[match(rownames(temp), rownames(stats$cit1)), ] <- temp

stats$cit3 <- table(tab[zone == 3, 2], tab[zone == 3, 29])

stats$soc <- table(tab[, 3], tab[, 29])

# Fij (identify the profile per social group and district)
fij <- as.matrix(xtabs(tab$Profile ~ idtab + tab$Socat))
idfij <- rownames(fij)

# Mismatch index
d <- as.matrix(dist(profiles$mu[, -1])) # Distance matrix between profiles

ref <- NULL # Mismatch between [sex, age and edu] versus [tot]
bet <- NULL # Mismatch between sex, age and edu
for (i in 1:dim(fij)[1]) { # Loop over districts

  indi <- as.numeric(fij[i, ]) # Profile of 11 sociodistricts
  indi[indi == 0] <- NA # Change 0 in NA
  di <- d[indi, indi] # Distance between the 11 profiles

  # Distance between tot and the others
  ref <- rbind(ref, di[11, -11])

  # Distance between sex, age and edu
  di <- di[-11, -11]
  colnames(di) <- colnames(fij)[-11]
  rownames(di) <- colnames(fij)[-11]

  x <- which(is.na(di) | !is.na(di), arr.ind = TRUE) # Change in three columns format
  rowx <- rownames(di)[x[, 1]]
  colx <- colnames(di)[x[, 2]]
  valx <- di[is.na(di) | !is.na(di)]

  beti <- valx[x[, 1] < x[, 2]]
  names(beti) <- paste0(rowx[x[, 1] < x[, 2]], "*", colx[x[, 1] < x[, 2]])

  bet <- rbind(bet, beti)
}

ref <- ref / max(d) # Normalize the distances by the total to obtain a mismatch
colnames(ref) <- colnames(fij)[-11]
colnames(ref) <- paste0(colnames(ref), "*tot")

bet <- bet / max(d) # Normalize the distances by the total to obtain a mismatch

mismatch <- cbind(ref, bet) # Merge the two tables


# Clustering WARD
mism <- mismatch[, c(55, 12, 13, 16, 36, 37, 38)] # Mismatch with reference category (male for gender, 35-64 years for age and high educ. for education)
ind <- !is.na(apply(mism, 1, sum)) # Remove district with at least one NA
mism <- mism[ind, ]

d <- as.matrix(dist(scale(mism))) # Dissimilarity matrix based on the Euclidean distance
n <- dim(d)[1]
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

pdf("FigS5.pdf", width = 8.322917, height = 5.811220, useDingbats = FALSE)
  par(mar = c(5, 7, 1, 1))
  par(mfrow = c(1, 1))
  plot(1:nclustmax, vars, typ = "b", cex = 2, lwd = 3, pch = 16, col = "steelblue3", axes = FALSE, xlab = "", ylab = "", xlim = c(1, nclustmax), ylim = c(0, 1))
  box(lwd = 1.5)
  axis(1, las = 1, cex.axis = 1.7, lwd = 1.5, padj = 0.2)
  axis(2, las = 1, cex.axis = 1.75, lwd = 1.5, at = seq(0, 1, 0.2))
  mtext("Number of clusters", 1, line = 3.5, cex = 2)
  mtext("Within / total variance", 2, line = 5, cex = 2)
  abline(v = 8, col = "#CC6666", lwd = 3)
dev.off()

# Check and rename clusters
clu8 <- cutree(h, 8)

temp <- clu8
clu8[temp == 1] <- 8
clu8[temp == 2] <- 2
clu8[temp == 3] <- 3
clu8[temp == 4] <- 4
clu8[temp == 5] <- 7
clu8[temp == 6] <- 6
clu8[temp == 7] <- 1
clu8[temp == 8] <- 5

table(clu8)

# Complete and export SHP
clushp8 <- rep(0, dim(shp)[1])
clushp8[ind] <- clu8

shp <- cbind(shp, fij, mismatch, cluster = clushp8) # Merge shp with Fij, the mismatch table and the clustering
if(dir.exists("Data/Outputs/SHP")){
   unlink("Data/Outputs/SHP", recursive = TRUE)
}
dir.create("Data/Outputs/SHP")
st_write(shp, "Data/Outputs/SHP/Data.shp")

# Save data
save(shp, profiles, stats, file = "Data/Outputs/Data.Rdata")




