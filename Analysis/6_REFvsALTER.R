# Load packages
library(sf)

# Working directory
# setwd("")

# Load Data
load("Data/Outputs/Data.Rdata")

# Extract Dominants & Subordinates
names(shp)[c(55, 60, 48)]
DOM <- shp[, c(55, 60, 48), drop = TRUE]
DOM <- apply(DOM, 1, mean) # Average mismatch between 'Dominants' social groups by districts

names(shp)[c(64, 65, 34)]
SUB <- shp[, c(64, 65, 34), drop = TRUE]
SUB <- apply(SUB, 1, mean) # Average mismatch between 'Subordinates' social groups by districts

city <- shp$city # City
clu <- shp$cluster # Cluster
zone <- shp$zone # Zone
tot <- shp$tot # Profile 'All'

tab <- data.frame(City=city, Zone=zone, Profile=tot, DOM, SUB) # Merge
tab <- tab[clu >= 1, ] # Filter out districts without cluster

sum(tab$DOM > 0) / dim(tab)[1] # % non-zero mismatch REF
sum(tab$SUB > 0) / dim(tab)[1] # % non-zero mismatch ALTER

# Table 1 (All)
tabi <- tab

mu <- t(matrix(c(mean(tabi[, 4]), mean(tabi[, 5]))))
colnames(mu) <- c("DOM", "SUB")

sd <- t(matrix(c(sd(tabi[, 4]), sd(tabi[, 5]))))
colnames(sd) <- c("DOM", "SUB")

tabmu <- round(mu, digits = 3)
tabsd <- round(sd, digits = 3)
tabnb <- length(tabi[, 3])

tex <- paste("All", " & \textit{", as.numeric(tabnb), "} & ", tabmu[1], " (", tabsd[1], ")", " & ", tabmu[2], " (", tabsd[2], ")", "\\", sep = "")

print(data.frame(tex), row.names = FALSE)

# Table 1 (Per urban gradient)
tabi <- tab

mu <- aggregate(tabi[, 4:5], list(tabi[, 2]), mean)
colnames(mu) <- c("GRA", "DOM", "SUB")

sd <- aggregate(tabi[, 4:5], list(tabi[, 2]), sd)
colnames(sd) <- c("GRA", "DOM", "SUB")

tabmu <- round(mu[3:1, -1], digits = 3)
tabsd <- round(sd[3:1, -1], digits = 3)
tabnb <- table(tabi[, 2])[3:1]

idcol <- c("Inner cities", "Urban areas", "Peripheral areas")
tex <- paste(idcol, " & \textit{", as.numeric(tabnb), "} & ", tabmu[, 1], " (", tabsd[, 1], ")", " & ", tabmu[, 2], " (", tabsd[, 2], ")", "\\", sep = "")

print(data.frame(tex), row.names = FALSE)

# Table 1 (Per profiles)
tabi <- tab

mu <- aggregate(tabi[, 4:5], list(tabi[, 3]), mean)
colnames(mu) <- c("PRO", "DOM", "SUB")

sd <- aggregate(tabi[, 4:5], list(tabi[, 3]), sd)
colnames(sd) <- c("PRO", "DOM", "SUB")

tabmu <- round(mu[, -1], digits = 3)
tabsd <- round(sd[, -1], digits = 3)
tabnb <- table(tabi[, 3])

idcol <- c("Profile 1", "Profile 2", "Profile 3", "Profile 4", "Profile 5")
tex <- paste(idcol, " & \textit{", as.numeric(tabnb), "} & ", tabmu[, 1], " (", tabsd[, 1], ")", " & ", tabmu[, 2], " (", tabsd[, 2], ")", "\\", sep = "")

print(data.frame(tex), row.names = FALSE)

