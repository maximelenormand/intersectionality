# Load packages
library(sf)
library(TeachingDemos)
library(plotrix)

# Working directory
# setwd("")

# Load Data
load("Data/Outputs/Data.Rdata")

AUT <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_AUT_0.shp") # Austria's administrative boundaries
BEL <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_BEL_0.shp") # Belgium's administrative boundaries
CHE <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_CHE_0.shp") # Switzerland's administrative boundaries
DEU <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_DEU_0.shp") # Germany's administrative boundaries
ESP <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_ESP_0.shp") # Spain's administrative boundaries
FRA <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_FRA_0.shp") # France's administrative boundaries
GBR <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_GBR_0.shp") # United Kingdom's administrative boundaries
ITA <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_ITA_0.shp") # Italy's administrative boundaries
LUX <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_LUX_0.shp") # Luxembourg's administrative boundaries
NLD <- st_read("Data/RAW/Additional Data/Administratives Boundaries/gadm36_NLD_0.shp") # Netherlands' administrative boundaries

CIT <- st_read("Data/RAW/Additional Data/LonLat_Cities.csv", options = c("X_POSSIBLE_NAMES=Longitude", "Y_POSSIBLE_NAMES=Latitude"))
st_crs(CIT) <- 4326
CIT <- st_transform(CIT, 27572)

# Separate Martinique, Metropolitan and Réunion
MAR <- shp[shp$city == "FORT DE FRANCE", 1:3]
MET <- shp[shp$city != "FORT DE FRANCE" & shp$city != "SAINT DENIS", 1:3]
REU <- shp[shp$city == "SAINT DENIS", 1:3]

MAR <- st_transform(MAR, 5490) # 5490 Martinique
MET <- st_transform(MET, 27572) # Europe Lambert Conformal Conic
REU <- st_transform(REU, 3727) # 3727 Réunion

# Build background
AUT <- st_transform(AUT, 27572) # Europe Lambert Conformal Conic
BEL <- st_transform(BEL, 27572)
CHE <- st_transform(CHE, 27572)
DEU <- st_transform(DEU, 27572)
ESP <- st_transform(ESP, 27572)
FRA <- st_transform(FRA, 27572)
GBR <- st_transform(GBR, 27572)
ITA <- st_transform(ITA, 27572)
LUX <- st_transform(LUX, 27572)
NLD <- st_transform(NLD, 27572)

REG <- st_union(AUT, BEL) # Surrounding lands
REG <- st_union(REG, CHE)
REG <- st_union(REG, DEU)
REG <- st_union(REG, ESP)
REG <- st_union(REG, GBR)
REG <- st_union(REG, ITA)
REG <- st_union(REG, LUX)
REG <- st_union(REG, NLD)

box <- st_bbox(MET) # Box Metropolitan
box[1] <- box[1] - 170000
box[2] <- box[2] - 200000
box[3] <- box[3] + 200000
box[4] <- box[4] + 50000
box <- st_as_sfc(box)
boxmet <- box

REG <- st_intersection(REG, boxmet) # Intersect Surrounding lands and Box Metropolitan

# Extract Zone France
Z1=MET[MET$zone==1,]
Z1=st_union(Z1)
Z2 <- MET[MET$zone == 2, ]
Z2 <- st_union(Z2)
Z3 <- MET[MET$zone == 3, ]
Z3 <- st_union(Z3)

# Colors
coloboxmet <- "#D4EDFF"
coloreg <- "#EDECEC"
colofra <- "#BCC5D5"

coloz1 <- "#ECA07E"
coloz2 <- "#CF5D4B"
coloz3 <- "#AC172A"

# Figure 2
pdf("Fig2.pdf", width = 7, height = 6, useDingbats = FALSE)

  # France
  par(mar = c(0, 0, 0, 0))
  plot(boxmet, col = coloboxmet, border = coloboxmet)
  plot(st_geometry(REG), col = coloreg, border = coloreg, add = TRUE)
  plot(st_geometry(FRA), col = colofra, border = colofra, add = TRUE)
  plot(st_geometry(Z1), col = coloz1, border = coloz1, add = TRUE)
  plot(st_geometry(Z2), col = coloz2, border = coloz2, add = TRUE)
  plot(st_geometry(Z3), col = coloz3, border = coloz3, add = TRUE)

  legend("topright", inset = c(0.04, 0.03), fill = c(coloz3, coloz2, coloz1), legend = c("Inner cities", "Urban areas", "Peripheral areas"), border = NA, bty = "n", cex = 1.1, xpd = NA)

  segments(st_bbox(boxmet)[1] + 100000, st_bbox(boxmet)[2] + 100000, st_bbox(boxmet)[1] + 300000, st_bbox(boxmet)[2] + 100000, lwd = 3, col = "grey2")
  text(st_bbox(boxmet)[1] + 200000, st_bbox(boxmet)[2] + 60000, labels = "200 km", cex = 1.25, font = 2, col = "grey2")

  draw.circle(st_bbox(boxmet)[1] + 200000, st_bbox(boxmet)[2] + 600000, 95000, lwd = 2, border = colofra)
  draw.circle(st_bbox(boxmet)[1] + 200000, st_bbox(boxmet)[2] + 380000, 95000, lwd = 2, border = colofra)

  # Cities
  text(st_coordinates(CIT)[1, 1] + 5000, st_coordinates(CIT)[1, 2] + 25000, labels = "Paris", cex = 0.9)
  text(st_coordinates(CIT)[2, 1] + 0, st_coordinates(CIT)[2, 2] + 40000, labels = "Marseille", cex = 0.9)
  text(st_coordinates(CIT)[3, 1] + 0, st_coordinates(CIT)[3, 2] + 40000, labels = "Lyon", cex = 0.9)
  text(st_coordinates(CIT)[4, 1] + 21000, st_coordinates(CIT)[4, 2] + 30000, labels = "Bordeaux", cex = 0.9)
  text(st_coordinates(CIT)[5, 1] + 0, st_coordinates(CIT)[5, 2] + 30000, labels = "Nantes", cex = 0.9)

  # Martinique
  subplot(fun = {
    plot(st_geometry(MAR[MAR$zone == 1, ]), col = coloz1, border = coloz1)
    plot(st_geometry(MAR[MAR$zone == 2, ]), col = coloz2, border = coloz2, add = TRUE)
    plot(st_geometry(MAR[MAR$zone == 3, ]), col = coloz3, border = coloz3, add = TRUE)
  }, x = grconvertX(c(0.11, 0.21), from = "npc"), y = grconvertY(c(0.47, 0.57), from = "npc"), type = "fig", pars = list(mar = c(0, 0, 0, 0)))

  text(st_bbox(boxmet)[1] + 152000, st_bbox(boxmet)[2] + 592000, labels = "Fort-", cex = 0.65)
  text(st_bbox(boxmet)[1] + 180000, st_bbox(boxmet)[2] + 560000, labels = "de-France", cex = 0.65)

  # Réunion
  subplot(fun = {
    plot(st_geometry(REU[REU$zone == 1, ]), col = coloz1, border = coloz1)
    plot(st_geometry(REU[REU$zone == 2, ]), col = coloz2, border = coloz2, add = TRUE)
    plot(st_geometry(REU[REU$zone == 3, ]), col = coloz3, border = coloz3, add = TRUE)
  }, x = grconvertX(c(0.128, 0.228), from = "npc"), y = grconvertY(c(0.29, 0.39), from = "npc"), type = "fig", pars = list(mar = c(0, 0, 0, 0)))

  text(st_bbox(boxmet)[1] + 200000, st_bbox(boxmet)[2] + 451000, labels = "St-Denis", cex = 0.65, xpd = NA)

dev.off()


