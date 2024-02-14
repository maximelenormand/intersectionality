# Load packages
library(sf)
sf::sf_use_s2(FALSE)
#library(lwgeom)
#library(DescTools)
#library(tidyverse)
library(scales)

# Working directory
#setwd("")

# Load Data
load("Data/Outputs/Data.Rdata")
tab=read.csv2("Data/Outputs/Signals_L2.csv",stringsAsFactors=FALSE)[,1:29]
rho=read.csv2("Vizu/rho.csv",stringsAsFactors=FALSE)

# Names
cit=read.csv2("Data/RAW/Additional Data/Table_Cities.csv")
cit=cit[order(cit[,1]),]
cit=data.frame(ID=as.character(cit[,1]),Name=as.character(c("Albi","Alençon","Amiens","Angers","Angoulême","Annecy","Annemasse","Bayonne","Besançon","Béziers","Bordeaux","Brest","Caen","Carcassonne","Cherbourg","Clermont-Ferrand","Creil","Dijon","Douai","Dunkerque","Fort-de-France","Grenoble","La Rochelle","Le Havre","Lille","Longwy","Lyon","Marseille","Metz","Montpellier","Nancy","Nantes","Nice","Nîmes","Niort","Paris","Poitiers","Quimper","Rennes","Rouen","Saint-Brieuc","St-Denis (La Réunion)","Saint-Etienne","Strasbourg","Thionville","Toulouse","Tours","Valence","Valenciennes")))
temp=shp$city
cit[,1]=as.character(sort(temp[!duplicated(temp)]))

cat=c("All","Men","Women","16-24 years","25-34 years","35-64 years","65 and more","Low educ.", "Middle-low educ.", "Middle-high educ.", "High educ.")

mismatch=rbind(substr(names(shp)[15:69],1,4),substr(names(shp)[15:69],6,9))

names=list(cit=cit,cat=cat,mismatch=mismatch)

# Clean SHP
#shpint=st_intersection(shp,shp)
#temp=shpint[,c(1,2,71,72)]
#temp=temp %>% filter(st_is(. , c("POLYGON","MULTIPOLYGON")))
#temp=temp[temp[,2]!=temp[,4],]
#
#atemp=st_area(temp)
#atemp1=st_area(shp[match(as.character(temp[,1,drop=TRUE]),as.character(shp$id)),])
#atemp2=st_area(shp[match(as.character(temp[,3,drop=TRUE]),as.character(shp$id)),])
#ratio=data.frame(ID1=as.character(temp[,1,drop=TRUE]),ID2=as.character(temp[,3,drop=TRUE]),r1=as.numeric(100*atemp/atemp1),r2=as.numeric(100*atemp/atemp2))
#ratio=ratio[ratio[,3]>50 | ratio[,4]>50,]

shp=shp[-c(match(c("QUIMPER_1","LE HAVRE_37","CAEN_211","CAEN_214","LONGWY_113"),shp$id)),]

# Extract bbox
shp=st_transform(shp,4326)
bbox=st_bbox(shp)
bbox=rbind(bbox,st_bbox(shp[shp$city!="SAINT DENIS" & shp$city!="FORT DE FRANCE",]))
for(i in 1:dim(cit)[1]){
  bbox=rbind(bbox,st_bbox(shp[shp[,2,drop=TRUE]==cit[i,1],]))
}
bbox=data.frame(c("ALL","METRO",as.character(cit[,1])),bbox)
colnames(bbox)[1]="Bbox"
rownames(bbox)=c("ALL","METRO",as.character(cit[,1]))

# Stats profiles by city (total and urban gradient) and social group
propsoc=list()
length(propsoc)=51
names(propsoc)=c("ALL","METRO",as.character(cit[,1]))
propsoc$ALL=table(tab[,3],tab[,29])[c(11,9,10,1:8),]
propsoc$ALL=propsoc$ALL/apply(propsoc$ALL,1,sum)
propsoc$METRO=table(tab[tab[,2]!="SAINT DENIS" & tab[,2]!="FORT DE FRANCE",3],tab[tab[,2]!="SAINT DENIS" & tab[,2]!="FORT DE FRANCE",29])[c(11,9,10,1:8),]
propsoc$METRO=propsoc$METRO/apply(propsoc$METRO,1,sum)
for(i in 1:dim(cit)[1]){
   propsoc[[cit[i,1]]]=table(tab[tab[,2]==cit[i,1],3],tab[tab[,2]==cit[i,1],29])[c(11,9,10,1:8),]
   propsoc[[cit[i,1]]]=propsoc[[cit[i,1]]]/apply(propsoc[[cit[i,1]]],1,sum)
}

# Save data
save(names, shp, bbox, profiles, propsoc, rho, file="Vizu/Data.Rdata")

# Generate and export popups 
colo=alpha(c("#1B7FAD", "#6D5B96", "#26938A", "#F27A2E", "#D64261"),0.7)
if(dir.exists("Vizu/WWW/popups")){
  unlink("Vizu/WWW/popups", recursive = TRUE)
}
dir.create("Vizu/WWW/popups")

fij=shp[,c(14,12:13,4:11),drop=TRUE]
for(i in 1:dim(shp)[1]){

    print(i)

    fiji=as.numeric(fij[i,])

    sigi=rbind(profiles$mu[fiji[!duplicated(fiji)],-1])
    coloi=colo[fiji[!duplicated(fiji)]]
    cololegi=colo[fiji]
   
    png(paste0("Vizu/WWW/popups/",i,".png"), width=1000, height=600)

      par(mar=c(6,9,5,21))
      matplot(1:24,t(sigi),col=coloi,cex=2.5,pch=16,ylim=c(0.02,0.07),axes=FALSE,xlab="",ylab="")   
      axis(1, las=1, cex.axis=2)
      axis(2, las=1, cex.axis=2)
      mtext("Time of day", 1, line=4, cex=2.5)
      mtext("Normalized volume", 2, line=6, cex=2.5)
      box(lwd=1.5)
      
      idi=shp[i,1,drop=TRUE]
      idi=strsplit(idi,"_")
      title(paste0(as.character(cit[,2][cit[,1]==idi[[1]][1]])," District ",idi[[1]][2]),cex.main=2.5)

      legend("topright",inset=c(-0.19,-0.05), col=cololegi[1], pch=16, legend="All", bty="n", cex=2.5,xpd=NA) 
      legend("topright",inset=c(-0.31,0.07), col=cololegi[2:3], pch=16, legend=c("Men","Women"), bty="n", cex=2.5,xpd=NA) 
      legend("topright",inset=c(-0.427,0.28), col=cololegi[4:7], pch=16, legend=c("16-24 years","25-34 years","35-64 years","65 and more"), bty="n", cex=2.5,xpd=NA) 
      legend("topright",inset=c(-0.55,0.65), col=cololegi[8:11], pch=16, legend=c("Low educ.", "Middle-low educ.", "Middle-high educ.", "High educ."), bty="n", cex=2.5,xpd=NA) 

    dev.off()

}







