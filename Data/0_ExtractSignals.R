# Import packages
library(DescTools)

# Working directory
# setwd("")

# Download the archive at https://doi.org/10.5281/zenodo.7738571 and decompress it in the RAW folder.

# Clean data
cit <- list.files("Data/RAW")                         # Extract the list of cities
cit <- cit[cit != "odbl_en_49FrenchCityRegions.pdf"]  # Remove License (not a city)
cit <- cit[cit != "Additional Data"]                  # Remove folder Additional Data (not a city)
cit <- cit[cit != "SAMPLE"]                           # Remove folder SAMPLE (not a city)
cit <- cit[cit != "SHP"]                              # Remove folder SHP (not a city)
cit <- cit[cit != "README.md"]                        # Remove README (not a city)
ncit <- length(cit)                                   # Number of cities

tab <- NULL
for (i in 1:ncit) { # Loop cities

  print(c(cit[i], ncit))

  # Load Data
  sex <- read.csv(paste("Data/RAW/", cit[i], "/sex_prop_stacked.csv", sep = ""))
  age <- read.csv(paste("Data/RAW/", cit[i], "/age_prop_stacked.csv", sep = ""))
  edu <- read.csv(paste("Data/RAW/", cit[i], "/cleduc_prop_stacked.csv", sep = ""))

  # Sex
  temp <- sex
  temp[, 2] <- as.character(temp[, 2]) # Modify pm/am format to 1-24
  pmam <- substr(temp[, 2], nchar(temp[, 2]) - 1, nchar(temp[, 2]))
  temp[, 2] <- as.numeric(substr(temp[, 2], 1, nchar(temp[, 2]) - 2))
  temp[pmam == "pm", 2] <- temp[pmam == "pm", 2] + 12

  sex1 <- as.matrix(xtabs(sex1 ~ district + hour, temp)) # Number of men per district (row) and hour (column)
  sex2 <- as.matrix(xtabs(sex2 ~ district + hour, temp)) # Number of women per district (row) and hour (column)

  # Age
  temp <- age
  temp[, 2] <- as.character(temp[, 2]) # Modify pm/am format to 1-24
  pmam <- substr(temp[, 2], nchar(temp[, 2]) - 1, nchar(temp[, 2]))
  temp[, 2] <- as.numeric(substr(temp[, 2], 1, nchar(temp[, 2]) - 2))
  temp[pmam == "pm", 2] <- temp[pmam == "pm", 2] + 12

  age1 <- as.matrix(xtabs(age1 ~ district + hour, temp)) # Number of 16-24 years per district (row) and hour (column)
  age2 <- as.matrix(xtabs(age2 ~ district + hour, temp)) # Number of 25-34 years per district (row) and hour (column)
  age3 <- as.matrix(xtabs(age3 ~ district + hour, temp)) # Number of 35-64 years per district (row) and hour (column)
  age4 <- as.matrix(xtabs(age4 ~ district + hour, temp)) # Number of 65 and more per district (row) and hour (column)

  # Education
  temp <- edu
  temp[, 2] <- as.character(temp[, 2]) # Modify pm/am format to 1-24
  pmam <- substr(temp[, 2], nchar(temp[, 2]) - 1, nchar(temp[, 2]))
  temp[, 2] <- as.numeric(substr(temp[, 2], 1, nchar(temp[, 2]) - 2))
  temp[pmam == "pm", 2] <- temp[pmam == "pm", 2] + 12

  edu1 <- as.matrix(xtabs(cleduc1 ~ district + hour, temp)) # Number of low educ. per district (row) and hour (column)
  edu2 <- as.matrix(xtabs(cleduc2 ~ district + hour, temp)) # Number of middle-low educ. per district (row) and hour (column)
  edu3 <- as.matrix(xtabs(cleduc3 ~ district + hour, temp)) # Number of middle-high educ. years per district (row) and hour (column)
  edu4 <- as.matrix(xtabs(cleduc4 ~ district + hour, temp)) # Number of high educ. per district (row) and hour (column)

  # Merge the 10 tables
  tabi <- rbind(
    data.frame(City = cit[i], Socat = "sex1", District = as.numeric(rownames(sex1)), sex1),
    data.frame(City = cit[i], Socat = "sex2", District = as.numeric(rownames(sex2)), sex2),
    data.frame(City = cit[i], Socat = "age1", District = as.numeric(rownames(age1)), age1),
    data.frame(City = cit[i], Socat = "age2", District = as.numeric(rownames(age2)), age2),
    data.frame(City = cit[i], Socat = "age3", District = as.numeric(rownames(age3)), age3),
    data.frame(City = cit[i], Socat = "age4", District = as.numeric(rownames(age4)), age4),
    data.frame(City = cit[i], Socat = "edu1", District = as.numeric(rownames(edu1)), edu1),
    data.frame(City = cit[i], Socat = "edu2", District = as.numeric(rownames(edu2)), edu2),
    data.frame(City = cit[i], Socat = "edu3", District = as.numeric(rownames(edu3)), edu3),
    data.frame(City = cit[i], Socat = "edu4", District = as.numeric(rownames(edu4)), edu4)
  )
  tabi <- tabi[order(tabi[, 1], tabi[, 2], tabi[, 3]), ]

  tab <- rbind(tab, tabi)
}

# Remove district with 0 individual
tab <- tab[apply(tab[, -c(1, 2, 3, 4)], 1, sum) > 0, ] # We remove here 11 sociodistricts (2 low educ. + 9 high educ.) from 11 distinct districts

# Add the social group 'All' based on the total number of individuals
tabtot <- tab[substr(tab[, 2], 1, 3) == "sex", -2] # We use the sex category but we could have used the age category (but not educ, the four categories do not cover the whole population)
tabtot <- aggregate(tabtot[, -c(1, 2)], list(tabtot[, 1], tabtot[, 2]), sum)
tabtot <- data.frame(City = tabtot[, 1], Socat = "tot", District = tabtot[, 2], tabtot[, -c(1, 2)])

tab <- rbind(tabtot, tab)
tab <- tab[order(tab[, 1], tab[, 3], tab[, 2]), ]

# Rename IDF -> PARIS, LA REUNION -> SAINT DENIS and MARTINIQUE -> FORT DE FRANCE
tab[, 1] <- as.character(tab[, 1])
tab[, 1][tab[, 1] == "IDF"] <- "PARIS"
tab[, 1][tab[, 1] == "LA REUNION"] <- "SAINT DENIS"
tab[, 1][tab[, 1] == "MARTINIQUE"] <- "FORT DE FRANCE"
tab <- tab[order(tab[, 1], tab[, 3], tab[, 2]), ]

# Add ID
tab <- cbind(1:length(tab[, 1]), tab)
colnames(tab)[1] <- "ID"

# Rename column
colnames(tab)[-c(1, 2, 3, 4)] <- paste0("H", 1:24)

# Export
if(dir.exists("Data/Outputs")){
  unlink("Data/Outputs", recursive = TRUE)
}
dir.create("Data/Outputs")
write.csv2(tab, paste("Data/Outputs/Signals.csv", sep = ""), row.names = FALSE)


