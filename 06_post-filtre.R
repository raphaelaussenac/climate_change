# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
##                  Data & Packages               ##
####################################################
library(plyr) # pour la fonction "ddply"

# Choose the work directory = folder
setwd("/Users/raphaelaussenac/Documents/GitHub/climate_change/data")

# complete data
load("./dataBAIcompet.rdata")
range(nchar(data$ID_PET_MES))
range(nchar(data$ID_ARB))

# filtered cores
cores <- read.csv("./UPAYS_filtre.csv", sep=";")
range(nchar(cores$ID_PET_MES))
range(nchar(cores$ID_ARB))
# format ID_PET_MES
cores$ID_PET_MES <- formatC(cores$ID_PET_MES, width = 12, format = "fg", flag = "0")
range(nchar(cores$ID_PET_MES))
# format ID_ARB
cores$ID_ARB <- formatC(cores$ID_ARB, width = 14, format = "fg", flag = "0")
range(nchar(cores$ID_ARB))


length(unique(cores$ID_PET_MES))
length(unique(cores[cores$ESSENCE=="SAB", "ID_PET_MES"]))
length(unique(cores[cores$ESSENCE=="PET", "ID_PET_MES"]))


length(unique(cores$ID_ARB))
length(unique(cores[cores$ESSENCE=="SAB", "ID_ARB"]))
length(unique(cores[cores$ESSENCE=="PET", "ID_ARB"]))

####################################################
## filter complete data
####################################################

data <- data[data$ID_ARB %in% cores$ID_ARB,]

length(unique(data$ID_ARB))
length(unique(data[data$ESSENCE=="SAB", "ID_ARB"]))
length(unique(data[data$ESSENCE=="PET", "ID_ARB"]))

####################################################
## save
####################################################

save(data, file="./dataBAIcompetfilt.rdata")


####################################################
## liste des sites après filtre
####################################################

sites <- data.frame(unique(data$ID_PET_MES))
colnames(sites) <- "ID_PET_MES"
sites$ID_PET_MES <- as.character(sites$ID_PET_MES)
range(nchar(sites$ID_PET_MES))
write.csv(sites, file="./liste_sites.csv")




# sitesLAT <- ddply(data, .(ID_PET_MES), summarise, LAT=unique(LAT))
# sitesLONG <- ddply(data, .(ID_PET_MES), summarise, LONG=unique(LONG))
# sites <- merge(sitesLAT, sitesLONG, by="ID_PET_MES")
# write.csv(sites, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/liste_sites.csv")
