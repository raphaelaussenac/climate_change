# Delete all objects in the work space
rm(list=ls(all=TRUE))
options(digits=22)

####################################################
##                  Data & Packages               ##
####################################################
library(plyr) # pour la fonction "ddply"

# Choose the work directory = folder
setwd("/Users/raphaelaussenac/Documents/GitHub/climate_change/data")

# Import selected plots (file "mixedsites")
load("./mixedsites.rdata")

# Import tree ring data
cores <- read.table("./T_CernesSABPET.txt", sep=";", header=T)
cores <- cores[, c("ID_PET_MES", "ID_ARB", "AN_CERNE", "LARG_CERNE", "ESSENCE", "DHP_MM")]


# format ID_PET_MES
cores$ID_PET_MES <- formatC(cores$ID_PET_MES, width = 12, format = "fg", flag = "0")
range(nchar(cores$ID_PET_MES))
str(cores)

# format ID_ARB
cores$ID_ARB <- formatC(cores$ID_ARB, width = 14, format = "fg", flag = "0")
range(nchar(cores$ID_ARB))
str(cores)

# Select only the cores in the selected sites
cores <- cores[cores$ID_PET_MES %in% unique(mixedsites$ID_PET_MES),]

# Import coordinates
load("./merge_Loic.rdata")
data <- data[,c("ID_PET_MES","LATITUDE","LONGITUDE","DEP_DR")]
data <- data[data$ID_PET_MES %in% unique(mixedsites$ID_PET_MES),]

# delete sites without information on soil
data <- data[data$DEP_DR!="",]

# data$ID_PET_MES <- as.numeric(data$ID_PET_MES)
LONG <- ddply(data, .(ID_PET_MES), summarise, LONG=unique(LONGITUDE))
LAT <- ddply(data, .(ID_PET_MES), summarise, LAT=unique(LATITUDE))
DD <- ddply(data, .(ID_PET_MES), summarise, DD=unique(DEP_DR))
data <- merge(LAT,LONG,by="ID_PET_MES")
data <- merge(data,DD,by="ID_PET_MES")

# Merge data
data2 <- merge(cores, data, by="ID_PET_MES", all.x=T)
data2 <- merge(data2, mixedsites, by="ID_PET_MES")
data <- data2

data <- data[!is.na(data$LONG), ]
data <- data[!is.na(data$LAT), ]
data <- data[!is.na(data$DD), ]

####################################################
##                    Save Data                   ##
####################################################

# identify sites as pure or mixed
nbsp <- ddply(data, .(ID_PET_MES), summarise, nbsp=length(unique(ESSENCE)))
data <- merge(data,nbsp, by="ID_PET_MES")
pure <- data[data$nbsp==1,]
pure$Compo <- pure$ESSENCE
mixed <- data[data$nbsp==2,]
mixed$Compo <- "MIXED"
data <- rbind(pure,mixed)
save(data, file="./data.rdata")


# sites coordinates for climate variables extraction
LAT <- ddply(data, .(ID_PET_MES), summarise, LONG=unique(LONG))
LONG <- ddply(data, .(ID_PET_MES), summarise, LAT=unique(LAT))
compo <- ddply(data, .(ID_PET_MES), summarise, Compo=unique(Compo))
DD <- ddply(data, .(ID_PET_MES), summarise, DD=unique(DD))
prop_SAB_BA <- ddply(data, .(ID_PET_MES), summarise, prop_SAB_BA=unique(prop_SAB_BA))
prop_PET_BA <- ddply(data, .(ID_PET_MES), summarise, prop_PET_BA=unique(prop_PET_BA))
coord <- merge(LAT,LONG,by="ID_PET_MES")
coord <- merge(coord,compo,by="ID_PET_MES")
coord <- merge(coord,DD,by="ID_PET_MES")
coord <- merge(coord,prop_SAB_BA,by="ID_PET_MES")
coord <- merge(coord,prop_PET_BA,by="ID_PET_MES")
save(coord, file="./coord.rdata")


####################################################
##                    Count                  ##
####################################################

# nb cores
length(unique(data$ID_ARB))
# nb cores SAB
length(unique(data[data$ESSENCE=="SAB","ID_ARB"]))
# nb cores PET
length(unique(data[data$ESSENCE=="PET","ID_ARB"]))




# nb sites
dim(coord)
# nb sites SAB
dim(coord[coord$Compo=="SAB",])
# nb sites PET
dim(coord[coord$Compo=="PET",])
# nb sites mixed
dim(coord[coord$Compo=="MIXED",])
