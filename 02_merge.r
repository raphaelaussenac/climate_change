# Delete all objects in the work space
rm(list=ls())


####################################################
##                  Data & Packages               ##
####################################################
# Choose the work directory = folder
setwd("/Users/raphaelaussenac/Documents/GitHub/climate_change/data")

# open placette info files
geolocali <- read.table("T_Localis.txt", header=T, sep=",", colClasses="character")
classeco <- read.table("T_ClassEcol.txt", header=T, sep=",", colClasses="character")
stratecarto <- read.table("T_StrateCarto.txt", header=T, sep=",", colClasses="character")
peupobserve <- read.table("T_PeupObserve.txt", header=T, sep=",", colClasses="character")

# merge geo files
geo_a <-  merge(geolocali, classeco, all.x=T)
rm(geolocali)
rm(classeco)
geo_b <- merge(geo_a,stratecarto, all.x=T)
rm(stratecarto)
geo <- merge(geo_b,peupobserve, all.x=T)
rm(peupobserve)
rm(geo_a)
rm(geo_b)
geo$ID_PET <- as.factor(geo$ID_PET)
geo$LATITUDE <- as.numeric(geo$LATITUDE)
geo$LONGITUDE <- as.numeric(geo$LONGITUDE)
geo$UPAYS_REG <- as.factor(geo$UPAYS_REG)
geo$SREG_ECO <- as.factor(geo$SREG_ECO)
geo$DOM_BIO <- as.factor(geo$DOM_BIO)
geo$SDOM_BIO <- as.factor(geo$SDOM_BIO)
geo$REG_ECO <- as.factor(geo$REG_ECO)
geo$TYPE_ECO <- as.factor(geo$TYPE_ECO)

#open tree info file
tree <- read.table("T_ArbrePET.txt", header=T, sep=",", colClasses="character")
tree$ID_PET <- as.factor(tree$ID_PET)
tree$ID_ARB <- as.factor(tree$ID_ARB)
tree$ESSENCE <- as.factor(tree$ESSENCE)
tree$NO_SEQ <- as.factor(tree$NO_SEQ)
tree$DOM_BIO <- as.factor(tree$DOM_BIO)
tree$SDOM_BIO <- as.factor(tree$SDOM_BIO)
tree$REG_ECO <- as.factor(tree$REG_ECO)

# merge tree and site files
data <- merge(geo, tree, all=T)

#create depot/drainage variable from type eco (combines both)
data$DEP_DR <- as.factor(substr(data$TYP_ECO, 4, 4))

# save data
save(data, file = "./merge_Loic.rdata")
