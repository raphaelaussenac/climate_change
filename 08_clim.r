# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(plyr) # pour la fonction "ddply"

# clim data
clim <- read.table("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input/Q1985-2005.txt", sep=";", header=T)
# format ID_PET_MES
range(nchar(clim$ID))
clim$ID <- formatC(clim$ID, width = 12, format = "fg", flag = "0")
range(nchar(clim$ID))
clim$yr <- substr(clim$Date, 1, 4)

# growth data
load(file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAIcompetfilt.rdata")


####################################################
## Tmean & Pmean (annual)
####################################################

Tannual <- ddply(clim, .(ID, yr), summarise, Tannual=mean(TMean))
Tannual$plotyr <- paste(Tannual$ID, Tannual$yr, sep="")
Tannual_save <- Tannual
Tannual <- Tannual[, 3:4]
Pannual <- ddply(clim, .(ID, yr), summarise, Pannual=sum(TotalPrecip))
Pannual$plotyr <- paste(Pannual$ID, Pannual$yr, sep="")
Pannual_save <- Pannual
Pannual <- Pannual[, 3:4]

# merge clim data with growth data
data$plotyr <- paste(data$ID_PET_MES, data$AN_CERNE, sep="")
data <- merge(data, Tannual, by = "plotyr")
data <- merge(data, Pannual, by = "plotyr")

####################################################
## Tmean & Pmean (period)
####################################################

Tperiod <- ddply(Tannual_save, .(ID), summarise, Tperiod=mean(Tannual))
Pperiod <- ddply(Pannual_save, .(ID), summarise, Pperiod=mean(Pannual))

# merge clim data with growth data
data <- merge(data, Tperiod, by.x = "ID_PET_MES", by.y = "ID")
data <- merge(data, Pperiod, by.x = "ID_PET_MES", by.y = "ID")

####################################################
## save
####################################################

save(data, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAIcompetfilt.rdata")
