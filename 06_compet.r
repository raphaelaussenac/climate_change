# Delete all objects in the work space
rm(list=ls(all=TRUE))
options(digits=16)

####################################################
##                  Data & Packages               ##
####################################################
library(plyr) # pour la fonction "ddply"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/dataBAI.rdata")

load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/compet.rdata")


####################################################
## Competition
####################################################

# BA of the focal tree
BA_focal_CM <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, BA_focal_CM=unique(BA_focal_CM))

comp <- merge(BA_focal_CM, compet[, c("ID_PET_MES", "ESSENCE", "BAj_CM2", "ST_CM2HA")], by="ID_PET_MES")

# Difference in trees BA
comp$diff <- comp$BA_focal_CM - comp$BAj_CM2
comp <- comp[comp$diff<=0,]

# Competition hardwood/softwood
hardwood <- c("PET", "BOP", "SAL", "BOJ", "ERR", "PEG", "PRP", "CAF", "AUR", "CET", "OSV", "ORA", "PEB", "ERS", "FRN", "BOG", "ERP", "AME", "SOA", "ERE")

softwood <- c("SAB", "EPB", "EPN", "MEL", "EPR", "THO", "PIR", "PIB", "PIG", "PRU")


comphard <- comp[comp$ESSENCE %in% hardwood,]
compsoft <- comp[comp$ESSENCE %in% softwood,]

compethard<- ddply(comphard, .(ID_PET_MES, ID_ARB), summarise, compethard=sum(ST_CM2HA))
competsoft<- ddply(compsoft, .(ID_PET_MES, ID_ARB), summarise, competsoft=sum(ST_CM2HA))

compet <- merge(BA_focal_CM, compethard[,2:3], by="ID_ARB", all.x=T)
compet <- merge(compet, competsoft[,2:3], by="ID_ARB", all.x=T)

compet[is.na(compet$compethard), "compethard"] <- 0
compet[is.na(compet$competsoft), "competsoft"] <- 0

# compet<- ddply(comp, .(ID_PET_MES, ID_ARB), summarise, compet=sum(ST_CM2HA))
# compet <- merge(BA_focal_CM, compet[,2:3], by="ID_ARB", all.x=T)
# compet[is.na(compet[,4]), "compet"] <- 0

hist(compet$compethard)
hist(compet$competsoft)


####################################################
## save
####################################################

data <- merge(data, compet[, c("ID_ARB", "compethard", "competsoft")], by="ID_ARB")
save(data, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/dataBAIcompet.rdata")


####################################################
## liste des sites avant filtre
####################################################

sites <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, SP=unique(ESSENCE))
save(sites, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/sitescores.rdata")
