# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(plyr) # pour la fonction "ddply"
library(reshape2) # pour la fonction melt
library(ggplot2)

# liste_sites_clim_futur
sites <- read.table("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input/futur_climate/liste_sites_clim_futur.csv", sep=",", header=T)
range(nchar(sites$ID_PET_MES))
sites$ID_PET_MES <- formatC(sites$ID_PET_MES, width = 12, format = "fg", flag = "0")
range(nchar(sites$ID_PET_MES))

# growth data + present clim,
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
#DHP_CM = dbhmax
DHP_CM <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, DHP_CM=max(DHP_CM))
prop_SAB_BA <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, prop_SAB_BA=unique(prop_SAB_BA))
prop_PET_BA <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, prop_PET_BA=unique(prop_PET_BA))
BAtot_CM2HA <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, BAtot_CM2HA=unique(BAtot_CM2HA))
drainage <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, drainage=unique(drainage))
texture <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, texture=unique(texture))
ESSENCE <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, ESSENCE=unique(ESSENCE))
compethard <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, compethard=unique(compethard))
competsoft <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, competsoft=unique(competsoft))
data1 <- cbind(DHP_CM, prop_SAB_BA$prop_SAB_BA, prop_PET_BA$prop_PET_BA, BAtot_CM2HA$BAtot_CM2HA, drainage$drainage, texture$texture, ESSENCE$ESSENCE, compethard$compethard, competsoft$competsoft)
colnames(data1) <- c("ID_PET_MES", "ID_ARB", "DHP_CM", "prop_SAB_BA", "prop_PET_BA", "BAtot_CM2HA", "drainage", "texture", "ESSENCE", "compethard", "competsoft")

####################################################
## function formatage
####################################################

# DC max jun, jul, aug
funDC <- function(){
  # correspondance ID_PET_MES / id
  colnames(DC) <- c("yr", "month", "day", sites$ID_PET_MES)
  DC <- melt(DC, id.vars=c("yr","month","day"))
  colnames(DC) <- c("yr", "month", "day", "ID_PET_MES", "DC")
  # DC max month
  month <- c(6,7,8) # jun, jul, aug
  DCMAXjun_jul_aug <- ddply(DC[DC$month %in% month,], .(ID_PET_MES, yr), summarise, DCMAXjun_jul_aug=max(DC))
  # # DC max jun, jul, aug
  # DCMAXjun_jul_aug <- ddply(DCmax_month, .(ID_PET_MES, yr), summarise, DCMAXjun_jul_aug=max(DCmax_month))
  DCMAXjun_jul_aug$plotyr <- paste(DCMAXjun_jul_aug$ID_PET_MES, DCMAXjun_jul_aug$yr, sep="")
  DCMAXjun_jul_aug <- DCMAXjun_jul_aug[,c("plotyr", "ID_PET_MES", "DCMAXjun_jul_aug", "yr")]
  return(DCMAXjun_jul_aug)
}

# Tmean annual
funT <- function(){
  # correspondance ID_PET_MES / id
  colnames(Tannual) <- c("yr", sites$ID_PET_MES)
  Tannual <- melt(Tannual, id.vars="yr")
  colnames(Tannual) <- c("yr","ID_PET_MES", "Tannual")
  Tannual$plotyr <- paste(Tannual$ID_PET_MES, Tannual$yr, sep="")
  Tannual <- Tannual[,c("plotyr", "Tannual")]
  return(Tannual)
}

# Pmean annual
funP <- function(){
  # correspondance ID_PET_MES / id
  colnames(Pannual) <- c("yr", sites$ID_PET_MES)
  Pannual <- melt(Pannual, id.vars="yr")
  colnames(Pannual) <- c("yr","ID_PET_MES", "Pannual")
  Pannual$plotyr <- paste(Pannual$ID_PET_MES, Pannual$yr, sep="")
  Pannual <- Pannual[,c("plotyr", "Pannual")]
  return(Pannual)
}

# DC t-1
funDCp <- function(data = data){
  DCp <- ddply(data, .(ID_PET_MES, yr), summarise, DCp = unique(DCMAXjun_jul_aug))
  range(DCp$yr)
  DCp$yr <- DCp$yr + 1
  DCp <- DCp[DCp$yr != 2101, ]
  DCp$plotyr <- paste(DCp$ID_PET_MES, DCp$yr, sep = "")
  data$plotyr <- paste(data$ID_PET_MES, data$yr, sep = "")
  data <- merge(data, DCp[, 3:4], by = "plotyr")
  return(data)
}

dim(data)
length(unique(data$ID_PET_MES))
length(unique(data$ID_ARB))

####################################################
## Format climatic data for all models
####################################################
setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input/futur_climate/all_climate_data")
fileNames <- Sys.glob("*.csv")
fileNames

# select the scenario and model
for (i in unique(substr(fileNames, 1, 11))){
  # DC
  DC <- read.table(paste(i, "_DC.csv", sep = ""), sep=",", header=T)
  DC <- funDC()
  colnames(DC) <- c("plotyr", "ID_PET_MES", "DCMAXjun_jul_aug", "yr")

  # Tannual
  Tannual <- read.table(paste(i, "_Ta.csv", sep = ""), sep = ",", header = T)
  Tannual <- funT()
  colnames(Tannual) <- c("plotyr", "Tannual")

  # Pannual
  Pannual <- read.table(paste(i, "_Pa.csv", sep = ""), sep = ",", header = T)
  Pannual <- funP()
  colnames(Pannual) <- c("plotyr", "Pannual")

  # Gather
  mod <- merge(Tannual, Pannual, by = "plotyr")
  mod <- merge(mod, DC, by = "plotyr")
  mod <- mod[, -1]

  data <- merge(data1, mod, by = "ID_PET_MES")
  data <- funDCp(data)
  save(data, file = paste(i, ".rdata", sep = ""))
}
