####################################################
##                  Data & Packages               ##
####################################################
# Choose the work directory = folder
setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input")

# Load Package
library(foreign) # pour ouvrir les ".dbf"
library(plyr) # pour la fonction "ddply"
library(reshape2) # pour "tableau croisé dynamique"

# Import data
data1 <- read.dbf("TIGES3.DBF")
# data1[,"ID_PET_MES"] <- as.character(data1[,"ID_PET_MES"])
data2 <- read.dbf("TIGES4.DBF")
# data2[,"ID_PET_MES"] <- as.character(data2[,"ID_PET_MES"])

####################################################
##                 %G/ha/sp/site                  ##
####################################################

# G/ha/sp/site
sitesp1 <- ddply(data1, .(ID_PET_MES, ESSENCE), summarise, BA=sum(ST_M2HA))
sitesp2 <- ddply(data2, .(ID_PET_MES, ESSENCE), summarise, BA=sum(ST_M2HA))

# G/ha/site
Gsite1 <- ddply(sitesp1, .(ID_PET_MES), summarise, G=sum(BA))
Gsite2 <- ddply(sitesp2, .(ID_PET_MES), summarise, G=sum(BA))

# G/ha/sp/site
propsp1 <- merge(sitesp1, Gsite1, by="ID_PET_MES", all=T)
propsp1$Prop <- propsp1$BA*100/propsp1$G
propsp2 <- merge(sitesp2, Gsite2, by="ID_PET_MES", all=T)
propsp2$Prop <- propsp2$BA*100/propsp2$G

# summary table
tab1 <- dcast(propsp1, ID_PET_MES ~ ESSENCE, value.var= "Prop")
tab1[is.na(tab1)] <- 0
tab2 <- dcast(propsp2, ID_PET_MES ~ ESSENCE, value.var= "Prop")
tab2[is.na(tab2)] <- 0

####################################################
##               mixed stands selection           ##
####################################################
# sp
sp1 <- "SAB"
sp2 <- "PET"
# prop sp
prop1 <- 0   #25
prop2 <- 0   #25
prop3 <- 90   #75
# site selection
sites1 <- tab1[tab1[,sp1]>prop1 & tab1[,sp2]>prop2 & tab1[,sp1]+tab1[,sp2]>=prop3,]
sites2 <- tab2[tab2[,sp1]>prop1 & tab2[,sp2]>prop2 & tab2[,sp1]+tab2[,sp2]>=prop3,]

####################################################
# sp propotions and total BA
####################################################

sites1 <- sites1[,c("ID_PET_MES", "SAB", "PET")]
sites2 <- sites2[,c("ID_PET_MES", "SAB", "PET")]
mixedsites <- rbind(sites1, sites2)
colnames(mixedsites) <- c("ID_PET_MES", "prop_SAB_BA", "prop_PET_BA")
dim(mixedsites)

proportion_G <- c(80,85,90,95,100)
nb_sites <- c(3572, 2328, 1319, 568, 112)
plot(proportion_G, nb_sites, pch=16, col="blue")

# add the total BA
Gsites <- rbind(Gsite1, Gsite2)
mixedsites <- merge(mixedsites, Gsites)
colnames(mixedsites)[colnames(mixedsites)=="G"] <- "BAtot"

# units
mixedsites$prop_SAB_BA <- mixedsites$prop_SAB_BA/100
mixedsites$prop_PET_BA <- mixedsites$prop_PET_BA/100
mixedsites$BAtot <- mixedsites$BAtot*10000
colnames(mixedsites)[colnames(mixedsites)=="BAtot"] <- "BAtot_CM2HA"

####################################################
##                   save data                    ##
####################################################

save(mixedsites, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mixedsites.rdata")


####################################################
## data for competition
####################################################

# add tree sp, nb and DBHb
compet <- rbind(data1[, c("ID_PET_MES", "ESSENCE", "CL_DHP", "ST_M2HA")], data2[, c("ID_PET_MES", "ESSENCE", "CL_DHP", "ST_M2HA")])

compet <- compet[compet$ID_PET_MES %in% mixedsites$ID_PET_MES,]

# BA des voisins à l'année de mesure
compet$CL_DHP <- as.numeric(as.character(compet$CL_DHP))
compet$BAj_CM2 <- pi * ((compet$CL_DHP)^2)/4
compet$ST_M2HA <-  compet$ST_M2HA * 10000
colnames(compet)[colnames(compet)=="ST_M2HA"] <- "ST_CM2HA"


####################################################
##                   save data                    ##
####################################################

save(compet, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/compet.rdata")
