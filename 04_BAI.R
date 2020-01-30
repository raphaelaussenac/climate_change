# Delete all objects in the work space
rm(list=ls(all=TRUE))
options(digits=16)

####################################################
##                  Data & Packages               ##
####################################################
library(ggplot2)
library(plyr) # pour la fonction "ddply"

# Choose the work directory = folder
setwd("/Users/raphaelaussenac/Documents/GitHub/climate_change/data")

# site
load("./data.rdata")

####################################################
## Data verification / transformation
####################################################


# data availability
ggplot(data, aes(AN_CERNE))+
geom_histogram(binwidth=1, fill="white", color="black")+
geom_vline(xintercept=1985, color="red", linetype = "longdash")+
geom_vline(xintercept=2005, color="red", linetype = "longdash")

# tree ring width
ggplot(data, aes(LARG_CERNE))+
geom_histogram(binwidth=0.1)+
geom_vline(xintercept=10, color="red", linetype = "longdash")

# delete individuals having suspicious growth (>10)
MAX <- ddply(data, .(ID_ARB), summarise, MAX=max(LARG_CERNE))
MAX <- MAX[MAX$MAX<10,]
data <- data[data$ID_ARB %in% MAX$ID_ARB,]

# tree ring width
ggplot(data, aes(LARG_CERNE))+
geom_histogram(binwidth=0.1)

####################################################
## BA focal tree
####################################################
data$DHP_MM <- data$DHP_MM /10
colnames(data)[colnames(data)=="DHP_MM"] <- "DHP_CM"
data$BA_focal_CM <- pi * ((data$DHP_CM)^2)/4

####################################################
## DBH
####################################################
data$LARG_CERNE <- data$LARG_CERNE /10
colnames(data)[colnames(data)=="LARG_CERNE"] <- "LARGE_CERNE_CM"

data <- data[order(-data$AN_CERNE),]
# i <- 1000500503137
for (i in unique(data$ID_ARB)){
  ind <- data[data$ID_ARB==i,]
  for (j in 2:nrow(ind)){
    ind[j, "DHP_CM"] <- ind[j-1, "DHP_CM"] - (2 * ind[j-1, "LARGE_CERNE_CM"])
  }
  data[data$ID_ARB==i,"DHP_CM"] <- ind$DHP_CM
}

# data availability
ggplot(data, aes(DHP_CM, fill=ESSENCE))+
geom_histogram(bins=500)+
geom_vline(xintercept=0, color="red", linetype = "longdash")

####################################################
## set TW 1985-2005
####################################################
data <- data[data$AN_CERNE>=1984 & data$AN_CERNE<=2005,] # 1984 because otherwise we will loose 1985 since it is the first year

# data availability
ggplot(data, aes(DHP_CM, fill=ESSENCE))+
geom_histogram(bins=500)+
geom_vline(xintercept=0, color="red", linetype = "longdash")

####################################################
## delete negative DBH
####################################################
data <- data[data$DHP_CM>0,]

# data availability
ggplot(data, aes(DHP_CM, fill=ESSENCE))+
geom_histogram(bins=500)+
geom_vline(xintercept=0, color="red", linetype = "longdash")

####################################################
# Ring as a function of DBH
####################################################

ggplot(data, aes(DHP_CM, LARGE_CERNE_CM, color=ESSENCE))+
geom_point()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")
#
ggplot(data[data$ESSENCE=="SAB",], aes(DHP_CM, LARGE_CERNE_CM))+
geom_point()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")
#
ggplot(data[data$ESSENCE=="PET",], aes(DHP_CM, LARGE_CERNE_CM))+
geom_point()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")


####################################################
# BA
####################################################

# BA
data$BAt <- pi*((data$DHP_CM^2)/4)

# data availability
ggplot(data, aes(BAt, fill=ESSENCE))+
geom_histogram(bins=500)+
geom_vline(xintercept=0, color="red", linetype = "longdash")


####################################################
# BAI
####################################################

data <- data[order(-data$AN_CERNE),]
data$BAI_CM <- NA
# i <- 1000500503137
for (i in unique(data$ID_ARB)){
  ind <- data[data$ID_ARB==i,]
  for (j in 1:(nrow(ind)-1)){
    ind[j, "BAI"] <- ind[j, "BAt"] - ind[j+1, "BAt"]
  }
  data[data$ID_ARB==i,"BAI_CM"] <- ind$BAI
}

# data availability
ggplot(data, aes(BAI_CM, fill=ESSENCE))+
geom_histogram(bins=500)+
geom_vline(xintercept=0, color="red", linetype = "longdash")


####################################################
## Prepare data for model
####################################################

# supprimer quand BAI = NA
data <- data[!is.na(data$BAI),]

# log (BAI + 1)
data$lBAI_CM <- log(data$BAI_CM + 1)

# interaction proportion et maturité (G)
data$inter_prop_BAtot_SAB <- data$prop_SAB_BA * data$BAtot_CM2HA
data$inter_prop_BAtot_PET <- data$prop_PET_BA * data$BAtot_CM2HA

ggplot(data, aes(DHP_CM, BAI_CM, color=ID_ARB))+
geom_line()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")+
theme(legend.position="none")


## only complete chronologies on the period
datemin <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, min = min(AN_CERNE))
hist(datemin$min)
dim(datemin[datemin$min > 1985, ])
listmin <- datemin[datemin$min == 1985, "ID_ARB"]

datemax <- ddply(data, .(ID_PET_MES, ID_ARB), summarise, max = max(AN_CERNE))
hist(datemax$max)
dim(datemax[datemax$max < 2005, ])
listmax <- datemax[datemax$max == 2005, "ID_ARB"]

listok <- c(listmin, listmax)
listok <- listok[duplicated(listok)]

data <- data[data$ID_ARB %in% listok, ]

save(data, file = "./dataBAI.rdata")










####################################################
# BAI as a function of DBH
####################################################

# !!! There is one NA for each individual
# because we didn't have the previous BA measure to calculate
# the older BAI
options(digits=4)
library(gridExtra)
scatter <- ggplot(data)+
geom_point(aes(DHP_CM, BAI_CM, color=ESSENCE), size=0.5)+
geom_vline(xintercept=0, color="orange", linetype = "longdash")+
theme(legend.position="none")+
xlim(0,0.8)

hist_top <- ggplot(data)+
geom_histogram(aes(DHP_CM, fill=ESSENCE), binwidth=0.01, alpha=0.5)+
theme(legend.position="none")+
xlim(0,0.8)

hist_right <- ggplot(data)+
coord_flip()+
geom_histogram(aes(BAI_CM, fill=ESSENCE), binwidth=0.0001, alpha=0.5)+
theme(legend.position="none")


empty <- ggplot()+geom_point(aes(1,1), colour="white")+
         theme(axis.ticks=element_blank(),
               panel.background=element_blank(),
               axis.text.x=element_blank(), axis.text.y=element_blank(),
               axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))



#
ggplot(data[data$ESSENCE=="SAB",], aes(DHP_CM, BAI_CM))+
geom_point()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")
#
ggplot(data[data$ESSENCE=="PET",], aes(DHP_CM, BAI_CM))+
geom_point()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")



ggplot(data[data$ESSENCE=="SAB",], aes(DHP_CM, BAI_CM, color=ID_ARB))+
geom_line()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")+
theme(legend.position="none")

ggplot(data[data$ESSENCE=="PET",], aes(DHP_CM, BAI_CM, color=ID_ARB))+
geom_line()+
geom_vline(xintercept=0, color="orange", linetype = "longdash")+
theme(legend.position="none")







####### lab
library(lme4)

dataSAB <- data[data$ESSENCE=="SAB",]
dataSAB$DHP_CM <- scale(dataSAB$DHP_CM)
dataSAB$BAI_CM <- scale(dataSAB$BAI_CM)

dataPET <- data[data$ESSENCE=="PET",]
dataPET$DHP_CM <- scale(dataPET$DHP_CM)
dataPET$BAI_CM <- scale(dataPET$BAI_CM)


# model formula
formSAB <- BAI_CM ~ DHP_CM + (DHP_CM| ID_ARB)
formPET <- BAI_CM ~ DHP_CM + (DHP_CM| ID_ARB)

modSABtot <-  lmer(formSAB, data=dataSAB, REML = FALSE)
modSAB1 <-  lmer(formSAB, data=dataSAB[dataSAB$DHP_CM<(-1),], REML = FALSE)
modSAB2 <-  lmer(formSAB, data=dataSAB[dataSAB$DHP_CM>=(-1) & dataSAB$DHP_CM<0,], REML = FALSE)
modSAB3 <-  lmer(formSAB, data=dataSAB[dataSAB$DHP_CM>=0 & dataSAB$DHP_CM<1,], REML = FALSE)
modSAB4 <-  lmer(formSAB, data=dataSAB[dataSAB$DHP_CM>=1 & dataSAB$DHP_CM<2,], REML = FALSE)
modSAB5 <-  lmer(formSAB, data=dataSAB[dataSAB$DHP_CM>=2,], REML = FALSE)

modPETtot <-  lmer(formPET, data=dataPET, REML = FALSE)
modPET1 <-  lmer(formPET, data=dataPET[dataPET$DHP_CM<(-1),], REML = FALSE)
modPET2 <-  lmer(formPET, data=dataPET[dataPET$DHP_CM>=(-1) & dataPET$DHP_CM<0.5,], REML = FALSE)
modPET3 <-  lmer(formPET, data=dataPET[dataPET$DHP_CM>=0.5 & dataPET$DHP_CM<1.5,], REML = FALSE)
modPET4 <-  lmer(formPET, data=dataPET[dataPET$DHP_CM>=1.5,], REML = FALSE)


dataSABPET <- rbind(dataSAB, dataPET)

ggplot(dataSABPET, aes(DHP_CM, BAI_CM, color=ID_ARB))+
geom_line(alpha=0.2)+
geom_vline(xintercept=0, color="orange", linetype = "longdash")+
theme(legend.position="none")+
stat_function(fun=function(x) summary(modSABtot)$coefficients[1] + summary(modSABtot)$coefficients[2]*x,size=2, colour="red", linetype=2)+
stat_function(fun=function(x) summary(modSAB1)$coefficients[1] + summary(modSAB1)$coefficients[2]*x, xlim=c(-2,-1),size=2, colour="red")+
stat_function(fun=function(x) summary(modSAB2)$coefficients[1] + summary(modSAB2)$coefficients[2]*x, xlim=c(-1,0),size=2, colour="red")+
stat_function(fun=function(x) summary(modSAB3)$coefficients[1] + summary(modSAB3)$coefficients[2]*x, xlim=c(0,1),size=2, colour="red")+
stat_function(fun=function(x) summary(modSAB4)$coefficients[1] + summary(modSAB4)$coefficients[2]*x, xlim=c(1,2),size=2, colour="red")+
stat_function(fun=function(x) summary(modSAB5)$coefficients[1] + summary(modSAB5)$coefficients[2]*x, xlim=c(2,4),size=2, colour="red")+

stat_function(fun=function(x) summary(modPETtot)$coefficients[1] + summary(modPETtot)$coefficients[2]*x,size=2, colour="black", linetype=2)+
stat_function(fun=function(x) summary(modPET1)$coefficients[1] + summary(modPET1)$coefficients[2]*x, xlim=c(-2,-1),size=2, colour="black")+
stat_function(fun=function(x) summary(modPET2)$coefficients[1] + summary(modPET2)$coefficients[2]*x, xlim=c(-1,0.5),size=2, colour="black")+
stat_function(fun=function(x) summary(modPET3)$coefficients[1] + summary(modPET3)$coefficients[2]*x, xlim=c(0.5,1.5),size=2, colour="black")+
stat_function(fun=function(x) summary(modPET4)$coefficients[1] + summary(modPET4)$coefficients[2]*x, xlim=c(1.5,5),size=2, colour="black")
