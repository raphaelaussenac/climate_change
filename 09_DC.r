# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(plyr) # pour la fonction "ddply"

# Choose the work directory = folder
setwd("/Users/raphaelaussenac/Documents/GitHub/climate_change/data")

# Drought code (row data)
dc <- read.csv("./DC.csv", sep=" ")
range(nchar(dc$ID_PET_MES))
dc$ID_PET_MES <- formatC(dc$ID_PET_MES, width = 12, format = "fg", flag = "0")
range(nchar(dc$ID_PET_MES))

# growth data
load(file="./dataBAIcompetfilt.rdata")


####################################################
## DC max for specific periods
####################################################
# Max DC per month
DC_MAX_month <- ddply(dc, .(ID_PET_MES,yr,mon), summarise, DC_MAX_month=max(dc))

# MAX DC for specific months
DCMAXapr <- DC_MAX_month[DC_MAX_month$mon==4,]
DCMAXmay <- DC_MAX_month[DC_MAX_month$mon==5,]
DCMAXjun <- DC_MAX_month[DC_MAX_month$mon==6,]
DCMAXjul <- DC_MAX_month[DC_MAX_month$mon==7,]
DCMAXaug <- DC_MAX_month[DC_MAX_month$mon==8,]

# MAX DC for specific periods
# April-May-June
apr_may_jun <- rbind(DCMAXapr, DCMAXmay, DCMAXjun)
period1 <- ddply(apr_may_jun, .(ID_PET_MES,yr), summarise, DCMAXapr_may_jun=max(DC_MAX_month))
# May-June-July
may_jun_jul <- rbind(DCMAXmay, DCMAXjun, DCMAXjul)
period2 <- ddply(may_jun_jul, .(ID_PET_MES,yr), summarise, DCMAXmay_jun_jul=max(DC_MAX_month))
# June-July-August
jun_jul_aug <- rbind(DCMAXjun, DCMAXjul, DCMAXaug)
period3 <- ddply(jun_jul_aug, .(ID_PET_MES,yr), summarise, DCMAXjun_jul_aug=max(DC_MAX_month))

####################################################
## merge with data
####################################################

period1$plotyr <- paste(period1$ID_PET_MES, period1$yr, sep="")
period1 <- period1[,3:4]

period2$plotyr <- paste(period2$ID_PET_MES, period2$yr, sep="")
period2 <- period2[,3:4]

period3$plotyr <- paste(period3$ID_PET_MES, period3$yr, sep="")
period3 <- period3[,3:4]

data <- merge(data, period1, by="plotyr")
data <- merge(data, period2, by="plotyr")
data <- merge(data, period3, by="plotyr")

####################################################
## DC t-1
####################################################

DCp <- ddply(data, .(ID_PET_MES, AN_CERNE), summarise, DCp = unique(DCMAXjun_jul_aug))
range(DCp$AN_CERNE)
DCp$AN_CERNE <- DCp$AN_CERNE + 1
DCp <- DCp[DCp$AN_CERNE != 2005, ]
DCp$plotyr <- paste(DCp$ID_PET_MES, DCp$AN_CERNE, sep = "")

data <- merge(data, DCp[, 3:4], by = "plotyr")

dim(data)
length(unique(data$ID_PET_MES))
length(unique(data$ID_ARB))


####################################################
## save
####################################################

save(data, file="./dataBAI.rdata")
