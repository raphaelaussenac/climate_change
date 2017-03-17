# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("plyr")
library("ggplot2")


####################################################
## data
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")

####################################################
## Aboveground Biomass (BA)
####################################################

BA1 <- ddply(data, .(ID_PET_MES, mix), summarise, BA = unique(BAtot_CM2HA))
BA2 <- ddply(data, .(ID_PET_MES), summarise, texture = unique(texture))
BA3 <- ddply(data, .(ID_PET_MES), summarise, drainage = unique(drainage))

BA <- merge(BA1, BA2, by = "ID_PET_MES")
BA <- merge(BA, BA3, by = "ID_PET_MES")
BA$mix <- factor(BA$mix, levels = c("PET", "SAB", "MIX"), ordered = FALSE)
BA$texture <- paste("texture", BA$texture, sep = " ")
BA$drainage <- paste("drainage", BA$drainage, sep = " ")

ggplot(BA, aes(mix, BA))+
geom_boxplot()+
ggtitle("Basal Area as a fucntion of mixture (entire QC)")

ggplot(BA, aes(mix, BA))+
geom_boxplot()+
facet_grid( ~ drainage, scales="free", space="free")+
ggtitle("Basal Area as a fucntion of mixture, soil drainage")

ggplot(BA, aes(mix, BA))+
geom_boxplot()+
facet_grid( ~ texture, scales="free", space="free")+
ggtitle("Basal Area as a fucntion of mixture, soil texture")


####################################################
## BAI
####################################################

# supprimer les PET en SAB et les SAB en PET
mix <- data[data$mix == "MIX",]
PET <- data[data$mix == "PET" & data$ESSENCE == "PET",]
SAB <- data[data$mix == "SAB" & data$ESSENCE == "SAB",]
data <- rbind(mix, PET, SAB)
data$texture <- paste("texture", data$texture, sep = " ")
data$drainage <- paste("drainage", data$drainage, sep = " ")


data$mix <- factor(data$mix, levels = c("PET", "SAB", "MIX"), ordered = FALSE)
ggplot(data, aes(mix, BAI_CM))+
geom_boxplot()+
facet_grid( ~ ESSENCE, scales="free", space="free")+
ggtitle("Basal Area Increment as a fucntion of mixture (entire QC)")


data$mix <- factor(data$mix, levels = c("SAB", "PET", "MIX"), ordered = FALSE)
ggplot(data, aes(mix, BAI_CM))+
geom_boxplot()+
facet_grid(drainage ~ ESSENCE, scales="free", space="free")+
ggtitle("Basal Area Increment as a fucntion of mixture, soil drainage")


data$mix <- factor(data$mix, levels = c("SAB", "PET", "MIX"), ordered = FALSE)
ggplot(data, aes(mix, BAI_CM))+
geom_boxplot()+
facet_grid(texture ~ ESSENCE, scales="free", space="free")+
ggtitle("Basal Area Increment as a fucntion of mixture, soil texture")


####################################################
## BAI
####################################################

data <- data[data$AN_CERNE == 2000, ]

ggplot(data, aes(BAI_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
theme_bw()+
ggtitle("BAI distribution as a fucntion of mixture (entire QC)")

ggplot(data, aes(BAI_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
facet_grid(~ drainage, scales="free", space="free")+
theme_bw()+
ggtitle("BAI distribution as a fucntion of mixture, drainage")

ggplot(data, aes(BAI_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
facet_grid(~ texture, scales="free", space="free")+
theme_bw()+
ggtitle("BAI distribution as a fucntion of mixture, texture")


####################################################
## DBH
####################################################

data <- data[data$AN_CERNE == 2000, ]

ggplot(data, aes(DHP_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
theme_bw()+
ggtitle("DBH distribution as a fucntion of mixture (entire QC)")

ggplot(data, aes(DHP_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
facet_grid(~ drainage, scales="free", space="free")+
theme_bw()+
ggtitle("DBH distribution as a fucntion of mixture, drainage")

ggplot(data, aes(DHP_CM, fill = mix))+
# geom_histogram(bins= 50)+
geom_density(alpha = 0.5, adjust = 3)+
facet_grid(~ texture, scales="free", space="free")+
theme_bw()+
ggtitle("DBH distribution as a fucntion of mixture, texture")
