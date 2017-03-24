# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("plyr")
library("ggplot2")


####################################################
## data
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
data <- data[, c("ID_PET_MES", "ID_ARB", "AN_CERNE", "BAI_CM", "ESSENCE", "mix", "drainage", "texture", "prop_PET_BA", "prop_SAB_BA")]
data$mix <- factor(data$mix, levels = c("PET", "SAB", "MIX"), ordered = FALSE)



####################################################
## mean(SAB)
####################################################
# Select SAB in SAB stands
SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("SAB", "MIX"), ]
SAB <- ddply(SAB, .(ID_ARB, drainage, texture, mix, prop_SAB_BA), summarise, BAImean = mean(BAI_CM))

plot(SAB$prop_SAB_BA, SAB$BAImean)
summary(lm(BAImean ~ prop_SAB_BA, data = SAB))
summary(lm(BAImean ~ mix, data = SAB))

####################################################
## mean(PET)
####################################################
# Select PET in PET stands
PET <- data[data$ESSENCE == "PET" & data$mix %in% c("PET", "MIX"), ]
PET <- ddply(PET, .(ID_ARB, drainage, texture, mix, prop_SAB_BA), summarise, BAImean = mean(BAI_CM))

plot(PET$prop_SAB_BA, PET$BAImean)
summary(lm(BAImean ~ prop_SAB_BA, data = PET))
summary(lm(BAImean ~ mix, data = PET))

####################################################
## var(SAB)
####################################################
# Select SAB in SAB stands
SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("SAB", "MIX"), ]
SAB <- ddply(SAB, .(ID_ARB, drainage, texture, mix, prop_SAB_BA), summarise, BAIvar = var(BAI_CM))

plot(SAB$prop_SAB_BA, SAB$BAIvar)
summary(lm(BAIvar ~ prop_SAB_BA, data = SAB))
summary(lm(BAIvar ~ mix, data = SAB))

####################################################
## var(PET)
####################################################
# Select PET in PET stands
PET <- data[data$ESSENCE == "PET" & data$mix %in% c("PET", "MIX"), ]
PET <- ddply(PET, .(ID_ARB, drainage, texture, mix, prop_SAB_BA), summarise, BAIvar = var(BAI_CM))

plot(PET$prop_SAB_BA, PET$BAIvar)
summary(lm(BAIvar ~ prop_SAB_BA, data = PET))
summary(lm(BAIvar ~ mix, data = PET))


####################################################
## TS
####################################################
BAItot <- ddply(data, .(mix, AN_CERNE), summarise, BAItot = sum(BAI_CM))

ggplot()+
geom_line(data = BAItot, aes(AN_CERNE, BAItot, colour = mix))


TSmix <- mean(BAItot[BAItot$mix == "MIX", "BAItot"]) / var(BAItot[BAItot$mix == "MIX", "BAItot"])
TSsab <- mean(BAItot[BAItot$mix == "SAB", "BAItot"]) / var(BAItot[BAItot$mix == "SAB", "BAItot"])
TSpet <- mean(BAItot[BAItot$mix == "PET", "BAItot"]) / var(BAItot[BAItot$mix == "PET", "BAItot"])

TSmix
TSsab
TSpet

plot(TSmix, TSsab)

plot(BAItot[BAItot$mix == "MIX", "AN_CERNE"], BAItot[BAItot$mix == "MIX", "BAItot"], type = "l", xlab = "Yr", ylab = "BAItot")
