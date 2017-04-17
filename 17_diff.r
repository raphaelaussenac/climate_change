# Delete all objects in the work space
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)

####################################################
# Data
####################################################
# import and merge predictions
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_PETT2D1.rdata")
PET <- predictions
PET$sp <- "PET"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_SABT2D1.rdata")
SAB <- predictions
SAB$sp <- "SAB"
data <- rbind(PET, SAB)

####################################################
# Define mixture
####################################################
data$plot <- 1
data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "plot"] <- "MIX"
data[data$prop_PET_BA >= 0.75, "plot"] <- "PET"
data[data$prop_SAB_BA >= 0.75, "plot"] <- "SAB"

####################################################
# wide to long
####################################################

data <- melt(data, id.vars = c("ID_PET_MES", "ID_ARB", "ESSENCE", "yr", "rcp", "mod", "plot"), measure.vars = paste("V", seq(1,10), sep=""))

####################################################
# difference between mixed and PET
####################################################

# sum of BAI for each year/rcp/mod/sim/mix
pred <- ddply(data, .(yr, rcp, mod, variable, plot), summarise, BAI = sum(value))
# difference between
pred <- dcast(pred, yr + rcp + mod + variable ~ plot)
pred$diff <- pred$MIX - pred$PET

################
# min, max, CI
################

pred_min <- ddply(pred, .(yr, rcp), summarise, diffmin = min(diff))
pred_max <- ddply(pred, .(yr, rcp), summarise, diffmax = max(diff))
pred_CImin <- ddply(pred, .(yr, rcp), summarise, CImin = wilcox.test(diff,conf.int=TRUE)$conf.int[1])
pred_CImax <- ddply(pred, .(yr, rcp), summarise, CImax = wilcox.test(diff,conf.int=TRUE)$conf.int[2])

pred_min$collage <- paste(pred_min$yr, pred_min$rcp, sep = "")
pred_max$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
pred_CImin$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
pred_CImax$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")

pred <- merge(pred_min, pred_max[, c("diffmax", "collage")], by = "collage")
pred <- merge(pred, pred_CImin[, c("CImin", "collage")], by = "collage")
pred <- merge(pred, pred_CImax[, c("CImax", "collage")], by = "collage")

################
# plot
################

pred[pred$rcp == "rcp45", "rcp" ] <- "RCP4.5"
pred[pred$rcp == "rcp85", "rcp" ] <- "RCP8.5"

ggplot(data = pred)+
geom_ribbon(aes(x=yr, ymax=diffmax, ymin=diffmin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI of mixed stands - total BAI of pure aspen stands")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())

ggsave("~/Desktop/diffplot.pdf", width = 4, height = 5)

####################################################
# difference PET decline between  mixed and pure stands
####################################################

# sum of PET BAI for each year/rcp/mod/sim/mix
pred <- ddply(data[data$ESSENCE == "PET" & data$plot %in% c("MIX", "PET"), ], .(yr, rcp, mod, variable, plot), summarise, BAI = sum(value))
# difference between
pred <- dcast(pred, yr + rcp + mod + variable ~ plot)
pred$diff <- pred$MIX - pred$PET

################
# min, max, CI
################

pred_min <- ddply(pred, .(yr, rcp), summarise, diffmin = min(diff))
pred_max <- ddply(pred, .(yr, rcp), summarise, diffmax = max(diff))
pred_CImin <- ddply(pred, .(yr, rcp), summarise, CImin = wilcox.test(diff,conf.int=TRUE)$conf.int[1])
pred_CImax <- ddply(pred, .(yr, rcp), summarise, CImax = wilcox.test(diff,conf.int=TRUE)$conf.int[2])

pred_min$collage <- paste(pred_min$yr, pred_min$rcp, sep = "")
pred_max$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
pred_CImin$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
pred_CImax$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")

pred <- merge(pred_min, pred_max[, c("diffmax", "collage")], by = "collage")
pred <- merge(pred, pred_CImin[, c("CImin", "collage")], by = "collage")
pred <- merge(pred, pred_CImax[, c("CImax", "collage")], by = "collage")

################
# plot
################

pred[pred$rcp == "rcp45", "rcp" ] <- "RCP4.5"
pred[pred$rcp == "rcp85", "rcp" ] <- "RCP8.5"

ggplot(data = pred)+
geom_ribbon(aes(x=yr, ymax=diffmax, ymin=diffmin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI of aspen in mixed stands - in pure aspen stands")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())

ggsave("~/Desktop/diffsp.pdf", width = 4, height = 5)
