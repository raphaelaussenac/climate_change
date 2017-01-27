# Delete all objects in the work space
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)

####################################################
# Data
####################################################
# import and merge predictions
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_PET.rdata")
PET <- predictions
PET$sp <- "PET"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_SAB.rdata")
SAB <- predictions
SAB$sp <- "SAB"
data <- rbind(PET, SAB)

predict <- function(data = data){
  # prediction at the QC scale
  tab <- data.frame(yr = NA, rcpmod = NA, BAI = NA, sim = NA)
  for (i in colnames(data)[substr(colnames(data), 1, 1) == "V"]){
    pred <- data[, c("yr", "rcpmod", i)]
    colnames(pred)[3] <- "V"
    pred <- ddply(pred, .(yr, rcpmod), summarise, BAI = sum(V))
    pred$sim <- i
    tab <- rbind(tab, pred)
  }
  pred <- tab[-1, ]

  # min, max, mean for each scenario
  pred$rcp <- substr(pred$rcpmod, 1, 5)
  pred_min <- ddply(pred, .(yr, rcp), summarise, BAImin = min(BAI))
  pred_mean <- ddply(pred, .(yr, rcp), summarise, BAImean = mean(BAI))
  pred_max <- ddply(pred, .(yr, rcp), summarise, BAImax = max(BAI))
  pred_CImin <- ddply(pred, .(yr, rcp), summarise, CImin = wilcox.test(BAI,conf.int=TRUE)$conf.int[1])
  pred_CImax <- ddply(pred, .(yr, rcp), summarise, CImax = wilcox.test(BAI,conf.int=TRUE)$conf.int[2])


  pred_min$collage <- paste(pred_min$yr, pred_min$rcp, sep = "")
  pred_mean$collage <- paste(pred_mean$yr, pred_mean$rcp, sep = "")
  pred_max$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
  pred_CImin$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
  pred_CImax$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")

  pred <- merge(pred_min, pred_mean[, c("BAImean", "collage")], by = "collage")
  pred <- merge(pred, pred_max[, c("BAImax", "collage")], by = "collage")
  pred <- merge(pred, pred_CImin[, c("CImin", "collage")], by = "collage")
  pred <- merge(pred, pred_CImax[, c("CImax", "collage")], by = "collage")

  return(pred)
}

####################################################
# PLOT SCALE: total growth for all, mix, PET & SAB plots
####################################################

pred_all <- predict(data)
pred_all$plot <- "all"
pred_mix <- predict(data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, ])
pred_mix$plot <- "mix"
pred_PET <- predict(data[data$prop_PET_BA >= 0.75, ])
pred_PET$plot <- "PET"
pred_SAB <- predict(data[data$prop_SAB_BA >= 0.75, ])
pred_SAB$plot <- "SAB"

pred <- rbind(pred_all, pred_mix, pred_PET, pred_SAB)

####################################################
# plots
####################################################

ggplot(data = pred)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
# stat_smooth(aes(yr, BAImin, fill = rcp, color = rcp), method="loess", se = FALSE)+
# stat_smooth(aes(yr, BAImean, fill = rcp, color = rcp), method="loess", se = FALSE)+
# stat_smooth(aes(yr, BAImax, fill = rcp, color = rcp), method="loess", se = FALSE)+
# facet_grid(~ plot, rcpmodales="free", space="free")
facet_wrap(~ plot, scales="free_y")+
ggsave ("~/Desktop/plot.pdf", width=10, height= 5)


####################################################
# SP SCALE: total growth for all, mix, PET & SAB plots
####################################################

tab <- as.data.frame(matrix(ncol = 10))
colnames(tab) <- c("collage", "yr", "rcp", "BAImin", "BAImean", "BAImax", "CImin", "CImax", "plot", "sp")
for (i in unique(data$sp)){
  pred_all <- predict(data[data$ESSENCE == i, ])
  pred_all$plot <- "all"
  pred_mix <- predict(data[data$ESSENCE == i & data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, ])
  pred_mix$plot <- "mix"

  pred_mono <- predict(data[data$ESSENCE == i & data[, paste("prop_", i, "_BA", sep = "")] >= 0.75, ])
  pred_mono$plot <- "mono"

  pred_sp <- rbind(pred_all, pred_mix, pred_mono)
  pred_sp$sp <- i

  tab <- rbind(tab, pred_sp)
}
pred_sp <- tab[-1, ]

####################################################
# plots
####################################################

ggplot(data = pred_sp)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
# stat_smooth(aes(yr, BAImin, fill = rcp, color = rcp), method = "loess", se = FALSE)+
# stat_smooth(aes(yr, BAImean, fill = rcp, color = rcp), method = "loess", se = FALSE)+
# stat_smooth(aes(yr, BAImax, fill = rcp, color = rcp), method = "loess", se = FALSE)+
# facet_grid(sp ~ plot, scales="fixed", space="fixed")
facet_wrap(sp ~ plot, scales="free_y")+
# stat_density2d(aes(x=yr, y=BAImean, fill = rcp), geom="polygon", alpha = 0.2)
# geom_density2d(aes(x=yr, y=BAImean, colour = rcp))+
ggsave ("~/Desktop/sp.pdf", width=10, height= 5)
