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

  # Add all chronologies from each rcp and each sim
  pred <- ddply(pred, .(yr, rcpmod, sim), summarise, BAI = sum(BAI))

  # define rcp
  pred$rcp <- substr(pred$rcpmod, 1, 5)

  # min, max, mean for each scenario
  pred_min <- ddply(pred, .(yr, rcp), summarise, BAImin = min(BAI))
  pred_max <- ddply(pred, .(yr, rcp), summarise, BAImax = max(BAI))
  pred_CImin <- ddply(pred, .(yr, rcp), summarise, CImin = wilcox.test(BAI,conf.int=TRUE)$conf.int[1])
  pred_CImax <- ddply(pred, .(yr, rcp), summarise, CImax = wilcox.test(BAI,conf.int=TRUE)$conf.int[2])

  pred_min$collage <- paste(pred_min$yr, pred_min$rcp, sep = "")
  pred_max$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
  pred_CImin$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")
  pred_CImax$collage <- paste(pred_max$yr, pred_max$rcp, sep = "")

  pred <- merge(pred_min, pred_max[, c("BAImax", "collage")], by = "collage")
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

pred[pred$rcp == "rcp45", "rcp"] <- "RCP4.5"
pred[pred$rcp == "rcp85", "rcp"] <- "RCP8.5"
colnames(pred)[colnames(pred) == "rcp"] <- "Scenario"

ggplot(data = pred)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = Scenario), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = Scenario), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(~ plot, nrow = 1, scales="free_y",  labeller = as_labeller(c("all" = "a) all stands", "mix" = "b) mixed stands", "PET" = "c) pure aspen stands", "SAB" = "d) pure fir stands")))+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave ("~/Desktop/plot.pdf", width = 8, height= 5)

colnames(pred)[colnames(pred) == "Scenario"] <- "rcp"

####################################################
# SP SCALE: total growth for all, mix, PET & SAB plots
####################################################

tab <- as.data.frame(matrix(ncol = 9))
colnames(tab) <- c("collage", "yr", "rcp", "BAImin", "BAImax", "CImin", "CImax", "plot", "sp")
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

pred_sp[pred_sp$rcp == "rcp45", "rcp"] <- "RCP4.5"
pred_sp[pred_sp$rcp == "rcp85", "rcp"] <- "RCP8.5"

pred_sp <- pred_sp[pred_sp$plot != "all",]

pred_sp$a <- "a"
pred_sp[pred_sp$sp == "PET" & pred_sp$plot == "mix", "a"] <- "a) aspen in mixed stands"
pred_sp[pred_sp$sp == "PET" & pred_sp$plot == "mono", "a"] <- "b) aspen in pure stands"
pred_sp[pred_sp$sp == "SAB" & pred_sp$plot == "mix", "a"] <- "c) fir in mixed stands"
pred_sp[pred_sp$sp == "SAB" & pred_sp$plot == "mono", "a"] <- "d) fir in pure stands"



ggplot(data = pred_sp)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
# facet_wrap(sp ~ plot, scales="free_y", labeller = as_labeller(c("PET" = "a) aspen", "mix" = "in mixed stands", "mono" = "in pure stands", "SAB" = "b) fir")))+
facet_wrap(~ a, nrow = 1, scales="free_y")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave ("~/Desktop/sp.pdf", width = 8, height = 5)
