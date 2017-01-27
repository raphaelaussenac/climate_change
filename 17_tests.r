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

pred$rcp <- substr(pred$rcpmod, 1, 5)

ggplot(data = pred, aes(x = yr, y = BAI))+
geom_line(aes(colour = rcp))+
facet_wrap(~ plot, scales="free_y")

####################################################
# Tests plot scale
####################################################
# Evolution de la prod dans le futur

# period
pred$period <- "p"
pred[pred$yr >= 1986 & pred$yr <= 2005, "period"] <- "p1"
pred[pred$yr >= 2081 & pred$yr <= 2100, "period"] <- "p2"
# class
pred$period <- factor(pred$period, levels = c("p1", "p2", "p"), ordered = FALSE)
pred$rcp <- as.factor(pred$rcp)
pred$plot <- factor(pred$plot, levels = c("all", "mix", "PET", "SAB"),ordered = FALSE)
pred$sim <- as.factor(pred$sim)
pred$rcpmod <- as.factor(pred$rcpmod)
pred$yr <- as.numeric(pred$yr)


predrcp45 <- pred[pred$rcp == "rcp45", ]
# tests (paramètre d'intérêt = periodp2)
predrcp45 <- predrcp45[predrcp45$period != "p", ]
predrcp45all <- predrcp45[predrcp45$plot == "all", ]
summary(lm(BAI ~ period, data = predrcp45all))
predrcp45mix <- predrcp45[predrcp45$plot == "mix", ]
summary(lm(BAI ~ period, data = predrcp45mix))
predrcp45PET <- predrcp45[predrcp45$plot == "PET", ]
summary(lm(BAI ~ period, data = predrcp45PET))
predrcp45SAB <- predrcp45[predrcp45$plot == "SAB", ]
summary(lm(BAI ~ period, data = predrcp45SAB))



predrcp85 <- pred[pred$rcp == "rcp85", ]
# tests (paramètre d'intérêt = periodp2)
predrcp85 <- predrcp85[predrcp85$period != "p", ]
predrcp85all <- predrcp85[predrcp85$plot == "all", ]
summary(lm(BAI ~ period, data = predrcp85all))
predrcp85mix <- predrcp85[predrcp85$plot == "mix", ]
summary(lm(BAI ~ period, data = predrcp85mix))
predrcp85PET <- predrcp85[predrcp85$plot == "PET", ]
summary(lm(BAI ~ period, data = predrcp85PET))
predrcp85SAB <- predrcp85[predrcp85$plot == "SAB", ]
summary(lm(BAI ~ period, data = predrcp85SAB))



# ####################################################
# # TS
# ####################################################
# # Evolution de la variance dans le futur
#
# predmix <- pred[pred$plot == "mix", ]
# predmix <- predmix[predmix$rcp == "rcp85", ]
# predmix <- predmix[order(predmix$period), ]
# # ratio de variance
# var(predmix[predmix$period == "p2", "BAI"]) / var(predmix[predmix$period == "p1", "BAI"])
# # ratio de TS
# # (mean(predmix[predmix$period == "p2", "BAI"]) / var(predmix[predmix$period == "p2", "BAI"]) )/ (mean(predmix[predmix$period == "p1", "BAI"]) / var(predmix[predmix$period == "p1", "BAI"]) )
# # TS
# TSmixp1 <- mean(predmix[predmix$period == "p1", "BAI"]) / var(predmix[predmix$period == "p1", "BAI"])
# TSmixp2 <- mean(predmix[predmix$period == "p2", "BAI"]) / var(predmix[predmix$period == "p2", "BAI"])
#
# predPET <- pred[pred$plot == "PET", ]
# predPET <- predPET[predPET$rcp == "rcp85", ]
# predPET <- predPET[order(predPET$period), ]
# var(predPET[predPET$period == "p2", "BAI"]) / var(predPET[predPET$period == "p1", "BAI"])
# # (mean(predPET[predPET$period == "p2", "BAI"]) / var(predPET[predPET$period == "p2", "BAI"]) )/ (mean(predPET[predPET$period == "p1", "BAI"]) / var(predPET[predPET$period == "p1", "BAI"]) )
# TSPETp1 <- mean(predPET[predPET$period == "p1", "BAI"]) / var(predPET[predPET$period == "p1", "BAI"])
# TSPETp2 <- mean(predPET[predPET$period == "p2", "BAI"]) / var(predPET[predPET$period == "p2", "BAI"])
#
# predSAB <- pred[pred$plot == "SAB", ]
# predSAB <- predSAB[predSAB$rcp == "rcp85", ]
# predSAB <- predSAB[order(predSAB$period), ]
# var(predSAB[predSAB$period == "p2", "BAI"]) / var(predSAB[predSAB$period == "p1", "BAI"])
# # (mean(predSAB[predSAB$period == "p2", "BAI"]) / var(predSAB[predSAB$period == "p2", "BAI"]) )/ (mean(predSAB[predSAB$period == "p1", "BAI"]) / var(predSAB[predSAB$period == "p1", "BAI"]) )
# TSSABp1 <- mean(predSAB[predSAB$period == "p1", "BAI"]) / var(predSAB[predSAB$period == "p1", "BAI"])
# TSSABp2 <- mean(predSAB[predSAB$period == "p2", "BAI"]) / var(predSAB[predSAB$period == "p2", "BAI"])
#
# TSmixp1
# TSPETp1
# TSSABp1
# #
# #
# #
# TSmixp2
# TSPETp2
# TSSABp2
#


# modvar <- summary(lm(BAI ~  period, data = pred3))
# coco <- resid(modvar)
#
#
# plot(coco, type="p")
# abline(v = dim(pred3[pred3$period== "p1",])[1])
# segments(0, min(coco[1:dim(pred3[pred3$period== "p1",])[1]]), dim(pred3[pred3$period== "p1",])[1], min(coco[1:dim(pred3[pred3$period== "p1",])[1]]))
# segments(0, max(coco[1:dim(pred3[pred3$period== "p1",])[1]]), dim(pred3[pred3$period== "p1",])[1], max(coco[1:dim(pred3[pred3$period== "p1",])[1]]))
# segments(dim(pred3[pred3$period== "p2",])[1], min(coco[dim(pred3[pred3$period== "p2",])[1] : length(coco)]), length(coco), min(coco[dim(pred3[pred3$period== "p2",])[1] : length(coco)]))
# segments(dim(pred3[pred3$period== "p2",])[1], max(coco[dim(pred3[pred3$period== "p2",])[1] : length(coco)]), length(coco), max(coco[dim(pred3[pred3$period== "p2",])[1] : length(coco)]))
#
# # augmentation par x fois
# #sorte de variance avec mean(résidus) forçée à 0
# coco1 <- (sum((coco[1:800]-0)^2))/(800-1)
# coco2 <- (sum((coco[801:length(coco)]-0)^2))/(800-1)
# coco2 / coco1
#
# # somme des résidus
# coco3 <- sqrt(coco^2)
# sum(coco3[ (dim(pred3[pred3$period == "p1", ])[1]+1) : length(coco3)]) / sum(coco3[ 1 : dim(pred3[pred3$period == "p1", ])[1] ])



####################################################
# SP SCALE: total growth for all, mix, PET & SAB plots
####################################################

tab <- as.data.frame(matrix(ncol = 6))
colnames(tab) <- c("yr", "rcpmod", "BAI", "sim", "plot", "sp")

for (i in unique(data$sp)){
  pred_mix <- predict(data[data$ESSENCE == i & data$prop_PET_BA <= 0.75 & data$prop_SAB_BA <= 0.75, ])
  pred_mix$plot <- "mix"

  pred_mono <- predict(data[data$ESSENCE == i & data[, paste("prop_", i, "_BA", sep = "")] >= 0.75, ])
  pred_mono$plot <- "mono"

  pred_sp <- rbind(pred_mix, pred_mono)
  pred_sp$sp <- i

  tab <- rbind(tab, pred_sp)
}
pred_sp <- tab[-1, ]

####################################################
# plots
####################################################

pred_sp$rcp <- substr(pred_sp$rcpmod, 1, 5)

ggplot(data = pred_sp, aes(x = yr, y = BAI))+
geom_line(aes(colour = rcp))+
facet_wrap(sp ~ plot, scales="free_y")


####################################################
# Tests sp scale
####################################################
# Déclin du peuplier plus faible en peuplement mixte qu'en peuplement pur?
# Augmentation de croissance du sapin plus forte en peuplement mixte qu'en peuplement pur

# period
pred_sp$period <- "p"
pred_sp[pred_sp$yr >= 1986 & pred_sp$yr <= 2005, "period"] <- "p1"
pred_sp[pred_sp$yr >= 2081 & pred_sp$yr <= 2100, "period"] <- "p2"
# class
pred_sp$period <- factor(pred_sp$period, levels = c("p1", "p2", "p"), ordered = FALSE)
pred_sp$rcp <- as.factor(pred_sp$rcp)
pred_sp$sp <- as.factor(pred_sp$sp)
pred_sp$plot <- factor(pred_sp$plot, levels = c("mono", "mix"),ordered = FALSE)
pred_sp$sim <- as.factor(pred_sp$sim)
pred_sp$rcpmod <- as.factor(pred_sp$rcpmod)
pred_sp$yr <- as.numeric(pred_sp$yr)


####################################################
# peuplier

pop <- pred_sp[pred_sp$period != "p", ]

poprcp45 <- pop[pop$rcp == "rcp45", ]
poprcp45 <- poprcp45[poprcp45$sp == "PET",]
summary(lm(BAI ~ period + period:plot, data = poprcp45))

poprcp85 <- pop[pop$rcp == "rcp85", ]
poprcp85 <- poprcp85[poprcp85$sp == "PET",]
summary(lm(BAI ~ period + period:plot, data = poprcp85))

####################################################
# sapin

abi <- pred_sp[pred_sp$period != "p", ]

abircp45 <- abi[abi$rcp == "rcp45", ]
abircp45 <- abircp45[abircp45$sp == "SAB",]
summary(lm(BAI ~ period + period:plot, data = abircp45))


abircp85 <- abi[abi$rcp == "rcp85", ]
abircp85 <- abircp85[abircp85$sp == "SAB",]
summary(lm(BAI ~ period + period:plot, data = abircp85))








####################################################
# Tests var(PET + SAB) = var(MIX)
####################################################

# var peuplemets mixtes
varmixobs <- var(predrcp45[predrcp45$period == "p1" & predrcp45$plot == "mix", "BAI"])
varmixobs

# var mix théorique (déduite des mix)
varmixtheomix <- var(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mix", "BAI"]) + var(abircp45[abircp45$period == "p1" & abircp45$plot == "mix", "BAI"]) + 2 * cov(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mix", "BAI"], abircp45[abircp45$period == "p1" & abircp45$plot == "mix", "BAI"])
varmixtheomix

# var mix théorique (déduite des mono)
varmixtheomono <- var(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mono", "BAI"]) + var(abircp45[abircp45$period == "p1" & abircp45$plot == "mono", "BAI"]) + 2 * cov(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mono", "BAI"], abircp45[abircp45$period == "p1" & abircp45$plot == "mono", "BAI"])
varmixtheomono





meanmixobs <- mean(predrcp45[predrcp45$period == "p1" & predrcp45$plot == "mix", "BAI"])
meanmixobs

meanmixtheomix <- mean(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mix", "BAI"]) + mean(abircp45[abircp45$period == "p1" & abircp45$plot == "mix", "BAI"])
meanmixtheomix

meanmixtheomono <- mean(poprcp45[poprcp45$period == "p1" & poprcp45$plot == "mono", "BAI"]) + mean(abircp45[abircp45$period == "p1" & abircp45$plot == "mono", "BAI"])
meanmixtheomono




TSmixobs <- meanmixobs / varmixobs
TSmixobs

TSmixtheomix <- meanmixtheomix / varmixtheomix
TSmixtheomix

TSmixtheomono <- meanmixtheomono / varmixtheomono
TSmixtheomono
