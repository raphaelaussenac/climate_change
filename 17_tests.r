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


# nombre de sites PET/SAB/MIX
length(unique(data$ID_PET_MES))
nb_plot_mix <- length(unique(data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "ID_PET_MES"]))
nb_plot_pet <- length(unique(data[data$prop_PET_BA >= 0.75 , "ID_PET_MES"]))
nb_plot_sab <- length(unique(data[data$prop_SAB_BA >= 0.75, "ID_PET_MES"]))



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



####################################################
# TS plot
####################################################

TSplot <- function(plot = c("mix", "PET", "SAB"), period = "p1", rcp = "rcp45"){
  if (plot == "mix"){
    if (rcp == "rcp45"){
      pred <- predrcp45mix
    } else if (rcp == "rcp85"){
      pred <- predrcp85mix
    }
  } else if (plot == "PET"){
    if (rcp == "rcp45"){
      pred <- predrcp45PET
    } else if (rcp == "rcp85"){
      pred <- predrcp85PET
    }
  } else if (plot == "SAB"){
    if (rcp == "rcp45"){
      pred <- predrcp45SAB
    } else if (rcp == "rcp85"){
      pred <- predrcp85SAB
    }
  }

  TS <- mean(pred[pred$period == period, "BAI"]) / var(pred[pred$period == period, "BAI"])
  a <- data.frame(plot = plot, period = period, rcp = rcp, TS = TS)
  return(a)
}



b <- data.frame(plot = NA, period = NA, rcp = NA, TS = NA)
for (i in c("mix", "PET", "SAB")){
  for (j in c("p1", "p2")){
    for (k in c("rcp45", "rcp85")){

      a <- TSplot(plot = i, period = j, rcp = k)
      b <- rbind(b, a)

    }
  }
}

b <- b[-1,]

ggplot(b)+
geom_point(aes(period, TS, color = plot))+
facet_wrap(~ rcp, , scales="free")


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

TS <- function(period = "p1", rcp = "rcp45", psab = 0.5, ppet = 0.5){
  if (rcp == "rcp45"){
    p <- predrcp45
    po <- poprcp45
    ab <- abircp45
  } else if (rcp == "rcp85"){
    p <- predrcp85
    po <- poprcp85
    ab <- abircp85
  }

  psabmix <- (psab/nb_plot_mix)
  ppetmix <- (ppet/nb_plot_mix)

  psab <- psab/nb_plot_sab
  ppet <- ppet/nb_plot_pet


  # var peuplemets mixtes
  varmixobs <- var(p[p$period == period & p$plot == "mix", "BAI"]) / nb_plot_mix

  # var mix théorique (déduite des mix)
  varmixtheomix <- (ppetmix^2 * var(po[po$period == period & po$plot == "mix", "BAI"])) + (psabmix^2 * var(ab[ab$period == period & ab$plot == "mix", "BAI"])) + (2 * ppetmix * psabmix * cov(po[po$period == period & po$plot == "mix", "BAI"], ab[ab$period == period & ab$plot == "mix", "BAI"]))

  # var mix théorique (déduite des mono)
  varmixtheomono <- (ppet^2 * var(po[po$period == period & po$plot == "mono", "BAI"])) + (psab^2 * var(ab[ab$period == period & ab$plot == "mono", "BAI"])) + (2 * ppet * psab * cov(po[po$period == period & po$plot == "mono", "BAI"], ab[ab$period == period & ab$plot == "mono", "BAI"]))



  # mean peuplemets mixtes
  meanmixobs <- mean(p[p$period == period & p$plot == "mix", "BAI"])  / nb_plot_mix

  # mean mix théorique (déduite des mix)
  meanmixtheomix <- (ppetmix * mean(po[po$period == period & po$plot == "mix", "BAI"])) + (psabmix * mean(ab[ab$period == period & ab$plot == "mix", "BAI"]))

  # mean mix théorique (déduite des mono)
  meanmixtheomono <- (ppet * mean(po[po$period == period & po$plot == "mono", "BAI"])) + (psab * mean(ab[ab$period == period & ab$plot == "mono", "BAI"]))


  # TS peuplemets mixtes
  TSmixobs <- meanmixobs / varmixobs

  # TS mix théorique (déduite des mix)
  TSmixtheomix <- meanmixtheomix / varmixtheomix

  # TS mix théorique (déduite des mono)
  TSmixtheomono <- meanmixtheomono / varmixtheomono


  a <- data.frame(period = period, rcp = rcp, varmixobs = varmixobs, varmixtheomix = varmixtheomix, varmixtheomono = varmixtheomono, meanmixobs = meanmixobs, meanmixtheomix = meanmixtheomix, meanmixtheomono = meanmixtheomono, TSmixobs = TSmixobs, TSmixtheomix = TSmixtheomix, TSmixtheomono = TSmixtheomono)

  return(a)
}

a <- TS(period = "p1", rcp = "rcp45", psab = 0.5, ppet = 0.5)


prop <- data.frame(ppet = seq(0,1, 0.01))
prop$psab <- 1 - prop$ppet


b <- data.frame(psab = NA, ppet = NA, period = NA, rcp = NA, varmixobs = NA, varmixtheomix = NA, varmixtheomono = NA, meanmixobs = NA, meanmixtheomix = NA, meanmixtheomono = NA, TSmixobs = NA, TSmixtheomix = NA, TSmixtheomono = NA)
for (i in c("p1", "p2")){
  for (j in c("rcp45", "rcp85")){
    for (k in 1:nrow(prop)){

      psab <- prop[k, "psab"]
      ppet <- prop[k, "ppet"]

      a <- TS(period = i, rcp = j, psab = psab, ppet = ppet)
      a <- cbind(psab, ppet, a)
      b <- rbind(b, a)

    }
  }
}

b <- b[-1,]

# weighted variance https://www.maths-forum.com/superieur/variance-ponderee-plus-deux-variables-t78091.html

####################################################
# Plot TS theo
####################################################

par(mfrow = c(1,2))
# period 1
# deduce from mix
plot(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","TSmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 1500), col = "black", main = "1986 - 2005", xlab = "Proportion of balsam fir", ylab = "TS")
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","TSmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","TSmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","TSmixtheomono"], col = "red", lty = 2)

# period 2
# deduce from mix
plot(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","TSmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 1500), col = "black", main = "2081 - 2100", xlab = "Proportion of balsam fir", ylab = "TS")
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","TSmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","TSmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","TSmixtheomono"], col = "red", lty = 2)


####################################################
# Plot var theo
####################################################

# period 1
# deduce from mix
plot(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","varmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 0.05), col = "black", main = "1986 - 2005", xlab = "Proportion of balsam fir", ylab = "productivity variance")
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","varmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","varmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","varmixtheomono"], col = "red", lty = 2)

# period 2
# deduce from mix
plot(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","varmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 0.05), col = "black", main = "2081 - 2100", xlab = "Proportion of balsam fir", ylab = "productivity variance")
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","varmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","varmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","varmixtheomono"], col = "red", lty = 2)


####################################################
# Plot mean theo
####################################################

# period 1
# deduce from mix
plot(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","meanmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 6), col = "black", main = "1986 - 2005", xlab = "Proportion of balsam fir", ylab = "productivity")
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","meanmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","meanmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","meanmixtheomono"], col = "red", lty = 2)

# period 2
# deduce from mix
plot(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","meanmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 6), col = "black", main = "2081 - 2100", xlab = "Proportion of balsam fir", ylab = "productivity")
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","meanmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","meanmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","meanmixtheomono"], col = "red", lty = 2)
