# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("ggplot2")
library("plyr")
library("reshape2")
library("doParallel")

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

####################################################
# Define stand composition
####################################################
data$mix <- "all"
data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "mix"] <- "MIX"
data[data$prop_PET_BA >= 0.75, "mix"] <- "PET"
data[data$prop_SAB_BA >= 0.75, "mix"] <- "SAB"


data <- melt(data, id.vars = c("ID_PET_MES", "ID_ARB", "ESSENCE", "yr", "rcp", "mod", "rcpmod", "mix"), measure.vars = paste("V", seq(1,10), sep=""))

####################################################
# Define periods p1 and p2
####################################################

data$period <- "p"
data[data$yr >= 1986 & data$yr <= 2005, "period"] <- "p1"
data[data$yr >= 2081 & data$yr <= 2100, "period"] <- "p2"


####################################################
# Define variable classes & name
####################################################

colnames(data)[colnames(data) == "value"] <- "BAI"
data$ID_PET_MES <- as.factor(data$ID_PET_MES)
data$ID_ARB <- as.factor(data$ID_ARB)
data$ESSENCE <- as.factor(data$ESSENCE)
data$rcp <- as.factor(data$rcp)
data$mod <- as.factor(data$mod)
data$rcpmod <- as.factor(data$rcpmod)
data$mix <- factor(data$mix, levels = c("MIX", "PET", "SAB"),ordered = FALSE)
data$period <- factor(data$period, levels = c("p1", "p2", "p"), ordered = FALSE)

####################################################
# Test effet du mélange sur déclin de croissance de
# PET et augmentation de croissance de SAB
####################################################
# SAB
SAB <- ddply(data[data$ESSENCE == "SAB" & data$mix %in% c("MIX", "SAB"),], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = SAB, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")
summary(lm(BAI ~ period + period:mix, data = SAB[SAB$rcp == "rcp45" & SAB$period != "p",]))
summary(lm(BAI ~ period + period:mix, data = SAB[SAB$rcp == "rcp85" & SAB$period != "p",]))


# PET
PET <- ddply(data[data$ESSENCE == "PET" & data$mix %in% c("MIX", "PET"),], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = PET, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")
summary(lm(BAI ~ period + period:mix, data = PET[PET$rcp == "rcp45" & PET$period != "p",]))
summary(lm(BAI ~ period + period:mix, data = PET[PET$rcp == "rcp85" & PET$period != "p",]))

####################################################
# Evolution de la TS pour les différentes compo
####################################################

stand <- ddply(data, .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = stand, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")



TSplot <- function(mix = c("MIX", "PET", "SAB"), period = "p1", rcp = "rcp45"){

  pred <- stand[stand$mix == mix & stand$period == period & stand$rcp == rcp,]

  TS <- mean(pred$BAI) / var(pred$BAI)
  a <- data.frame(mix = mix, period = period, rcp = rcp, TS = TS)
  return(a)
}



b <- data.frame(mix = NA, period = NA, rcp = NA, TS = NA)
for (i in c("MIX", "PET", "SAB")){
  for (j in c("p1", "p2")){
    for (k in c("rcp45", "rcp85")){

      a <- TSplot(mix = i, period = j, rcp = k)
      b <- rbind(b, a)

    }
  }
}

b <- b[-1,]

ggplot(b)+
geom_point(aes(period, TS, color = mix))+
facet_wrap(~ rcp, , scales="free")


c <- dcast(b, mix + rcp ~ period)
c$proportion <- 100 - (c$p2*100/c$p1)


####################################################
# Tests TS(PET + SAB) = TS(MIX)
####################################################

# nombre de sites PET/SAB/MIX pour ponderation dans le calcul de la TS theorique
length(unique(data$ID_PET_MES))
nb_plot_mix <- length(unique(data[data$mix == "MIX", "ID_PET_MES"]))
nb_plot_pet <- length(unique(data[data$mix == "PET", "ID_PET_MES"]))
nb_plot_sab <- length(unique(data[data$mix == "SAB", "ID_PET_MES"]))


# total chronology for stands
stand <- ddply(data, .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = stand, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")

# total chronology for species
# PET
PET <- ddply(data[data$ESSENCE == "PET",], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
PET <- PET[PET$mix != "SAB",]
ggplot()+
geom_line(data = PET, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")
# SAB
SAB <- ddply(data[data$ESSENCE == "SAB",], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
SAB <- SAB[SAB$mix != "PET",]
ggplot()+
geom_line(data = SAB, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")


TS <- function(period = "p1", rcp = "rcp45", psab = 0.5, ppet = 0.5){

  p <- stand[stand$rcp == rcp & stand$period == period,]
  po <- PET[PET$rcp == rcp & PET$period == period,]
  ab <- SAB[SAB$rcp == rcp & SAB$period == period,]

  # ponderation par abondance ds le ppt mixte et par le nombre de placettes utilisées
  psabmix <- (psab/nb_plot_mix)
  ppetmix <- (ppet/nb_plot_mix)
  psab <- psab/nb_plot_sab
  ppet <- ppet/nb_plot_pet


  # var peuplemets mixtes
  varmixobs <- var(p[p$mix == "MIX", "BAI"]) / nb_plot_mix

  # var mix théorique (déduite des mix)
  varmixtheomix <- (ppetmix^2 * var(po[po$mix == "MIX", "BAI"])) + (psabmix^2 * var(ab[ab$mix == "MIX", "BAI"])) + (2 * ppetmix * psabmix * cov(po[po$mix == "MIX", "BAI"], ab[ab$mix == "MIX", "BAI"]))

  # var mix théorique (déduite des mono)
  varmixtheomono <- (ppet^2 * var(po[po$mix == "PET", "BAI"])) + (psab^2 * var(ab[ab$mix == "SAB", "BAI"])) + (2 * ppet * psab * cov(po[po$mix == "PET", "BAI"], ab[ab$mix == "SAB", "BAI"]))



  # mean peuplemets mixtes
  meanmixobs <- mean(p[p$mix == "MIX", "BAI"])  / nb_plot_mix

  # mean mix théorique (déduite des mix)
  meanmixtheomix <- (ppetmix * mean(po[po$mix == "MIX", "BAI"])) + (psabmix * mean(ab[ab$mix == "MIX", "BAI"]))

  # mean mix théorique (déduite des mono)
  meanmixtheomono <- (ppet * mean(po[po$mix == "PET", "BAI"])) + (psab * mean(ab[ab$mix == "SAB", "BAI"]))


  # TS peuplemets mixtes
  TSmixobs <- meanmixobs / varmixobs

  # TS mix théorique (déduite des mix)
  TSmixtheomix <- meanmixtheomix / varmixtheomix

  # TS mix théorique (déduite des mono)
  TSmixtheomono <- meanmixtheomono / varmixtheomono


  a <- data.frame(period = period, rcp = rcp, varmixobs = varmixobs, varmixtheomix = varmixtheomix, varmixtheomono = varmixtheomono, meanmixobs = meanmixobs, meanmixtheomix = meanmixtheomix, meanmixtheomono = meanmixtheomono, TSmixobs = TSmixobs, TSmixtheomix = TSmixtheomix, TSmixtheomono = TSmixtheomono)

  return(a)
}


# test function
a <- TS(period = "p1", rcp = "rcp45", psab = 0.5, ppet = 0.5)

# combinations of proportions
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
plot(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","varmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 0.7), col = "black", main = "1986 - 2005", xlab = "Proportion of balsam fir", ylab = "productivity variance")
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","varmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","varmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","varmixtheomono"], col = "red", lty = 2)

# period 2
# deduce from mix
plot(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","varmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 0.7), col = "black", main = "2081 - 2100", xlab = "Proportion of balsam fir", ylab = "productivity variance")
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","varmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","varmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","varmixtheomono"], col = "red", lty = 2)


####################################################
# Plot mean theo
####################################################

# period 1
# deduce from mix
plot(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","meanmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 100), col = "black", main = "1986 - 2005", xlab = "Proportion of balsam fir", ylab = "productivity")
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","meanmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p1" & b$rcp == "rcp45","psab"], b[b$period == "p1" & b$rcp == "rcp45","meanmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p1" & b$rcp == "rcp85","psab"], b[b$period == "p1" & b$rcp == "rcp85","meanmixtheomono"], col = "red", lty = 2)

# period 2
# deduce from mix
plot(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","meanmixtheomix"], type = "l", xlim = c(0.25, 0.75), ylim = c(0, 100), col = "black", main = "2081 - 2100", xlab = "Proportion of balsam fir", ylab = "productivity")
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","meanmixtheomix"], col = "red", lty = 1)
# deduce from mono
lines(b[b$period == "p2" & b$rcp == "rcp45","psab"], b[b$period == "p2" & b$rcp == "rcp45","meanmixtheomono"], col = "black", lty = 2)
lines(b[b$period == "p2" & b$rcp == "rcp85","psab"], b[b$period == "p2" & b$rcp == "rcp85","meanmixtheomono"], col = "red", lty = 2)



####################################################
# Covariance
####################################################
# function cov intra / inter

covintra <- function(data = PET, period = "p1"){
  # Calculate all combinations of individuals
  comb <- combn(unique(data$ID_ARB),2)

  covintra <- numeric()
  for (j in unique(data$variable)){  #  pour chaque couple 10 mesures de cov, une pour chaque simulation
    dataV <- data[data$variable == j & data$period == period,]
    for (i in 1:dim(comb)[2]){  # une mesure de cov pur chaque couple
      t1 <- dataV[dataV$ID_ARB == comb[1,i], "BAI"]
      t2 <- dataV[dataV$ID_ARB == comb[2,i], "BAI"]
      a <- cov(t1,t2)
      covintra <- c(covintra, a)
    }
  }
  return(covintra)
}


covinter <- function(PET = PET, SAB = SAB, period = "p1"){
  cov_mix <- numeric()
  for (k in unique(SAB$variable)){
    PETV <- PET[PET$variable == k & PET$period == period,]
    SABV <- SAB[SAB$variable == k & SAB$period == period,]
    for (i in unique(PETV$ID_ARB)){
      for (j in unique(SABV$ID_ARB)){
        t1 <- PETV[PETV$ID_ARB == i, "BAI"]
        t2 <- SABV[SABV$ID_ARB == j, "BAI"]
        a <- cov(t1,t2)
        cov_mix <- c(cov_mix, a)
      }
    }
  }
  return(cov_mix)
}

# Data PET and SAB (all trees of each species in PET/SAB/MIX)
PET <- data[data$ESSENCE == "PET", ]
# PET <- PET[PET$variable == "V1",]
SAB <- data[data$ESSENCE == "SAB", ]
# SAB <- SAB[SAB$variable == "V1",]


# cov
cov_petp1 <- covintra(data = PET, period = "p1")
cov_petp2 <- covintra(data = PET, period = "p2")

cov_sabp1 <- covintra(data = SAB, period = "p1")
cov_sabp2 <- covintra(data = SAB, period = "p2")


cov_mixp1 <- covinter(PET = PET, SAB = SAB, period = "p1")
cov_mixp2 <- covinter(PET = PET, SAB = SAB, period = "p2")


boxplot(cov_sabp1, cov_sabp2, cov_petp1, cov_petp2, cov_mixp1, cov_mixp2, names = c("cov_sabp1", "cov_sabp2", "cov_petp1", "cov_petp2", "cov_mixp1", "cov_mixp2"))
