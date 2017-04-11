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
# test différence entre mixed et pet
####################################################
dd <- data

dd$mix <- factor(dd$mix, levels = c("PET", "SAB", "MIX"), ordered = FALSE)

summary(lm(BAI ~ period + mix + period:mix, data = dd[dd$mix %in% c("MIX", "PET") & dd$period != "p" & dd$rcp == "rcp45",]))


####################################################
# Test effet du mélange sur déclin de croissance de
# PET et augmentation de croissance de SAB
####################################################
# SAB
SAB <- ddply(data[data$ESSENCE == "SAB" & data$mix %in% c("MIX", "SAB"),], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = SAB, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")
SAB$mix <- factor(SAB$mix, levels = c("SAB", "MIX"), ordered = FALSE)
summary(lm(BAI ~ period + mix + period:mix, data = SAB[SAB$rcp == "rcp45" & SAB$period != "p",]))
summary(lm(BAI ~ period + mix + period:mix, data = SAB[SAB$rcp == "rcp85" & SAB$period != "p",]))
ggplot()+
geom_boxplot(data = SAB[SAB$period != "p",], aes(period, BAI, color = mix))+
facet_grid(~ rcp, scale = "fixed")


# PET
PET <- ddply(data[data$ESSENCE == "PET" & data$mix %in% c("MIX", "PET"),], .(yr, rcp, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = PET, aes(yr, BAI, color = variable))+
facet_grid(rcp ~ mix, scale = "fixed")+
theme_bw()
PET$mix <- factor(PET$mix, levels = c("PET", "MIX"), ordered = FALSE)
summary(lm(BAI ~ period + mix + period:mix, data = PET[PET$rcp == "rcp45" & PET$period != "p",]))
summary(lm(BAI ~ period + mix + period:mix, data = PET[PET$rcp == "rcp85" & PET$period != "p",]))
ggplot()+
geom_boxplot(data = PET[PET$period != "p",], aes(period, BAI, color = mix))+
facet_grid(~ rcp, scale = "fixed")


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

colnames(b)[colnames(b)=="mix"] <- "compo"
ggplot(b)+
geom_point(aes(period, TS, shape = compo), size = 3)+
facet_wrap(~ rcp, , scales="free")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"))
ggsave ("~/Desktop/TS.pdf", width = 7, height = 10)
colnames(b)[colnames(b)=="compo"] <- "mix"


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
stand <- ddply(data, .(yr, rcpmod, variable, mix, period), summarise, BAI = sum(BAI))
ggplot()+
geom_line(data = stand, aes(yr, BAI, color = rcpmod))+
facet_grid(variable ~ mix, scale = "fixed")

# total chronology for species
# PET
PET <- ddply(data[data$ESSENCE == "PET",], .(yr, rcpmod, variable, mix, period), summarise, BAI = sum(BAI))
PET <- PET[PET$mix != "SAB",]
ggplot()+
geom_line(data = PET, aes(yr, BAI, color = rcpmod))+
facet_grid(variable ~ mix, scale = "fixed")
# SAB
SAB <- ddply(data[data$ESSENCE == "SAB",], .(yr, rcpmod, variable, mix, period), summarise, BAI = sum(BAI))
SAB <- SAB[SAB$mix != "PET",]
ggplot()+
geom_line(data = SAB, aes(yr, BAI, color = rcpmod))+
facet_grid(variable ~ mix, scale = "fixed")


TS <- function(period = "p1", rcpmod = "rcp45_ACCES", psab = 0.5, ppet = 0.5, variable = "V1"){

  p <- stand[stand$rcpmod == rcpmod & stand$period == period & variable == variable,]
  po <- PET[PET$rcpmod == rcpmod & PET$period == period & variable == variable,]
  ab <- SAB[SAB$rcpmod == rcpmod & SAB$period == period & variable == variable,]

  # ponderation par abondance ds le ppt mixte et par le nombre de placettes utilisées
  psabmix <- (psab/nb_plot_mix)
  ppetmix <- (ppet/nb_plot_mix)
  psab <- psab/nb_plot_sab
  ppet <- ppet/nb_plot_pet


  # var peuplemets mixtes
  varmixobs <- var(p[p$mix == "MIX", "BAI"]) / nb_plot_mix

  # var mix théorique (déduite des mix)
  varpetmix <- (ppetmix^2 * var(po[po$mix == "MIX", "BAI"]))
  varsabmix <- (psabmix^2 * var(ab[ab$mix == "MIX", "BAI"]))
  covmix <- (2 * ppetmix * psabmix * cov(po[po$mix == "MIX", "BAI"], ab[ab$mix == "MIX", "BAI"]))
  varmixtheomix <- varpetmix + varsabmix + covmix

  # var mix théorique (déduite des mono)
  varpetmono <- (ppet^2 * var(po[po$mix == "PET", "BAI"]))
  varsabmono <- (psab^2 * var(ab[ab$mix == "SAB", "BAI"]))
  covmono <- (2 * ppet * psab * cov(po[po$mix == "PET", "BAI"], ab[ab$mix == "SAB", "BAI"]))
  varmixtheomono <- varpetmono + varsabmono + covmono


  # mean peuplemets mixtes
  meanmixobs <- mean(p[p$mix == "MIX", "BAI"])  / nb_plot_mix

  # mean mix théorique (déduite des mix)
  meanpetmix <- (ppetmix * mean(po[po$mix == "MIX", "BAI"]))
  meansabmix <- (psabmix * mean(ab[ab$mix == "MIX", "BAI"]))
  meanmixtheomix <- meanpetmix + meansabmix

  # mean mix théorique (déduite des mono)
  meanpetmono <- (ppet * mean(po[po$mix == "PET", "BAI"]))
  meansabmono <- (psab * mean(ab[ab$mix == "SAB", "BAI"]))
  meanmixtheomono <- meanpetmono + meansabmono


  # TS peuplemets mixtes
  TSmixobs <- meanmixobs / varmixobs

  # TS mix théorique (déduite des mix)
  TSmixtheomix <- meanmixtheomix / varmixtheomix

  # TS mix théorique (déduite des mono)
  TSmixtheomono <- meanmixtheomono / varmixtheomono


  a <- data.frame(period = period, rcpmod = rcpmod, varmixobs = varmixobs, varmixtheomix = varmixtheomix, varmixtheomono = varmixtheomono, meanmixobs = meanmixobs, meanmixtheomix = meanmixtheomix, meanmixtheomono = meanmixtheomono, TSmixobs = TSmixobs, TSmixtheomix = TSmixtheomix, TSmixtheomono = TSmixtheomono, variable = variable, varpetmix, varsabmix, covmix, varpetmono, varsabmono, covmono, meanpetmix, meansabmix, meanpetmono, meansabmono )

  return(a)
}


# test function
a <- TS(period = "p1", rcpmod = "rcp45_ACCES", psab = 0.5, ppet = 0.5, variable = "V1")

# combinations of proportions
prop <- data.frame(ppet = seq(0,1, 0.01))
prop$psab <- 1 - prop$ppet


b <- data.frame(psab = NA, ppet = NA, period = NA, rcpmod = NA, varmixobs = NA, varmixtheomix = NA, varmixtheomono = NA, meanmixobs = NA, meanmixtheomix = NA, meanmixtheomono = NA, TSmixobs = NA, TSmixtheomix = NA, TSmixtheomono = NA, variable = NA, varpetmix = NA, varsabmix = NA, covmix = NA, varpetmono = NA, varsabmono = NA, covmono = NA, meanpetmix = NA, meansabmix = NA, meanpetmono = NA, meansabmono = NA)
for (i in c("p1", "p2")){
  for (j in unique(data$rcpmod)){
    for (l in unique(data$variable)){
      for (k in 1:nrow(prop)){

        psab <- prop[k, "psab"]
        ppet <- prop[k, "ppet"]

        a <- TS(period = i, rcpmod = j, psab = psab, ppet = ppet, variable = l)
        a <- cbind(psab, ppet, a)
        b <- rbind(b, a)

      }
    }
  }
}

b <- b[-1,]


# save(b, file="~/Desktop/b.rdata")
load(file="~/Desktop/b.rdata")

# weighted variance https://www.maths-forum.com/superieur/variance-ponderee-plus-deux-variables-t78091.html

####################################################
# Plot TS theo
####################################################
# Une estimataion par simulation pour chaque modele et chaque sc climatique
b$rcpmodsim <- paste(b$rcpmod, b$variable, sep = "")
# define rcp
b$rcp <- substr(b$rcpmod,1,5)


# min, max pour ribbon
minmax <- function(period = "p1", mixmono = "mix", component = "TS"){
  if (component == "TS"){
    b$componentmix <- b$TSmixtheomix
    b$componentmono <- b$TSmixtheomono
  } else if (component == "mean"){
    b$componentmix <- b$meanpetmix + b$meansabmix
    b$componentmono <- b$meanpetmono + b$meansabmono
  } else if (component == "variance"){
    b$componentmix <- b$varpetmix + b$varsabmix + b$covmix
    b$componentmono <- b$varpetmono + b$varsabmono + b$covmono
  } else if (component == "meanpet"){
    b$componentmix <- b$meanpetmix
    b$componentmono <- b$meanpetmono
  } else if (component == "meansab"){
    b$componentmix <- b$meansabmix
    b$componentmono <- b$meansabmono
  } else if (component == "varpet"){
    b$componentmix <- b$varpetmix
    b$componentmono <- b$varpetmono
  } else if (component == "varsab"){
    b$componentmix <- b$varsabmix
    b$componentmono <- b$varsabmono
  } else if (component == "cov"){
    b$componentmix <- b$covmix
    b$componentmono <- b$covmono
  }

  if (mixmono == "mix"){
    compomin <- ddply(b[b$period == period, ], .(psab, rcp), summarise, compomin = min(componentmix))
    compomax <- ddply(b[b$period == period, ], .(psab, rcp), summarise, compomax = max(componentmix))
    # CImin <- ddply(b[b$period == period, ], .(psab, rcp), summarise, CImin = wilcox.test(componentmix, conf.int=TRUE)$conf.int[1])
    # CImax <- ddply(b[b$period == period, ], .(psab, rcp), summarise, CImax = wilcox.test(componentmix, conf.int=TRUE)$conf.int[2])
    compomix <- cbind(compomin, compomax[,"compomax"])#, CImin[,"CImin"], CImax[,"CImax"])
    colnames(compomix) <- c("psab", "rcp", paste(component, "min", sep = ""), paste(component, "max", sep = ""))#, "CImin", "CImax")
    a <- compomix
  } else if (mixmono == "mono"){
    compomin <- ddply(b[b$period == period, ], .(psab, rcp), summarise, compomin = min(componentmono))
    compomax <- ddply(b[b$period == period, ], .(psab, rcp), summarise, compomax = max(componentmono))
    # CImin <- ddply(b[b$period == period, ], .(psab, rcp), summarise, CImin = wilcox.test(componentmono, conf.int=TRUE)$conf.int[1])
    # CImax <- ddply(b[b$period == period, ], .(psab, rcp), summarise, CImax = wilcox.test(componentmono, conf.int=TRUE)$conf.int[2])
    compomono <- cbind(compomin, compomax[,"compomax"])#, CImin[,"CImin"], CImax[,"CImax"])
    colnames(compomono) <- c("psab", "rcp", paste(component, "min", sep = ""), paste(component, "max", sep = ""))#, "CImin", "CImax")
    a <- compomono
  }
  return(a)
}


plotTS <- function(component = "TS"){

  # period 1
  p1mix <- minmax(period = "p1",  mixmono = "mix", component = component)
  p1mix$rcp <- paste(p1mix$rcp, "mix", sep = "")
  p1mix$period <- "1986-2005"
  p1mono <- minmax(period = "p1",  mixmono = "mono", component = component)
  p1mono$rcp <- paste(p1mono$rcp, "mono", sep = "")
  p1mono$period <- "1986-2005"

  # period 2
  p2mix <- minmax(period = "p2",  mixmono = "mix", component = component)
  p2mix$rcp <- paste(p2mix$rcp, "mix", sep = "")
  p2mix$period <- "2081-2100"
  p2mono <- minmax(period = "p2",  mixmono = "mono", component = component)
  p2mono$rcp <- paste(p2mono$rcp, "mono", sep = "")
  p2mono$period <- "2081-2100"
  p <- rbind(p1mix, p2mix, p1mono, p2mono)

  p$RCP <- 1
  p[substr(p$rcp, 1, 5) == "rcp45", "RCP"] <- "RCP4.5"
  p[substr(p$rcp, 1, 5) == "rcp85", "RCP"] <- "RCP8.5"
  p$RCP <- as.factor(p$RCP)
  p$mix <- 1
  p[substr(p$rcp, 6, 9) == "mono", "mix"] <- "pure"
  p[substr(p$rcp, 6, 8) == "mix", "mix"] <- "mixed"
  p$mix <- as.factor(p$mix)

  p$lettre <- 1
  p[p$period == "1986-2005" & p$mix == "mixed", "lettre"] <- "a) "
  p[p$period == "2081-2100" & p$mix == "mixed", "lettre"] <- "b) "
  p[p$period == "1986-2005" & p$mix == "pure", "lettre"] <- "c) "
  p[p$period == "2081-2100" & p$mix == "pure", "lettre"] <- "d) "
  p$plotp <- paste(paste(p$lettre, component, "t", p$mix, sep = ""), p$period, sep = " ")
  p$plotp <- as.factor(p$plotp)

  colnames(p)[colnames(p) == paste(component, "max", sep = "")] <- "Ymax"
  colnames(p)[colnames(p) == paste(component, "min", sep = "")] <- "Ymin"

  # plot
  ggplot()+
  geom_ribbon(data = p, aes(x = psab, ymax = Ymax, ymin = Ymin, fill = RCP), alpha = 0.5)+
  facet_wrap(~ plotp, nrow = 1)+
  theme_bw()+
  xlim(0,1)+
  theme(strip.background = element_rect(colour = "white", fill = "white"), legend.title=element_blank(), legend.position = "bottom",)+
  xlab("proportion of fir")+
  ylab("") #component)

  ggsave (paste("~/Desktop/", component, "theo.pdf", sep = ""), width = 8, height = 5)

}

plotTS(component = "varsab")


for (i in c("TS", "mean", "variance", "meanpet", "varpet", "meansab", "varsab", "cov")){
  plotTS(component = i)
}


####################################################
# Covariance parallel predicted
####################################################

# Filenames <-
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_petp1.rdata")
cov_petp1 <- as.numeric(cov_petp1)

load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_petp2.rdata")
cov_petp2 <- as.numeric(cov_petp2)

load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_sabp1.rdata")
cov_sabp1 <- as.numeric(cov_sabp1)

load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_sabp2.rdata")
cov_sabp2 <- as.numeric(cov_sabp2)


load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_mixp1.rdata")
cov_mixp1 <- as.numeric(cov_mixp1)

load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/cov_mixp2.rdata")
cov_mixp2 <- as.numeric(cov_mixp2)


boxplot(cov_petp1, cov_petp2, cov_sabp1, cov_sabp2, cov_mixp1, cov_mixp2)


petp1 <- data.frame(sp = "PET", cov = cov_petp1, period = "p1")
petp2 <- data.frame(sp = "PET", cov = cov_petp2, period = "p2")
sabp1 <- data.frame(sp = "SAB", cov = cov_sabp1, period = "p1")
sabp2 <- data.frame(sp = "SAB", cov = cov_sabp2, period = "p2")
mixp1 <- data.frame(sp = "MIX", cov = cov_mixp1, period = "p1")
mixp2 <- data.frame(sp = "MIX", cov = cov_mixp2, period = "p2")


tab <- rbind(petp1, petp2, sabp1, sabp2, mixp1, mixp2)


library("ggplot2")

ggplot()+
geom_histogram(data = tab, position = "dodge", aes(cov, fill = sp), bins = 100)+
facet_wrap(~period)

ggplot()+
geom_density(data = tab, aes(cov, fill = sp), alpha = 0.5)+
facet_wrap(~period)

ggplot()+
geom_boxplot(data = tab, aes(sp, cov))+
facet_wrap(~period)





mean(cov_petp1)
mean(cov_sabp1)
mean(cov_mixp1)

t.test(cov_petp1, cov_mixp1)


# # function cov intra / inter
#
# covintra <- function(data = PET, period = "p1"){
#   # Calculate all combinations of individuals
#   comb <- combn(unique(data$ID_ARB),2)
#
#   covintra <- numeric()
#   for (j in unique(data$variable)){  #  pour chaque couple 10 mesures de cov, une pour chaque simulation
#     dataV <- data[data$variable == j & data$period == period,]
#     for (i in 1:dim(comb)[2]){  # une mesure de cov pur chaque couple
#       t1 <- dataV[dataV$ID_ARB == comb[1,i], "BAI"]
#       t2 <- dataV[dataV$ID_ARB == comb[2,i], "BAI"]
#       a <- cov(t1,t2)
#       covintra <- c(covintra, a)
#     }
#   }
#   return(covintra)
# }
#
#
# covinter <- function(PET = PET, SAB = SAB, period = "p1"){
#   cov_mix <- numeric()
#   for (k in unique(SAB$variable)){
#     PETV <- PET[PET$variable == k & PET$period == period,]
#     SABV <- SAB[SAB$variable == k & SAB$period == period,]
#     for (i in unique(PETV$ID_ARB)){
#       for (j in unique(SABV$ID_ARB)){
#         t1 <- PETV[PETV$ID_ARB == i, "BAI"]
#         t2 <- SABV[SABV$ID_ARB == j, "BAI"]
#         a <- cov(t1,t2)
#         cov_mix <- c(cov_mix, a)
#       }
#     }
#   }
#   return(cov_mix)
# }
#
# # Data PET and SAB (all trees of each species in PET/SAB/MIX)
# PET <- data[data$ESSENCE == "PET", ]
# # PET <- PET[PET$variable == "V1",]
# SAB <- data[data$ESSENCE == "SAB", ]
# # SAB <- SAB[SAB$variable == "V1",]
#
#
# # cov
# cov_petp1 <- covintra(data = PET, period = "p1")
# cov_petp2 <- covintra(data = PET, period = "p2")
#
# cov_sabp1 <- covintra(data = SAB, period = "p1")
# cov_sabp2 <- covintra(data = SAB, period = "p2")
#
#
# cov_mixp1 <- covinter(PET = PET, SAB = SAB, period = "p1")
# cov_mixp2 <- covinter(PET = PET, SAB = SAB, period = "p2")
#
#
# boxplot(cov_sabp1, cov_sabp2, cov_petp1, cov_petp2, cov_mixp1, cov_mixp2, names = c("cov_sabp1", "cov_sabp2", "cov_petp1", "cov_petp2", "cov_mixp1", "cov_mixp2"))
