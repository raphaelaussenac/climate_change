# Delete all objects in the work space
rm(list=ls(all=TRUE))
# options(digits=22)

####################################################
## Packages
####################################################
# Packages
library(lme4)
library(arm)
library(ggplot2)
library(piecewiseSEM)
library(DHARMa)
library(MuMIn)
library(effects)
library(dplyr)

####################################################
## Model
####################################################
# data
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_SAB_lent_fin.rdata")
modSAB <- mod
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_PET_lent_fin.rdata")
modPET <- mod

# ####################################################
# ## CI & dotchart functions
# ####################################################
# # CI
# CI <- function(data = data){
#   # bayesian estimation of the parameters
#   nsim <- 2000
#   bsim <- sim(data, n.sim=nsim)
#   #str(bsim)
#   par <- round(apply(bsim@fixef,2,quantile, prob=c(0.025, 0.5, 0.975)), digits=3)
#   par
#
#   # representer les chaines (avec ou sans burnin)
#   nbpar <- dim(bsim@fixef)[2] ## nb of parameters
#
#   tab <- data.frame()
#   tab1 <- data.frame()
#   for (i in 1:nbpar){
#     tab <- rbind(tab, as.data.frame(bsim@fixef[,i]))
#     tab1 <- rbind(tab1, as.data.frame(rep(colnames(bsim@fixef)[i],nsim)))
#   }
#   posterior <- cbind(tab, tab1)
#   colnames(posterior) <- c("distrib", "parameter")
#
#   options(digits=2) ### seul moyen pour arrondir les étiquettes des axes
#   p1 <- ggplot(data=posterior, aes(distrib, group=parameter))+
#   geom_histogram()+
#   geom_vline(xintercept = 0, color="red", size=0.25)+
#   facet_wrap(~parameter, scales="free")+
#   theme(axis.text.x=element_text(angle=30, hjust=1, size=7))
#   options(digits=7) ### retour à la valeur par défaut
#   return(p1)
# }


# dotchart
dot <- function(data = data, sp = sp){
  # bayesian estimation of the parameters
  nsim <- 2000
  bsim <- sim(data, n.sim=nsim)
  #str(bsim)
  par <- round(apply(bsim@fixef,2,quantile, prob=c(0.025, 0.5, 0.975)), digits=3)
  par <- t(par)
  par <- as.data.frame(par)
  colnames(par) <- c("bas", "mean", "haut")

  a <- as.data.frame(matrix(ncol = 2, nrow = nrow(par)))
  for (i in 1:nrow(par)){
    a[i,1] <- rownames(par)[i]
    a[i,2] <- sum(bsim@fixef[,i]<0)/nsim
  }
  a$sign <- 1
  a[a$V2 >= 0.95, "sign"] <- "neg"
  a[a$V2 <= 0.05, "sign"] <- "pos"
  a[a$V2 > 0.05 & a$V2 < 0.95, "sign"] <- "ns"

  par$param <- rownames(par)
  par <- merge(par, a, by.x = "param", by.y = "V1")
  par$col <- 1
  par[par$sign == "pos", "col"] <- "blue"
  par[par$sign == "neg", "col"] <- "red"
  par[par$sign == "ns", "col"] <- "grey"

  a <- c("(Intercept)", "sizeE", "mixE", "compethard", "competsoft", "DC", "DCp", "Pannual", "Tannual", "drainage2", "texture2", "texture3")
  b <- c("compethard:DC", "compethard:DCp", "competsoft:DC", "competsoft:DCp", "mixE:BAtot_CM2HA", "mixE:DC", "mixE:DCp", "sizeE:DC", "sizeE:DCp")

  par$ord <- 3
  par[par$param %in% a, "ord"] <- 1
  par[par$param %in% b, "ord"] <- 2

  par <- par[order(par$ord),]
  par <- par %>% arrange(-row_number())
  par <- par[-nrow(par), ]

  # sp <- sp

  if (sp == "SAB"){
    dotchart(par$mean, labels = par$param, cex = 0.7, main = "SAB", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
    abline(v = 0, col = "orange")
    abline(h = c(26.5, 35.5))
    segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
  }

  if (sp == "PET"){
    dotchart(par$mean, labels = par$param, cex = 0.7, main = "PET", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
    abline(v = 0, col = "orange")
    abline(h = c(26.5, 35.5))
    segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
  }

}

####################################################
## plot dotchart
####################################################

par(mfrow = c(1,2))
dot(data = modSAB, sp = "SAB")
dot(data = modPET, sp = "PET")
quartz.save("~/Desktop/dochart.pdf", type = "pdf", device = dev.cur())

# ####################################################
# ## plot CI
# ####################################################
#
# par(mfrow = c(1,1))
# pSAB <- CI(data = modSAB, sp = "SAB")
# pPET <- CI(data = modPET, sp = "PET")
#
# ### specific hypothesis
# sum(bsim@fixef[,15]>0)/nsim

####################################################
## R^2
####################################################
# library(devtools)
# install_github("jslefche/piecewiseSEM")
# library(piecewiseSEM)

# package piecewiseSEM
library(piecewiseSEM)
sem.model.fits(mod)
rsquared(mod)

# package MuMIn
library(MuMIn)
r.squaredGLMM(mod)
# r.squaredLR(mod)

####################################################
## Effect displays
####################################################
allEffects(mod)
plot(allEffects(mod))


eff <- as.data.frame(Effect(c("texture","DCp"),mod, se=T))  ## texture, texture toujours en premier
colnames(eff)[colnames(eff)=="fit"] <- "lBAI"
plot(Effect(c("texture","DCp"),mod, se=T))


eff$texture <- factor(eff$texture, levels= sort(unique(eff$texture)),ordered = TRUE)

ggplot(eff, aes(x=DCp,y=lBAI,group=texture)) +
geom_line(aes(color=as.integer(as.character(texture)))) +
# facet_grid(. ~ plotsppar, scales = "free", space = "fixed", switch = "x")
scale_colour_gradient(name = "texture", low = "green", high = "red")+
theme(axis.ticks = element_line(colour="black"),
axis.text.y = element_text(size= 13, angle=0, hjust=0.5),
axis.text.x = element_text(size= 13, angle=0, hjust=0.5),
axis.title.y = element_text(size = 15, angle = 90),
panel.background=element_blank(),
panel.grid.major.x=element_blank(),
panel.grid.major.y=element_line(colour="grey90"),
# panel.grid.major = element_line(colour="white"),
legend.key=element_rect(colour = "white", fill=NA),
panel.grid.minor = element_blank(),
panel.border=element_rect(colour = "black", fill=NA),
strip.background = element_rect(colour="white",fill="white"),
# strip.text.x = element_text(colour = "black", angle = 0, size = 10, hjust = 0.5, vjust = 0.5),
strip.text.x = element_blank(),
panel.margin = unit(0.25, "lines"))+
# plot.margin = unit(c(0,0,0,0),"mm"))+
xlab("DCp")+
ylab("log(BAI)")
# ggtitle("Fixed effects values (absolute values)")


####################################################
## Beta
####################################################

# SAB
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/SAB_DBH_beta/resultGA.rdata")
GA@solution
# beta
C <- GA@solution[1]        # [0,10]
D <- GA@solution[2]      # [0,10]
curve(  (x^(C-1)) * ((1-x)^(D-1)), xlim=c(0,1), col="red", main="beta(DBH)", xlab="proportion", ylab="")
axis(1, at = seq(0, 1, by = 0.1), las=1)

# PET
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/PET_DBH_beta/resultGA.rdata")
GA@solution
# beta
C <- GA@solution[1]        # [0,10]
D <- GA@solution[2]      # [0,10]
curve(  (x^(C-1)) * ((1-x)^(D-1)), xlim=c(0,1), add=T, col="green")


#
abline(v= 0.41, type='p', col="red", lty=2)
abline(v= 0.51, type='p', col="green", lty=2)
