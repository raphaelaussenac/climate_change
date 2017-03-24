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
dot <- function(data = data, sp = sp, inter = "del"){
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
  a[a$V2 >= 0.90, "sign"] <- "neg-"
  a[a$V2 >= 0.95, "sign"] <- "neg"
  a[a$V2 >= 0.99, "sign"] <- "neg+"
  a[a$V2 <= 0.1, "sign"] <- "pos-"
  a[a$V2 <= 0.05, "sign"] <- "pos"
  a[a$V2 <= 0.01, "sign"] <- "pos+"
  a[a$V2 > 0.1 & a$V2 < 0.90, "sign"] <- "ns"


  par$param <- rownames(par)
  par <- merge(par, a, by.x = "param", by.y = "V1")
  par$col <- 1
  par[par$sign == "pos+", "col"] <- "blue"
  par[par$sign == "pos", "col"] <- "blue"
  par[par$sign == "pos-", "col"] <- "blue"
  par[par$sign == "neg+", "col"] <- "red"
  par[par$sign == "neg", "col"] <- "red"
  par[par$sign == "neg-", "col"] <- "red"
  par[par$sign == "ns", "col"] <- "grey"

  if (inter == "keep"){
    # keep Intercept
    par <- par[1,]
  } else if (inter == "del"){
    # delete Intercept
    par <- par[-1,]
  }

  # sifnificance symbol
  par$symb <- ""
  par[par$sign == "pos-" | par$sign == "neg-", "symb"] <- ""
  par[par$sign == "pos" | par$sign == "neg","symb"] <- "**"
  par[par$sign == "pos+" | par$sign == "neg+","symb"] <- "***"
  # sort by strength
  par <- par[order(par$mean),]

  # Change variable name
  par$param[par$param == "(Intercept)"] <- "    intercept "
  # avant les deux points
  par$param[substr(par$param, 1, 7) == "texture"] <- paste("T", substr(par$param[substr(par$param, 1, 7) == "texture"], 8, nchar(par$param[substr(par$param, 1, 7) == "texture"])), sep = "")

  par$param[substr(par$param, 1, 8) == "drainage"] <- paste("D", substr(par$param[substr(par$param, 1, 8) == "drainage"], 9, nchar(par$param[substr(par$param, 1, 8) == "drainage"])), sep = "")

  par$param[substr(par$param, 1, 5) == "sizeE"] <- paste("DBH", substr(par$param[substr(par$param, 1, 5) == "sizeE"], 6, nchar(par$param[substr(par$param, 1, 5) == "sizeE"])), sep = "")

  par$param[substr(par$param, 1, 4) == "mixE"] <- paste("Pr", substr(par$param[substr(par$param, 1, 4) == "mixE"], 5, nchar(par$param[substr(par$param, 1, 4) == "mixE"])), sep = "")

  par$param[substr(par$param, 1, 2) == "DC"] <- paste("DCm", substr(par$param[substr(par$param, 1, 2) == "DC"], 3, nchar(par$param[substr(par$param, 1, 2) == "DC"])), sep = "")

  par$param[substr(par$param, 1, 3) == "DCp"] <- paste("DCmp", substr(par$param[substr(par$param, 1, 3) == "DCp"], 4, nchar(par$param[substr(par$param, 1, 3) == "DCp"])), sep = "")

  # avant les deux points
  # compethard / soft
  par$param[substr(par$param, 4, 14) == "compethard"] <- paste(substr(par$param[substr(par$param, 4, 14) == "compethard"], 1, 3), "Ch", sep = "")
  par$param[substr(par$param, 4, 14) == "competsoft"] <- paste(substr(par$param[substr(par$param, 4, 14) == "competsoft"], 1, 3), "Cs", sep = "")
  par$param[substr(par$param, 5, 15) == "compethard"] <- paste(substr(par$param[substr(par$param, 5, 15) == "compethard"], 1, 4), "Ch", sep = "")
  par$param[substr(par$param, 5, 15) == "competsoft"] <- paste(substr(par$param[substr(par$param, 5, 15) == "competsoft"], 1, 4), "Cs", sep = "")
  par$param[substr(par$param, 6, 16) == "compethard"] <- paste(substr(par$param[substr(par$param, 6, 16) == "compethard"], 1, 5), "Ch", sep = "")
  par$param[substr(par$param, 6, 16) == "competsoft"] <- paste(substr(par$param[substr(par$param, 6, 16) == "competsoft"], 1, 5), "Cs", sep = "")
  # drainage
  par$param[substr(par$param, 4, 11) == "drainage"] <- paste(substr(par$param[substr(par$param, 4, 11) == "drainage"], 1, 3), "D", substr(par$param[substr(par$param, 4, 11) == "drainage"], 12, 12), sep = "")
  # P & T annual
  par$param[substr(par$param, 4, 10) == "Pannual"] <- paste(substr(par$param[substr(par$param, 4, 10) == "Pannual"], 1, 3), "Pan", sep = "")
  par$param[substr(par$param, 4, 10) == "Tannual"] <- paste(substr(par$param[substr(par$param, 4, 10) == "Tannual"], 1, 3), "Tan", sep = "")
  # DC & DCp
  par$param[substr(par$param, 4, 6) == "DC"] <- paste(substr(par$param[substr(par$param, 4, 6) == "DC"], 1, 3), "DCm", sep = "")
  par$param[substr(par$param, 5, 7) == "DC"] <- paste(substr(par$param[substr(par$param, 5, 7) == "DC"], 1, 4), "DCm", sep = "")


  par$param[substr(par$param, 4, 7) == "DCp"] <- paste(substr(par$param[substr(par$param, 4, 7) == "DCp"], 1, 3), "DCmp", sep = "")
  par$param[substr(par$param, 5, 8) == "DCp"] <- paste(substr(par$param[substr(par$param, 5, 8) == "DCp"], 1, 4), "DCmp", sep = "")


  # MixE sizeE BAtot
  par$param[substr(par$param, 4, 8) == "mixE"] <- paste(substr(par$param[substr(par$param, 4, 8) == "mixE"], 1, 3), "Pr", sep = "")
  par$param[substr(par$param, 4, 9) == "sizeE"] <- paste(substr(par$param[substr(par$param, 4, 9) == "sizeE"], 1, 3), "DBH", sep = "")
  par$param[substr(par$param, 4, 15) == "BAtot_CM2HA"] <- paste(substr(par$param[substr(par$param, 4, 15) == "BAtot_CM2HA"], 1, 3), "BAt", sep = "")

  # a <- c("(Intercept)", "sizeE", "mixE", "compethard", "competsoft", "DC", "DCp", "Pannual", "Tannual", "drainage2", "texture2", "texture3")
  # b <- c("compethard:DC", "compethard:DCp", "competsoft:DC", "competsoft:DCp", "mixE:BAtot_CM2HA", "mixE:DC", "mixE:DCp", "sizeE:DC", "sizeE:DCp")
  #
  # par$ord <- 3
  # par[par$param %in% a, "ord"] <- 1
  # par[par$param %in% b, "ord"] <- 2
  #
  # par <- par[order(par$ord),]
  # par <- par %>% arrange(-row_number())
  # par <- par[-nrow(par), ]

  # sp <- sp

  if (sp == "SAB"){
    if (inter == "del"){
      # dotchart(par$mean, labels = par$param, cex = 0.7, main = "fir", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
      dotchart(par$mean, labels = par$param, cex = 0.7, main = "fir", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color = "black")

      abline(v = 0, col = "grey", lty = 2)
      text(x = par$mean, y = 1:nrow(par), labels = par$symb, pos = 4, col = "black", cex = 0.75)
      # abline(h = c(26.5, 35.5))
      # segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
      segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = "black")
    } else if (inter == "keep"){
      dotchart(par$mean, labels = par$param, cex = 0.7, xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color = "black")
      text(x = par$mean, y = 1:nrow(par), labels = par$symb, pos = 4, col = "black", cex = 0.75)
      segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = "black")
    }
  }


  if (sp == "PET"){
    if (inter == "del"){
      # dotchart(par$mean, labels = par$param, cex = 0.7, main = "aspen", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
      dotchart(par$mean, labels = par$param, cex = 0.7, main = "aspen", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color = "black")

      abline(v = 0, col = "grey", lty = 2)
      text(x = par$mean, y = 1:nrow(par), labels = par$symb, pos = 4, col = "black", cex = 0.75)
      # abline(h = c(26.5, 35.5))
      # segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
      segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = "black")
    } else if (inter == "keep"){
      dotchart(par$mean, labels = par$param, cex = 0.7, xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color = "black")
      abline(v = 0, col = "grey", lty = 2)
      text(x = par$mean, y = 1:nrow(par), labels = par$symb, pos = 4, col = "black", cex = 0.75)
      segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = "black")
    }
  }
}

####################################################
## plot dotchart
####################################################

par(mar = c(2,0.1,1.25,1.25))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), height = c(12,1.5))
dot(data = modSAB, sp = "SAB", inter = "del")
dot(data = modPET, sp = "PET", inter = "del")
dot(data = modSAB, sp = "SAB", inter = "keep")
dot(data = modPET, sp = "PET", inter = "keep")

quartz.save("~/Desktop/dochart.pdf", type = "pdf", width = 8, height = 12, device = dev.cur())

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
round(r.squaredGLMM(mod), digits = 2)
# r.squaredLR(mod)

####################################################
## Effect displays
####################################################
allEffects(mod)
plot(allEffects(mod))


eff <- as.data.frame(Effect(c("competsoft","DC"),mod, se=T))  ## competsoft, competsoft toujours en premier
colnames(eff)[colnames(eff)=="fit"] <- "lBAI"
plot(Effect(c("competsoft","DC"),mod, se=T))


eff$competsoft <- factor(eff$competsoft, levels= sort(unique(eff$competsoft)),ordered = TRUE)

ggplot(eff, aes(x=DC,y=lBAI,group=competsoft)) +
geom_line(aes(color=as.integer(as.character(competsoft)))) +
# facet_grid(. ~ plotsppar, scales = "free", space = "fixed", switch = "x")
scale_colour_gradient(name = "competsoft", low = "green", high = "red")+
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
xlab("DC")+
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
