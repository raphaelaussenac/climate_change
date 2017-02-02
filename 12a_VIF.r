# Delete all objects in the work space
rm(list=ls(all=TRUE))
library(lme4)
library(arm)
library(piecewiseSEM)
library(MuMIn)
library(effects)
library(dplyr)

# https://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/

####################################################
## Data
####################################################
sp <- "SAB"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
source("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/climate_change/11_fun_model.r")
data <- prepare_mod_data(sp = sp)

# mise en forme tel que lmer/lm le fait (permet de prendre en compte les facteurs --> 0/1)
fix <- model.matrix(~ BAtot_CM2HA + sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture + drainage + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp  + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + texture:sizeE + texture:mixE + texture:compethard + texture:competsoft + texture:DC + texture:DCp + texture:Tannual + texture:Pannual + drainage:sizeE + drainage:mixE + drainage:compethard + drainage:competsoft + drainage:DC + drainage:DCp + drainage:Tannual + drainage:Pannual, data = data)
resp <- data[, c("lBAI_CM")]
data <- cbind(as.data.frame(resp), fix, data$ID_PET_MES, data$ID_ARB)
colnames(data)[1] <- "lBAI"
colnames(data)[(ncol(data)-1)] <- "ID_PET_MES"
colnames(data)[ncol(data)] <- "ID_ARB"

####################################################
## Formule de base
####################################################
form <- lBAI ~ sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture2 + texture3 + drainage2 + texture2:drainage2 + texture3:drainage2 + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + sizeE:texture2 + sizeE:texture3 + mixE:texture2 + mixE:texture3 + compethard:texture2 + compethard:texture3 + competsoft:texture2 + competsoft:texture3 + DC:texture2 + DC:texture3 + DCp:texture2 + DCp:texture3 + Tannual:texture2 + Tannual:texture3 + Pannual:texture2 + Pannual:texture3 + sizeE:drainage2 + mixE:drainage2 + compethard:drainage2 + competsoft:drainage2 + DC:drainage2 + DCp:drainage2 + Tannual:drainage2 + Pannual:drainage2 + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)

####################################################
## Formule pour sélection de variable
####################################################

# PET select
# form <- lBAI ~ texture2 + texture2:drainage2 + texture3:drainage2 + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + sizeE:texture2 + sizeE:texture3 + mixE:texture2 + mixE:texture3 + compethard:texture2 + compethard:texture3 + competsoft:texture2 + competsoft:texture3 + DC:texture3 + DCp:texture2 + DCp:texture3 + Tannual:texture2 + Tannual:texture3 + Pannual:texture2 + Pannual:texture3 + sizeE:drainage2 + mixE:drainage2 + compethard:drainage2 + DCp:drainage2 + Tannual:drainage2 + Pannual:drainage2 + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)

# SAB select
form <- lBAI ~ texture2 + texture2:drainage2 + texture3:drainage2 + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp + compethard:DC + competsoft:DC + competsoft:DCp + mixE:DCp + sizeE:texture2 + sizeE:texture3 + compethard:texture2 + compethard:texture3 + competsoft:texture2 + competsoft:texture3 + DC:texture2 + DC:texture3 + DCp:texture2 + DCp:texture3 + Tannual:texture2 + Tannual:texture3 + Pannual:texture2 + Pannual:texture3 + sizeE:drainage2 + compethard:drainage2 + competsoft:drainage2 + DC:drainage2 + DCp:drainage2 + Tannual:drainage2 + Pannual:drainage2 + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)



####################################################
## Run model
####################################################

speed <- "lent"

if (speed == "rapid"){
  mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 500)))
} else if (speed == "lent"){
  # mod <-  lmer(form, data=data, REML = FALSE)
  mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
}

####################################################
## VIF
####################################################

vif.mer <- function (fit) {
    ## adapted from rms::vif

    v <- vcov(fit)
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}


VIF <- as.data.frame(vif.mer(mod))
VIF$par <- rownames(VIF)

VIF <- VIF[order(VIF[,1], decreasing = TRUE), ]
par(mfrow = c(1,2))
dotchart(VIF[, 1], VIF[, 2], pch = 16)
VIF

####################################################
## dotchart
####################################################

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

  if (sp == "SAB"){
    dotchart(par$mean, labels = par$param, cex = 0.7, main = "SAB", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
    abline(v = 0, col = "orange")
    # abline(h = c(26.5, 35.5))
    segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
  }

  if (sp == "PET"){
    dotchart(par$mean, labels = par$param, cex = 0.7, main = "PET", xlab = "", pch = 16, xlim = c(min(par$bas), max(par$haut)), color=par$col)
    abline(v = 0, col = "orange")
    # abline(h = c(26.5, 35.5))
    segments(par$bas, 1:nrow(par), par$haut, 1:nrow(par), col = par$col)
  }

}

dot(data = mod, sp = sp)

####################################################
## R2 c & m
####################################################
sem.model.fits(mod)
rsquared(mod)
r.squaredGLMM(mod)

#
# ####################################################
# ## save
# ####################################################
# if (sp == "SAB"){
#   save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_SAB_select.rdata")
# } else if (sp == "PET"){
#   save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_PET_select.rdata")
# }
