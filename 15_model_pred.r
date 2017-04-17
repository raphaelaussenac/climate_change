# Delete all objects in the work space
rm(list = ls(all = TRUE))

####################################################
## Packages
####################################################
# https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
# Packages
library(ggplot2)
library(plyr)
library(merTools)
library(doParallel)

# function preparation données futures
source("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/climate_change/11_fun_model.r")

####################################################
## new data
####################################################
new <- function(data = data){
  data$DHP_CM <- data$DHP_CM
  data$prop_PET_BA <- data$prop_PET_BA
  data$prop_SAB_BA <- data$prop_SAB_BA
  data$BAtot_CM2HA <- data$BAtot_CM2HA
  data$texture <- as.factor(1) #data$texture
  data$drainage <- as.factor(2) # data$drainage
  data$compethard <- data$compethard
  data$competsoft <- data$competsoft
  return(data)
}

####################################################
##
####################################################
setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input/futur_climate/all_climate_data")
fileNames <- Sys.glob("*.rdata")
fileNames

pred_mod <- function(s = c("SAB", "PET")){

  # load model
  if (s == "SAB"){
    load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_SAB_lent_fin.rdata")
  } else if (s == "PET"){
    load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_PET_lent_fin.rdata")
  }

  # predictions
  nsim <- 10
  predictions <- as.data.frame(matrix(ncol = 9 + nsim))
  colnames(predictions) <- c("ID_PET_MES", "ID_ARB", "ESSENCE", "prop_SAB_BA","prop_PET_BA", "yr", paste("V", seq(1,nsim,1), sep=""), "rcp", "mod", "rcpmod" )
  for (i in fileNames){
    load(i)
    data <- new(data)
    sp <- s
    newdata <- prepare_mod_data_futur(data = data, sp = sp)

    # ############### model_select
    # fix <- model.matrix(~ BAtot_CM2HA + sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture + drainage + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp  + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + texture:sizeE + texture:mixE + texture:compethard + texture:competsoft + texture:DC + texture:DCp + texture:Tannual + texture:Pannual + drainage:sizeE + drainage:mixE + drainage:compethard + drainage:competsoft + drainage:DC + drainage:DCp + drainage:Tannual + drainage:Pannual, data = newdata)
    # fix <- as.data.frame(fix)
    # newdata <- cbind(fix, newdata[, c("ID_PET_MES", "ID_ARB", "ESSENCE", 'prop_SAB_BA', 'prop_PET_BA', 'yr')])
    # ############### model_select

    # pred <- predict(mod, re.form = NA, newdata = newdata)
    pred <- predictInterval(mod, newdata = newdata, which = "fixed", level = 0.95, n.sims = nsim, stat = "median", include.resid.var = TRUE, returnSims = TRUE)
    pred <- as.data.frame(attr(pred, "sim.results"))
    newdata <- cbind(newdata, pred)
    # newdata$BAI <- exp(pred)
    # newdata$BAI <- exp(pred$fit)
    # newdata$upr <- exp(pred$upr)
    # newdata$lwr <- exp(pred$lwr)
    newdata$rcp <- substr(i, 1, 5)
    newdata$mod <- substr(i, 7, 11)
    newdata$rcpmod <- substr(i, 1, 11)
    model <- newdata[, colnames(predictions)]
    predictions <- rbind(predictions, model)
  }
  predictions <- predictions[-1, ]

  # save
  setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output")
  save(predictions, file = paste("QC_BAI_", s, "_T1D2", ".rdata", sep = "")) # change name according to T and D
  setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/input/futur_climate/all_climate_data")

}

# We replicated the addresses nCores times.
registerDoParallel(2)
spe <- c("SAB", "PET")
foreach(i = 1:length(spe), .packages = "merTools", .export = c("new", "prepare_mod_data_futur", "fileNames")) %dopar% {
  pred_mod(s = spe[i])
}
