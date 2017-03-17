prepare_mod_data <- function(sp=sp){
  ####################################################
  ## Packages
  ####################################################
  library("lme4")

  ####################################################
  ## Data
  ####################################################
  meas <- data
  meas$ID_PET_MES <- as.factor(meas$ID_PET_MES)
  meas$ID_ARB <- as.factor(meas$ID_ARB)
  meas$DD <- as.factor(meas$DD)

  ####################################################
  ## Select Species
  ####################################################
  sp <- sp   # SAB or PET
  sizefun <- "DBH"    # weibull / logN / DBH
  mixturreE <- "prop"     # beta / prop

  if (sp == "SAB"){
    meas <- meas[meas$ESSENCE=="SAB",]
  } else if (sp == "PET"){
    meas <- meas[meas$ESSENCE=="PET",]
  }
  #
  # ####################################################
  # ## weibull, logN, beta functions
  # ####################################################
  # if (sp == "SAB"){
  #   load("~/ownCloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/SAB_DBH_beta/resultGA.rdata")
  # } else if (sp == "PET"){
  #   load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/PET_DBH_beta/resultGA.rdata")
  # }
  # # # weibull
  # # DBHopt <- GA@solution[1]
  # # DBHb   <- GA@solution[2]
  # # beta
  # C <- GA@solution[1]
  # D <- GA@solution[2]
  ####################################################
  ## Size effect
  ####################################################
  if (sizefun == "logN" ){
    meas$sizeE <- exp((-1/2)*(( log(meas$DHP_CM/DBHopt) )/DBHb)^2)
  } else if (sizefun == "weibull" ){
    meas$sizeE <- ((meas$DHP_CM/DBHopt)^(DBHb-1))  *   exp(-(meas$DHP_CM/DBHopt)^DBHb)
  } else if (sizefun == "DBH"){
    meas$sizeE <- meas$DHP_CM
  }

  ####################################################
  ## Mixture effect
  ####################################################
  if (sp == "SAB"){
    if (mixturreE == "beta"){
      meas$mixE <- (meas$prop_SAB_BA^(C-1)) * ((1-meas$prop_SAB_BA)^(D-1))
    } else if (mixturreE == "prop"){
      meas$mixE <- meas$prop_SAB_BA
    }
  } else if (sp == "PET"){
    if (mixturreE == "beta"){
      meas$mixE <- (meas$prop_PET_BA^(C-1)) * ((1-meas$prop_PET_BA)^(D-1))
    } else if (mixturreE == "prop"){
      meas$mixE <- meas$prop_PET_BA
    }
  }

  ####################################################
  ## DC
  ####################################################
  # meas$DC <- meas$DCMAXmay_jun_jul
  meas$DC <- meas$DCMAXjun_jul_aug

  ####################################################
  ## Scaling variables
  ####################################################
  # save unscaled parameters
  if (sp=="PET"){
    meas_scale <- meas
    save(meas_scale, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/meas_PET.rdata")
  } else if (sp=="SAB"){
    meas_scale <- meas
    save(meas_scale, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/meas_SAB.rdata")
  }
  meas$sizeE <- scale(meas$sizeE)
  meas$mixE <- scale(meas$mixE)
  meas$BAtot_CM2HA <- scale(meas$BAtot_CM2HA)
  meas$compethard <- scale(meas$compethard)
  meas$competsoft <- scale(meas$competsoft)
  meas$Tannual <- scale(meas$Tannual)
  meas$Pannual <- scale(meas$Pannual)
  meas$DC <- scale(meas$DC)
  meas$DCp <- scale(meas$DCp)
  return(meas)
}



prepare_mod_data_futur <- function(data = data, sp=sp){
  ####################################################
  ## Packages
  ####################################################
  library("lme4")

  ####################################################
  ## Data
  ####################################################
  meas <- data
  meas$ID_PET_MES <- as.factor(meas$ID_PET_MES)
  meas$ID_ARB <- as.factor(meas$ID_ARB)

  ####################################################
  ## Select Species
  ####################################################
  sp <- sp   # SAB or PET
  sizefun <- "DBH"    # weibull / logN / DBH
  mixturreE <- "prop"     # beta / prop

  if (sp == "SAB"){
    meas <- meas[meas$ESSENCE=="SAB",]
  } else if (sp == "PET"){
    meas <- meas[meas$ESSENCE=="PET",]
  }

  # ####################################################
  # ## Select mixture level
  # ####################################################
  # mix <- mix
  # if (mix=="mono"){
  #   if (sp == "SAB"){
  #     meas <- meas[meas$prop_SAB_BA>0.8,]
  #   } else if (sp == "PET"){
  #     meas <- meas[meas$prop_PET_BA>0.8,]
  #   }
  # } else if (mix=="mix"){
  #   if (sp == "SAB"){
  #     meas <- meas[meas$prop_SAB_BA>0.3 & meas$prop_SAB_BA<=0.8,]
  #   } else if (sp == "PET"){
  #     meas <- meas[meas$prop_PET_BA>0.3 & meas$prop_PET_BA<=0.8,]
  #   }
  # }

  # ####################################################
  # ## weibull, logN, beta functions
  # ####################################################
  # if (sp == "SAB"){
  #   load("~/ownCloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/SAB_DBH_beta/resultGA.rdata")
  # } else if (sp == "PET"){
  #   load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/RE on slopes/PET_DBH_beta/resultGA.rdata")
  # }
  # # # weibull
  # # DBHopt <- GA@solution[1]
  # # DBHb   <- GA@solution[2]
  # # beta
  # C <- GA@solution[1]
  # D <- GA@solution[2]
  ####################################################
  ## Size effect
  ####################################################
  if (sizefun == "logN" ){
    meas$sizeE <- exp((-1/2)*(( log(meas$DHP_CM/DBHopt) )/DBHb)^2)
  } else if (sizefun == "weibull" ){
    meas$sizeE <- ((meas$DHP_CM/DBHopt)^(DBHb-1))  *   exp(-(meas$DHP_CM/DBHopt)^DBHb)
  } else if (sizefun == "DBH"){
    meas$sizeE <- meas$DHP_CM
  }

  ####################################################
  ## Mixture effect
  ####################################################
  if (sp == "SAB"){
    if (mixturreE == "beta"){
      meas$mixE <- (meas$prop_SAB_BA^(C-1)) * ((1-meas$prop_SAB_BA)^(D-1))
    } else if (mixturreE == "prop"){
      meas$mixE <- meas$prop_SAB_BA
    }
  } else if (sp == "PET"){
    if (mixturreE == "beta"){
      meas$mixE <- (meas$prop_PET_BA^(C-1)) * ((1-meas$prop_PET_BA)^(D-1))
    } else if (mixturreE == "prop"){
      meas$mixE <- meas$prop_PET_BA
    }
  }

  ####################################################
  ## DC
  ####################################################
  # meas$DC <- meas$DCMAXmay_jun_jul
  meas$DC <- meas$DCMAXjun_jul_aug

  ####################################################
  ## Scaling variables using distribution in the present
  ####################################################
  # load unscaled parameters
  if (sp=="PET"){
    load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/meas_PET.rdata")
  } else if (sp=="SAB"){
    load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/meas_SAB.rdata")
  }

  # meas$sizeE <- scale(meas$sizeE)
  # meas$mixE <- scale(meas$mixE)
  # meas$BAtot_CM2HA <- scale(meas$BAtot_CM2HA)
  # meas$compethard <- scale(meas$compethard)
  # meas$competsoft <- scale(meas$competsoft)
  # meas$Tannual <- scale(meas$Tannual)
  # meas$Pannual <- scale(meas$Pannual)
  # meas$DC <- scale(meas$DC)

  meas$sizeE <- (meas$sizeE - mean(meas_scale$sizeE)) / sd(meas_scale$sizeE)
  meas$mixE <- (meas$mixE - mean(meas_scale$mixE)) / sd(meas_scale$mixE)
  meas$BAtot_CM2HA <- (meas$BAtot_CM2HA - mean(meas_scale$BAtot_CM2HA)) / sd(meas_scale$BAtot_CM2HA)
  meas$compethard <- (meas$compethard - mean(meas_scale$compethard)) / sd(meas_scale$compethard)
  meas$competsoft <- (meas$competsoft - mean(meas_scale$competsoft)) / sd(meas_scale$competsoft)
  meas$Tannual <- (meas$Tannual - mean(meas_scale$Tannual)) / sd(meas_scale$Tannual)
  meas$Pannual <- (meas$Pannual - mean(meas_scale$Pannual)) / sd(meas_scale$Pannual)
  meas$DC <- (meas$DC - mean(meas_scale$DC)) / sd(meas_scale$DC)
  meas$DCp <- (meas$DCp - mean(meas_scale$DCp)) / sd(meas_scale$DCp)
  return(meas)
}




run_mod <- function(speed=c("rapid", "lent")){
  # form <- lBAI_CM ~ sizeE + mixE + compethard + competsoft + DC + Tannual + Pannual + DD + mixE:BAtot_CM2HA + sizeE:DC + compethard:DC + competsoft:DC + mixE:DC + DD:sizeE + DD:mixE + DD:compethard + DD:competsoft + DD:DC + DD:DCp + DD:Tannual + DD:Pannual + (sizeE + sizeE:DC + mixE:DC + DC + DC:compethard + DC:competsoft |ID_PET_MES / ID_ARB)

    form <- lBAI_CM ~ sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture + drainage + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp  + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + texture:sizeE + texture:mixE + texture:compethard + texture:competsoft + texture:DC + texture:DCp + texture:Tannual + texture:Pannual + drainage:sizeE + drainage:mixE + drainage:compethard + drainage:competsoft + drainage:DC + drainage:DCp + drainage:Tannual + drainage:Pannual + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)

  if (speed == "rapid"){
    mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 500)))
  } else if (speed == "lent"){
    # mod <-  lmer(form, data=data, REML = FALSE)
    mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  }
  return(mod)
}


####################################################
## Model de base
####################################################

# run_mod <- function(speed=c("rapid", "lent")){
#     form <- lBAI_CM ~ sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture + drainage + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp  + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + texture:sizeE + texture:mixE + texture:compethard + texture:competsoft + texture:DC + texture:DCp + texture:Tannual + texture:Pannual + drainage:sizeE + drainage:mixE + drainage:compethard + drainage:competsoft + drainage:DC + drainage:DCp + drainage:Tannual + drainage:Pannual + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)
#
#   if (speed == "rapid"){
#     mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 500)))
#   } else if (speed == "lent"){
#     # mod <-  lmer(form, data=data, REML = FALSE)
#     mod <-  lmer(form, data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
#   }
#   return(mod)
# }


####################################################
## Model final
####################################################

run_mod_fin <- function(speed = c("rapid", "lent"), sp = c("PET", "SAB")){

  if (sp == "SAB"){
    form <- lBAI_CM ~ texture + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp + compethard:DC + competsoft:DC + competsoft:DCp + mixE:DCp + sizeE:texture + compethard:texture + competsoft:texture + DC:texture + DCp:texture + Tannual:texture + Pannual:texture + sizeE:drainage + compethard:drainage + competsoft:drainage + DC:drainage + DCp:drainage + Tannual:drainage + Pannual:drainage + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)
  } else if (sp == "PET"){
    form <- lBAI_CM ~ texture + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + sizeE:texture + mixE:texture + compethard:texture + competsoft:texture + DC:texture + DCp:texture + Tannual:texture + Pannual:texture + sizeE:drainage + mixE:drainage + compethard:drainage + DCp:drainage + Tannual:drainage + Pannual:drainage + (sizeE + DC + DCp |ID_PET_MES / ID_ARB)
  }

  if (speed == "rapid"){
    mod <- lmer(form, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 500)))
  } else if (speed == "lent"){
    # mod <-  lmer(form, data=data, REML = FALSE)
    mod <- lmer(form, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  }
  return(mod)
}
