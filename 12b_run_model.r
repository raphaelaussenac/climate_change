# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Model PET
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
source("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/climate_change/11_fun_model.r")
sp <- "PET"
data <- prepare_mod_data(sp = sp)
speed <- "lent" # rapid/lent

mod <- run_mod_fin(speed = speed, sp = sp)
if (speed=="rapid"){
  save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_PET_rapid_fin.rdata")
} else if (speed=="lent"){
  save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_PET_lent_fin.rdata")
}

####################################################
## Model SAB
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
source("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/climate_change/11_fun_model.r")
sp <- "SAB"
data <- prepare_mod_data(sp = sp)
speed <- "rapid" # rapid/lent

mod <- run_mod_fin(speed = speed, sp = sp)
if (speed=="rapid"){
  save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_SAB_rapid_fin.rdata")
} else if (speed=="lent"){
  save(mod, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/mod_SAB_lent_fin.rdata")
}
