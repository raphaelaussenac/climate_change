# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("plyr")
library("ggplot2")


####################################################
## data
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
data <- data[, c("ID_ARB", "AN_CERNE", "BAI_CM", "ESSENCE", "mix")]


####################################################
## cov(SAB,SAB)
####################################################
# Select SAB in SAB stands
SAB <- data[data$ESSENCE == "SAB" & data$mix == "SAB", ]

# Calculate all combinations of individuals
comb <- combn(unique(SAB$ID_ARB),2)

cov_sab <- numeric()
for (i in 1:dim(comb)[2]){
  t1 <- SAB[SAB$ID_ARB == comb[1,i], "BAI_CM"]
  t2 <- SAB[SAB$ID_ARB == comb[1,i], "BAI_CM"]
  a <- cov(t1,t2)
  cov_sab <- c(cov_sab, a)
}

####################################################
## cov(PET,PET)
####################################################
# Select PET in PET stands
PET <- data[data$ESSENCE == "PET" & data$mix == "PET", ]

# Calculate all combinations of individuals
comb <- combn(unique(PET$ID_ARB),2)

cov_pet <- numeric()
for (i in 1:dim(comb)[2]){
  t1 <- PET[PET$ID_ARB == comb[1,i], "BAI_CM"]
  t2 <- PET[PET$ID_ARB == comb[1,i], "BAI_CM"]
  a <- cov(t1,t2)
  cov_pet <- c(cov_pet, a)
}

####################################################
## cov(SAB,PET) pure
####################################################


cov_pure <- numeric()
for (i in unique(PET$ID_ARB)){
  for (j in unique(SAB$ID_ARB)){
    t1 <- PET[PET$ID_ARB == i, "BAI_CM"]
    t2 <- SAB[SAB$ID_ARB == j, "BAI_CM"]
    a <- cov(t1,t2)
    cov_pure <- c(cov_pure, a)
  }
}

####################################################
## cov(SAB,PET) mix
####################################################
# Select PET & SAB in mix stands
PETmix <- data[data$ESSENCE == "PET" & data$mix == "MIX", ]
SABmix <- data[data$ESSENCE == "SAB" & data$mix == "MIX", ]

cov_mix <- numeric()
for (i in unique(PETmix$ID_ARB)){
  for (j in unique(SABmix$ID_ARB)){
    t1 <- PETmix[PETmix$ID_ARB == i, "BAI_CM"]
    t2 <- SABmix[SABmix$ID_ARB == j, "BAI_CM"]
    a <- cov(t1,t2)
    cov_mix <- c(cov_mix, a)
  }
}

####################################################
## plot
####################################################

boxplot(cov_sab, cov_pet, cov_pure, cov_mix, names = c("cov_sab", "cov_pet", "cov_pure", "cov_mix"))
