# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("plyr")
library("ggplot2")


####################################################
## data
####################################################
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
data <- data[, c("ID_PET_MES", "ID_ARB", "AN_CERNE", "BAI_CM", "ESSENCE", "mix")]


####################################################
## cov(SAB,SAB)
####################################################
# Select SAB in SAB stands
SAB <- data[data$ESSENCE == "SAB" & data$mix == "SAB", ]

cov_sab <- numeric()
for (i in unique(SAB$ID_PET_MES)){
  SABP <- SAB[SAB$ID_PET_MES == i,]
  if (length(unique(SABP$ID_ARB))>1){
    # Calculate all combinations of individuals
    comb <- combn(unique(SABP$ID_ARB),2)
    for (i in 1:dim(comb)[2]){
      t1 <- SABP[SABP$ID_ARB == comb[1,i], "BAI_CM"]
      t2 <- SABP[SABP$ID_ARB == comb[1,i], "BAI_CM"]
      a <- cov(t1,t2)
      cov_sab <- c(cov_sab, a)
    }
  }
}

####################################################
## cov(PET,PET)
####################################################
# Select PET in PET stands
PET <- data[data$ESSENCE == "PET" & data$mix == "PET", ]

cov_pet <- numeric()
for (i in unique(PET$ID_PET_MES)){
  PETP <- PET[PET$ID_PET_MES == i,]
  if (length(unique(PETP$ID_ARB))>1){
    # Calculate all combinations of individuals
    comb <- combn(unique(PETP$ID_ARB),2)
    for (i in 1:dim(comb)[2]){
      t1 <- PETP[PETP$ID_ARB == comb[1,i], "BAI_CM"]
      t2 <- PETP[PETP$ID_ARB == comb[1,i], "BAI_CM"]
      a <- cov(t1,t2)
      cov_pet <- c(cov_pet, a)
    }
  }
}

####################################################
## cov(SAB,PET)
####################################################
# Select PET & SAB in mix stands
PETmix <- data[data$ESSENCE == "PET" & data$mix == "MIX", ]
SABmix <- data[data$ESSENCE == "SAB" & data$mix == "MIX", ]

a <- as.data.frame(unique(PETmix$ID_PET_MES))
b <- as.data.frame(unique(SABmix$ID_PET_MES))
c <- merge(a, b, by.x = "unique(PETmix$ID_PET_MES)", by.y = "unique(SABmix$ID_PET_MES)")

cov_mix <- numeric()
for (i in unique(c[,1])){
  PETP <- PETmix[PETmix$ID_PET_MES == i,]
  SABP <- SABmix[SABmix$ID_PET_MES == i,]
  if (length(unique(PETP$ID_ARB))>=1){
    if (length(unique(SABP$ID_ARB))>=1){
      for (j in unique(PETP$ID_ARB)){
        for (k in unique(SABP$ID_ARB)){
          t1 <- PETP[PETP$ID_ARB == j, "BAI_CM"]
          t2 <- SABP[SABP$ID_ARB == k, "BAI_CM"]
          d <- cov(t1,t2)
          cov_mix <- c(cov_mix, d)
        }
      }
    }
  }
}


####################################################
## plot
####################################################

boxplot(cov_sab, cov_pet, cov_mix, names = c("fir", "aspen", "mixed"), ylim = c(-20,150))



tabSAB <- as.data.frame(cov_sab)
colnames(tabSAB) <- "cov"
tabSAB$compo <- "SAB"

tabPET <- as.data.frame(cov_pet)
colnames(tabPET) <- "cov"
tabPET$compo <- "PET"

tabMIX <- as.data.frame(cov_mix)
colnames(tabMIX) <- "cov"
tabMIX$compo <- "MIX"

tab <- rbind(tabSAB, tabPET, tabMIX)


summary(lm(cov ~ compo, data = tab))
