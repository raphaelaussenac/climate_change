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
load("./QC_BAI_PET.rdata")
PET <- predictions
PET$sp <- "PET"
load("./QC_BAI_SAB.rdata")
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
# Covariance
####################################################
# function cov intrasp
covintra <- function(data = PET, period = "p1", rcpmod = rcpmod){
  data <- data[data$rcpmod == rcpmod,]
  covintra <- numeric()
  for (l in unique(data$variable)){
    for (k in unique(data$ID_PET_MES)){
      dataV <- data[data$period == period & data$ID_PET_MES == k & data$variable == l,]
      if (length(unique(dataV$ID_ARB))>1){
        # Calculate all combinations of individuals
        comb <- combn(unique(dataV$ID_ARB),2)
        for (j in 1:dim(comb)[2]){  # une mesure de cov pur chaque couple
          t1 <- dataV[dataV$ID_ARB == as.character(comb[1,j]), "BAI"]
          t2 <- dataV[dataV$ID_ARB == as.character(comb[2,j]), "BAI"]
          a <- cov(t1,t2)
          covintra <- c(covintra, a)
        }
      }
    }
  }
  return(covintra)
}

# function cov intersp
covinter <- function(PET = PET, SAB = SAB, period = "p1", rcpmod = rcpmod){
  PET <- PET[PET$rcpmod == rcpmod,]
  SAB <- SAB[SAB$rcpmod == rcpmod,]
  cov_mix <- numeric()
  for (l in unique(PET$variable)){
    a <- as.data.frame(unique(PET$ID_PET_MES))
    b <- as.data.frame(unique(SAB$ID_PET_MES))
    c <- merge(a, b, by.x = "unique(PET$ID_PET_MES)", by.y = "unique(SAB$ID_PET_MES)")
    for (k in unique(c[,1])){
      PETV <- PET[PET$variable == l & PET$period == period & PET$ID_PET_MES == k,]
      SABV <- SAB[SAB$variable == l & SAB$period == period & SAB$ID_PET_MES == k,]
      if (length(unique(PETV$ID_ARB))>=1){
        if (length(unique(SABV$ID_ARB))>=1){
          for (i in unique(PETV$ID_ARB)){
            for (j in unique(SABV$ID_ARB)){
              t1 <- PETV[PETV$ID_ARB == i, "BAI"]
              t2 <- SABV[SABV$ID_ARB == j, "BAI"]
              d <- cov(t1,t2)
              cov_mix <- c(cov_mix, d)
            }
          }
        }
      }
    }
  }
  return(cov_mix)
}


# We replicated the addresses nCores times.
registerDoParallel(32)

# iteration sur les 10 simulations (10 coeurs)
rcpmod <- unique(data$rcpmod)




# SAB/PET
# Species data
PET <- data[data$ESSENCE == "PET" & data$mix %in% c("MIX"),]
SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("MIX"),]
# p1
cov_mixp1 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covinter(PET = PET, SAB = SAB, period = "p1", rcpmod = rcpmod[i])
}
save(cov_mixp1, file = "./cov_mixp1.rdata")
# p2
cov_mixp2 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covinter(PET = PET, SAB = SAB, period = "p2", rcpmod = rcpmod[i])
}
save(cov_mixp2, file = "./cov_mixp2.rdata")



# PET/PET
# Species data
PET <- data[data$ESSENCE == "PET" & data$mix %in% c("PET"),]
# p1
cov_petp1 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p1", rcpmod = rcpmod[i])
}
save(cov_petp1, file = "./cov_petp1.rdata")
# p2
cov_petp2 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p2", rcpmod = rcpmod[i])
}
save(cov_petp2, file = "./cov_petp2.rdata")

# SAB/SAB
# Species data
SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("SAB"),]
# p1
cov_sabp1 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covintra(data = SAB, period = "p1", rcpmod = rcpmod[i])
}
save(cov_sabp1, file = "./cov_sabp1.rdata")
# p2
cov_sabp2 <- foreach(i = 1:length(rcpmod), .combine = 'rbind') %dopar% {
  covintra(data = SAB, period = "p2", rcpmod = rcpmod[i])
}
save(cov_sabp2, file = "./cov_sabp2.rdata")




#
#
#
# # p2
# cov_petp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
#   covintra(data = PET, period = "p2", variable = variable[i])
# }
# save(cov_petp2, file = "./cov_petp2.rdata")
#
#
# # SAB/SAB
# # Data PET and SAB (all trees of each species in PET/SAB/MIX)
# SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("PET", "SAB", "MIX"),]
# # p1
# cov_sabp1 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
#   covintra(data = SAB, period = "p1", variable = variable[i])
# }
# save(cov_sabp1, file = "./cov_sabp1.rdata")
# # p2
# cov_sabp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
#   covintra(data = SAB, period = "p2", variable = variable[i])
# }
# save(cov_sabp2, file = "./cov_sabp2.rdata")
#
#
# # SAB/PET
# # Data PET and SAB (all trees of each species in PET/SAB/MIX)
# PET <- data[data$ESSENCE == "PET" & data$mix %in% c("PET", "SAB", "MIX"),]
# SAB <- data[data$ESSENCE == "SAB" & data$mix %in% c("PET", "SAB", "MIX"),]
# # p1
# cov_mixp1 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
#   covinter(PET = PET, SAB = SAB, period = "p1", variable = variable[i])
# }
# save(cov_mixp1, file = "./cov_mixp1.rdata")
# # p2
# cov_mixp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
#   covinter(PET = PET, SAB = SAB, period = "p2", variable = variable[i])
# }
# save(cov_mixp2, file = "./cov_mixp2.rdata")
