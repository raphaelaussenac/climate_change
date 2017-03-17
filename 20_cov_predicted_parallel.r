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
covintra <- function(data = PET, period = "p1", variable = variable){
  # Calculate all combinations of individuals
  comb <- combn(unique(PET$ID_ARB),2)
  covintra <- numeric()
    dataV <- PET[PET$variable == variable & PET$period == period,]
    for (j in 1:dim(comb)[2]){  # une mesure de cov pur chaque couple
      t1 <- dataV[dataV$ID_ARB == as.character(comb[1,j]), "BAI"]
      t2 <- dataV[dataV$ID_ARB == as.character(comb[2,j]), "BAI"]
      a <- cov(t1,t2)
      covintra <- c(covintra, a)
    }
  return(covintra)
}

# function cov intersp
covinter <- function(PET = PET, SAB = SAB, period = "p1", variable = variable){
  cov_mix <- numeric()
    PETV <- PET[PET$variable == variable & PET$period == period,]
    SABV <- SAB[SAB$variable == variable & SAB$period == period,]
    for (i in unique(PETV$ID_ARB)){
      for (j in unique(SABV$ID_ARB)){
        t1 <- PETV[PETV$ID_ARB == i, "BAI"]
        t2 <- SABV[SABV$ID_ARB == j, "BAI"]
        a <- cov(t1,t2)
        cov_mix <- c(cov_mix, a)
      }
    }
  return(cov_mix)
}




# Data PET and SAB (all trees of each species in PET/SAB/MIX)
PET <- data[data$ESSENCE == "PET", ]
SAB <- data[data$ESSENCE == "SAB", ]


# We replicated the addresses nCores times.
registerDoParallel(30)

# iteration sur les 10 simulations (10 coeurs)
variable <- unique(data$variable)


# PET/PET
# p1
cov_petp1 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p1", variable = variable[i])
}
save(cov_petp1, file = "./cov_petp1.rdata")
# p2
cov_petp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p2", variable = variable[i])
}
save(cov_petp2, file = "./cov_petp2.rdata")

# SAB/SAB
# p1
cov_sabp1 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p1", variable = variable[i])
}
save(cov_sabp1, file = "./cov_sabp1.rdata")
# p2
cov_sabp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covintra(data = PET, period = "p2", variable = variable[i])
}
save(cov_sabp2, file = "./cov_sabp2.rdata")


# SAB/PET
# p1
cov_mixp1 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covinter(data = PET, period = "p1", variable = variable[i])
}
save(cov_mixp1, file = "./cov_mixp1.rdata")
# p2
cov_mixp2 <- foreach(i = 1:length(variable), .combine = 'rbind') %dopar% {
  covinter(data = PET, period = "p2", variable = variable[i])
}
save(cov_mixp2, file = "./cov_mixp2.rdata")
