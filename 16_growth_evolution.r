# Delete all objects in the work space
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)


allinone <- function(PET = PET, SAB = SAB, soil = soil){

  PET$sp <- "PET"
  SAB$sp <- "SAB"
  data <- rbind(PET, SAB)

  ####################################################
  # Define mixture
  ####################################################
  data$plot <- 1
  data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "plot"] <- "MIX"
  data[data$prop_PET_BA >= 0.75, "plot"] <- "PET"
  data[data$prop_SAB_BA >= 0.75, "plot"] <- "SAB"

  ####################################################
  # wide to long
  ####################################################

  data <- melt(data, id.vars = c("ID_PET_MES", "ID_ARB", "ESSENCE", "yr", "rcp", "mod", "plot"), measure.vars = paste("V", seq(1,10), sep=""))

  # change names
  data[data$rcp == "rcp45", "rcp"] <- "RCP4.5"
  data[data$rcp == "rcp85", "rcp"] <- "RCP8.5"
  colnames(data)[colnames(data) == "variable"] <- "sim"
  colnames(data)[colnames(data) == "value"] <- "BAI"

  ####################################################
  # Plot chronology over the 1950-2100 period
  ####################################################

  # sum of BAI for each year/rcp/mod/sim/mix
  pred <- ddply(data, .(yr, rcp, mod, sim, plot), summarise, BAI = sum(BAI))
  predall <- ddply(data, .(yr, rcp, mod, sim), summarise, BAI = sum(BAI))
  predall$plot <- "all"
  predall <- predall[, c("yr", "rcp", "mod", "sim", "plot", "BAI")]
  pred <- rbind(pred, predall)

  ################
  # min, max, CI
  ################
  minmax <- function(data = pred, plot = c("all", "MIX", "PET", "SAB")){
    data <- data[data$plot == plot,]
    data_min <- ddply(data, .(yr, rcp), summarise, BAImin = min(BAI))
    data_max <- ddply(data, .(yr, rcp), summarise, BAImax = max(BAI))
    data_CImin <- ddply(data, .(yr, rcp), summarise, CImin = wilcox.test(BAI,conf.int=TRUE)$conf.int[1])
    data_CImax <- ddply(data, .(yr, rcp), summarise, CImax = wilcox.test(BAI,conf.int=TRUE)$conf.int[2])
    chrono <- cbind(data_min, data_max$BAImax, data_CImin$CImin, data_CImax$CImax)
    chrono$plot <- plot
    colnames(chrono) <- c("yr", "rcp", "BAImin", "BAImax", "CImin", "CImax", "plot")
    return(chrono)
  }

  all <- minmax(pred, "all")
  MIX <- minmax(pred, "MIX")
  PET <- minmax(pred, "PET")
  SAB <- minmax(pred, "SAB")

  chronoplot <- rbind(all, MIX, PET, SAB)

  ################
  # plot
  ################

  chronoplot$soil <- soil
  save(chronoplot, file = paste("chronoplot", soil, ".rdata", sep = ""))

  ####################################################
  # Plot SP chronology over the 1950-2100 period
  ####################################################

  # sum of BAI for each year/rcp/mod/sim/mix
  predPET <- ddply(data[data$ESSENCE == "PET" & data$plot %in% c("MIX", "PET"), ], .(yr, rcp, mod, sim, plot), summarise, BAI = sum(BAI))
  predPET$ESSENCE <- "PET"
  predSAB <- ddply(data[data$ESSENCE == "SAB" & data$plot %in% c("MIX", "SAB"), ], .(yr, rcp, mod, sim, plot), summarise, BAI = sum(BAI))
  predSAB$ESSENCE <- "SAB"
  pred <- rbind(predPET, predSAB)

  ################
  # min, max, CI
  ################

  petp <- minmax(pred[pred$ESSENCE == "PET", ], "PET")
  petp$plot <- "PET"
  petp$ESSENCE <- "PET"
  petm <- minmax(pred[pred$ESSENCE == "PET", ], "MIX")
  petm$plot <- "MIX"
  petm$ESSENCE <- "PET"
  sabp <- minmax(pred[pred$ESSENCE == "SAB", ], "SAB")
  sabp$plot <- "SAB"
  sabp$ESSENCE <- "SAB"
  sabm <- minmax(pred[pred$ESSENCE == "SAB", ], "MIX")
  sabm$plot <- "MIX"
  sabm$ESSENCE <- "SAB"
  chronosp <- rbind(petp, petm, sabp, sabm)

  ################
  # plot
  ################

  chronosp$a <- "a"
  chronosp[chronosp$ESSENCE == "PET" & chronosp$plot == "MIX", "a"] <- "a) aspen in mixed stands"
  chronosp[chronosp$ESSENCE == "PET" & chronosp$plot == "PET", "a"] <- "b) aspen in pure stands"
  chronosp[chronosp$ESSENCE == "SAB" & chronosp$plot == "MIX", "a"] <- "c) fir in mixed stands"
  chronosp[chronosp$ESSENCE == "SAB" & chronosp$plot == "SAB", "a"] <- "d) fir in pure stands"

  chronosp$soil <- soil
  save(chronosp, file = paste("chronosp", soil, ".rdata", sep = ""))

  ####################################################
  # difference between Mixed and pure
  ####################################################

  # difference between MIX and PET
  diffpet <- pred[pred$ESSENCE == "PET", ]
  diffpet <- diffpet[,-ncol(pred)]
  diffpet <- dcast(diffpet[diffpet$plot %in% c("MIX", "PET"), ], yr + rcp + mod + sim ~ plot)
  diffpet$BAI <- diffpet$MIX - diffpet$PET
  diffpet$sp <- "PET"
  diffpet <- diffpet[, c("yr", "rcp", "mod", "sim", "BAI", "sp")]

  # difference between MIX and SAB
  diffsab <- pred[pred$ESSENCE == "SAB", ]
  diffsab <- diffsab[,-ncol(pred)]
  diffsab <- dcast(diffsab[diffsab$plot %in% c("MIX", "SAB"), ], yr + rcp + mod + sim ~ plot)
  diffsab$BAI <- diffsab$MIX - diffsab$SAB
  diffsab$sp <- "SAB"
  diffsab <- diffsab[, c("yr", "rcp", "mod", "sim", "BAI", "sp")]

  ################
  # min, max, CI
  ################
  # MIX PET
  colnames(diffpet)[colnames(diffpet) == "sp"] <- "plot"
  diffpet <- minmax(diffpet, "PET")
  # MIX SAB
  colnames(diffsab)[colnames(diffsab) == "sp"] <- "plot"
  diffsab <- minmax(diffsab, "SAB")
  # gather
  diff <- rbind(diffpet, diffsab)
  diff$soil <- soil
  # save
  save(diff, file = paste("diffsp", soil, ".rdata", sep = ""))

}

####################################################
# Data
####################################################

# File list
setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output")
PETs <- Sys.glob("QC_BAI_PET*")
SABs <- Sys.glob("QC_BAI_SAB*")


for (i in 1:length(PETs)){
  # import and merge predictions
  load(PETs[i])
  PET <- predictions
  load(SABs[i])
  SAB <- predictions
  soil <- substr(PETs[i], 12, 15)
  allinone(PET = PET, SAB = SAB, soil = soil)
}
