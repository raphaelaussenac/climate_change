# Delete all objects in the work space
rm(list=ls(all=TRUE))
library("ggplot2")
library("plyr")
library("reshape")

####################################################
# Data
####################################################
# import texture and drainage
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")
dr <- ddply(data, .(ID_PET_MES), summarise, drainage = unique(drainage))
te <- ddply(data, .(ID_PET_MES), summarise, texture = unique(texture))
drte <- merge(dr, te, by = "ID_PET_MES")

# import and merge predictions
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_PET.rdata")
PET <- predictions
PET$sp <- "PET"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_SAB.rdata")
SAB <- predictions
SAB$sp <- "SAB"
data <- rbind(PET, SAB)

####################################################
# Define the 2 periods and mixtures
####################################################
# period
data$period <- "p"
data[data$yr >= 1986 & data$yr <= 2005, "period"] <- "p1"
data[data$yr >= 2081 & data$yr <= 2100, "period"] <- "p2"
data <- data[data$period != "p",]

# class
data$period <- factor(data$period, levels = c("p1", "p2", "p"), ordered = FALSE)
data$rcp <- as.factor(data$rcp)
data$rcpmod <- as.factor(data$rcpmod)
data$yr <- as.numeric(data$yr)

# mixtures
data$compo <- 1
data[data$prop_PET_BA >= 0.75, "compo"] <- "PET"
data[data$prop_SAB_BA >= 0.75, "compo"] <- "SAB"
data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "compo"] <- "MIX"

####################################################
#
####################################################

tree_bai <- data.frame(ID_ARB = NA, ID_PET_MES = NA, mean = NA, var = NA, rcp = NA, period = NA, ESSENCE = NA, compo = NA)
for (i in unique(data$rcp)){
  datarcp <- data[data$rcp == i,]

  for (j in unique(datarcp$period)){
    datarcpp <- datarcp[datarcp$period == j,]

    for (k in unique(datarcpp$ESSENCE)){
      datarcppsp <- datarcpp[datarcpp$ESSENCE == k,]

      for (l in unique(datarcppsp$compo)){
        datarcppspcompo <- datarcppsp[datarcppsp$compo == l,]

        # all (150) growth measures per individual for each year
        essai <- melt(datarcppspcompo, id.vars = c("ID_PET_MES", "ID_ARB", "yr"), measure.vars = paste("V", seq(1,10), sep=""))

        # calculate the median growth for each tree and for each year
        # = calculate the median chronology for each tree
        med_arb_yr <- ddply(essai, .(ID_PET_MES, ID_ARB, yr), summarise, BAI = median(value))
        # plot(med_arb_yr[med_arb_yr$ID_ARB == med_arb_yr$ID_ARB[1], "yr"], med_arb_yr[med_arb_yr$ID_ARB == med_arb_yr$ID_ARB[1], "BAI"], type = "l")

        # calculate the mean growth for each tree on the complete period
        mean_arb <- ddply(essai, .(ID_PET_MES, ID_ARB), summarise, mean = mean(value))
        # calculate the var of growth for each tree on the complete period
        var_arb <- ddply(essai, .(ID_PET_MES, ID_ARB), summarise, var = var(value))

        tmp <- merge(mean_arb, var_arb[,c("ID_ARB", "var")], by = "ID_ARB")
        tmp$rcp <- i
        tmp$period <- j
        tmp$ESSENCE <- k
        tmp$compo <- l
        tree_bai <- rbind(tree_bai, tmp)

        # for (m in 1:it_boot){
        #   tmp <- tree(data = med_arb)
        #   tmp <- as.data.frame(tmp)
        #   tmp$rcp <- i
        #   tmp$period <- j
        #   tmp$ESSENCE <- k
        #   tmp$compo <- l
        #   tree_bai <- rbind(tree_bai, tmp)
        # }
      }
    }
  }
}
tree_bai <- tree_bai[-1,]

# add drainage and texture
tree_bai <- merge(tree_bai, drte, by = "ID_PET_MES")

####################################################
# plot
####################################################

plotdiv <- function(data = tree_bai, rcp = "rcp45", sp = "SAB", variable = "mean", texture = FALSE, drainage = FALSE){
  colnames(data)[colnames(data) == variable] <- "variable"
  if (texture == FALSE & drainage == FALSE){
    ggplot(data[data$rcp == rcp & data$ESSENCE == sp & data$compo %in% c("MIX", sp),], aes(compo, variable))+
    geom_boxplot()+
    facet_grid(~ period, scales="fixed", space="free")+
    theme_bw()+
    ggtitle(paste(variable, sp, rcp, sep = " "))
  } else if (texture == TRUE & drainage == FALSE){
    ggplot(data[data$rcp == rcp & data$ESSENCE == sp & data$compo %in% c("MIX", sp),], aes(compo, variable))+
    geom_boxplot()+
    facet_grid(texture ~ period, scales="fixed", space="free")+
    theme_bw()+
    ggtitle(paste(variable, sp, rcp, sep = " "))
  } else if (texture == FALSE & drainage == TRUE){
    ggplot(data[data$rcp == rcp & data$ESSENCE == sp & data$compo %in% c("MIX", sp),], aes(compo, variable))+
    geom_boxplot()+
    facet_grid(drainage ~ period, scales="fixed", space="free")+
    theme_bw()+
    ggtitle(paste(variable, sp, rcp, sep = " "))
  }
}

# mean
# SAB
plotdiv(tree_bai, rcp = "rcp45", sp = "SAB", variable = "mean", texture = FALSE, drainage = FALSE)
plotdiv(tree_bai, rcp = "rcp85", sp = "SAB", variable = "mean", texture = FALSE, drainage = FALSE)
# PET
plotdiv(tree_bai, rcp = "rcp45", sp = "PET", variable = "mean", texture = FALSE, drainage = FALSE)
plotdiv(tree_bai, rcp = "rcp85", sp = "PET", variable = "mean", texture = FALSE, drainage = FALSE)

# var
# SAB
plotdiv(tree_bai, rcp = "rcp45", sp = "SAB", variable = "var", texture = FALSE, drainage = FALSE)
plotdiv(tree_bai, rcp = "rcp85", sp = "SAB", variable = "var", texture = FALSE, drainage = FALSE)
# var
# PET
plotdiv(tree_bai, rcp = "rcp45", sp = "PET", variable = "var", texture = FALSE, drainage = FALSE)
plotdiv(tree_bai, rcp = "rcp85", sp = "PET", variable = "var", texture = FALSE, drainage = FALSE)
