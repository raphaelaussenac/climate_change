# Delete all objects in the work space
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(reshape2)

####################################################
# Data
####################################################
# import and merge predictions
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_PET.rdata")
PET <- predictions
PET$sp <- "PET"
load("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output/QC_BAI_SAB.rdata")
SAB <- predictions
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

# sum of BAI for each year/rcp/mod/sim/mix
predall <- ddply(data, .(yr, rcp, mod, variable), summarise, BAI = sum(value))
predall$plot <- "all"
predall <- predall[, c("yr", "rcp", "mod", "variable", "plot", "BAI")]
pred <- ddply(data, .(yr, rcp, mod, variable, plot), summarise, BAI = sum(value))
pred <- rbind(pred, predall)

pred <- dcast(pred, yr + rcp + mod + variable ~ plot)

####################################################
# TS
####################################################
tw <- 20 # temporal window

d <- data.frame(yr = NA, rcp = NA, mod = NA, variable = NA, mix = NA, mu = NA, var = NA, ts = NA)
for (i in seq(min(pred$yr), max(pred$yr)-tw)){
  for (j in unique(pred$rcp)){
    a <- pred[pred$yr %in% seq(i, i+tw) & pred$rcp == j, ]
    for (k in unique(pred$mod)){
      b <- a[a$mod == k, ]
      for (l in unique(pred$variable)){
        c <- b[b$variable == l, ]
        for (m in c("all", "MIX", "PET", "SAB")){
          mu <- mean(c[,m])
          va <- var(c[,m])
          ts <- mu/va
          e <- data.frame(yr = (i+i+tw)/2, rcp = j, mod = k, variable = l, mix = m, mu = mu, var = va, ts = ts)
          d <- rbind(d,e)

        }
      }
    }
  }
}
d <- d[-1,]

# save(d, file="~/Desktop/d.rdata")
load(file="~/Desktop/d.rdata")


################
# min, max, CI
################
d <- d[!is.na(d$ts),]

minmax <- function(compo = "MIX", V = "ts"){
  a <- d[d$mix == compo,]
  colnames(a)[colnames(a) == V] <- "V"

  d_min <- ddply(a, .(yr, rcp), summarise, diffmin = min(V))
  d_max <- ddply(a, .(yr, rcp), summarise, diffmax = max(V))
  d_CImin <- ddply(a, .(yr, rcp), summarise, CImin = wilcox.test(V,conf.int=TRUE)$conf.int[1])
  d_CImax <- ddply(a, .(yr, rcp), summarise, CImax = wilcox.test(V,conf.int=TRUE)$conf.int[2])

  d_min$collage <- paste(d_min$yr, d_min$rcp, sep = "")
  d_max$collage <- paste(d_max$yr, d_max$rcp, sep = "")
  d_CImin$collage <- paste(d_max$yr, d_max$rcp, sep = "")
  d_CImax$collage <- paste(d_max$yr, d_max$rcp, sep = "")

  b <- merge(d_min, d_max[, c("diffmax", "collage")], by = "collage")
  b <- merge(b, d_CImin[, c("CImin", "collage")], by = "collage")
  b <- merge(b, d_CImax[, c("CImax", "collage")], by = "collage")

  b$compo <- compo
  b$par <- V

  return(b)

}

aa <- minmax(compo = "MIX", V = "ts")

c <- data.frame(collage = NA, yr = NA, rcp = NA, diffmin = NA, diffmax = NA, CImin = NA, CImax = NA, compo = NA, par = NA)
for (i in c("all", "MIX", "PET", "SAB")){
  for (j in c("mu", "var", "ts")){
    aa <- minmax(compo = i, V = j)
    c <- rbind(c, aa)
  }
}

c <- c[-1,]

################
# plot
################

c[c$compo == "all", "compo"] <- "a) all stands"
c[c$compo == "MIX", "compo"] <- "b) mixed stands"
c[c$compo == "PET", "compo"] <- "c) pure aspen stands"
c[c$compo == "SAB", "compo"] <- "d) pure fir stands"

ggplot(data = c[c$par == "var", ])+
geom_ribbon(aes(x=yr, ymax=diffmax, ymin=diffmin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
facet_wrap( ~ compo, scale = "free", nrow = 1)+
xlab("year")+
ylab("variance of total BAI")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())

ggsave("~/Desktop/var.pdf", width = 8, height = 5)
