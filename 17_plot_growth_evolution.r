# Delete all objects in the work space
rm(list=ls(all=TRUE))
# library
library(ggplot2)


####################################################
# Data
####################################################
# File list
setwd("~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/output")

####################################################
# plot plot
####################################################
load("chronoplotT0D0.rdata")

ggplot(data = chronoplot)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(~ plot, nrow = 1, scales="free_y",  labeller = as_labeller(c("all" = "a) all stands", "MIX" = "b) mixed stands", "PET" = "c) pure aspen stands", "SAB" = "d) pure fir stands")))+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave (paste("~/Desktop/chap3/plot/plot", ".pdf", sep = ""), width = 8, height= 5)

####################################################
# plot sp
####################################################
load("chronospT0D0.rdata")

ggplot(data = chronosp)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(~ a, nrow = 1, scales="free_y")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave (paste("~/Desktop/chap3/plot/sp", ".pdf", sep = ""), width = 8, height= 5)

####################################################
# All plots
####################################################
# File list
multiplot <- Sys.glob("chronoplot*")

load(multiplot[1])
chronoplot1 <- chronoplot
for (i in 2:length(multiplot)){
  load(multiplot[i])
  chronoplot1 <- rbind(chronoplot1, chronoplot)
}

# plot
ggplot(data = chronoplot1[chronoplot1$plot %in% c("PET", "SAB") & chronoplot1$soil != "T0D0",])+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(soil ~ plot, scales = "free", ncol = 2)+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave ("~/Desktop/chap3/plot/allplot.pdf", width = 8, height= 15)

####################################################
# All sp
####################################################
# File list
multisp <- Sys.glob("chronosp*")

load(multisp[1])
chronosp1 <- chronosp
for (i in 2:length(multisp)){
  load(multisp[i])
  chronosp1 <- rbind(chronosp1, chronosp)
}

# plot
ggplot(data = chronosp1[chronosp1$soil != "T0D0",])+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(soil ~ a, scales = "free", ncol = 4)+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave ("~/Desktop/chap3/plot/allsp.pdf", width = 8, height= 15)

####################################################
# Diff
####################################################
# File list
multidiff <- Sys.glob("diffsp*")

load(multidiff[1])
diff1 <- diff
for (i in 2:length(multidiff)){
  load(multidiff[i])
  diff1 <- rbind(diff1, diff)
}

# plot
ggplot(data = diff1)+
geom_ribbon(aes(x=yr, ymax=BAImax, ymin=BAImin, fill = rcp), alpha = 0.2)+
geom_ribbon(aes(x=yr, ymax=CImax, ymin=CImin, fill = rcp), alpha = 0.5)+
xlab("year")+
ylab("total BAI")+
facet_wrap(soil ~ plot, scales = "free", ncol = 2)+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
ggsave ("~/Desktop/chap3/plot/diffsp.pdf", width = 8, height= 15)
