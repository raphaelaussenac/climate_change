# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(plyr) # pour la fonction "ddply"
library(colorout)
library(plot3D)
library(ggplot2)
library(gridExtra)

# growth data
load(file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")

# number of sites
length(unique(data$ID_PET_MES))

# number of trees
length(unique(data$ID_ARB))

# DD
DD <- ddply(data, .(ID_ARB, ESSENCE), summarise, DD=unique(DD))
ggplot(data = DD)+
geom_histogram(aes(x = DD, fill = ESSENCE), stat = "count")
ggsave ("~/Desktop/DD.pdf")

table(DD$DD)

####################################################
## Drainage and Texture classes
####################################################
# delete classes 0, 7, 8, 9
data <- data[data$DD %in% c(1, 2, 3, 4, 5, 6), ]

# number of sites
length(unique(data$ID_PET_MES))

# number of trees
length(unique(data$ID_ARB))

# drainage
cl1 <- c(1, 2, 3)
cl2 <- c(4, 5, 6)
data$drainage <- 20
data[data$DD %in% cl1, "drainage"] <- 1
data[data$DD %in% cl2, "drainage"] <- 2
data$drainage <- as.factor(data$drainage)

# plot
drainage <- ddply(data, .(ID_ARB, ESSENCE), summarise, drainage=unique(drainage))
p1 <- ggplot(data = drainage)+
geom_histogram(aes(x = drainage, fill = ESSENCE), stat = "count")+
theme(legend.position="none")+
ggtitle("Absolute")
p2 <- ggplot(data = drainage, aes(drainage))+
geom_bar(aes(fill = ESSENCE), position = "fill")+
ggtitle("Relative")
grid.arrange(p1, p2, ncol=2)
quartz.save("~/Desktop/Dfinal.pdf", type = "pdf", device = dev.cur())


# texture
cl1 <- c(1, 4)
cl2 <- c(2, 5)
cl3 <- c(3, 6)
data$texture <- 20
data[data$DD %in% cl1, "texture"] <- 1
data[data$DD %in% cl2, "texture"] <- 2
data[data$DD %in% cl3, "texture"] <- 3
data$texture <- as.factor(data$texture)

# plot
texture <- ddply(data, .(ID_ARB, ESSENCE), summarise, texture=unique(texture))
p1 <- ggplot(data = texture)+
geom_histogram(aes(x = texture, fill = ESSENCE), stat = "count")+
theme(legend.position="none")+
ggtitle("Absolute")
p2 <- ggplot(data = texture, aes(texture))+
geom_bar(aes(fill = ESSENCE), position = "fill")+
ggtitle("Relative")
grid.arrange(p1, p2, ncol=2)
quartz.save("~/Desktop/Tfinal.pdf", type = "pdf", device = dev.cur())




######################### MIX
data$mix <- "RIEN"
data[data$prop_SAB_BA >= 0.75, "mix"] <- "SAB"
data[data$prop_PET_BA >= 0.75, "mix"] <- "PET"
data[data$prop_PET_BA < 0.75 & data$prop_SAB_BA < 0.75, "mix"] <- "MIX"
data$mix <- as.factor(data$mix)

# drainage
drainage <- ddply(data, .(ID_PET_MES, mix), summarise, drainage=unique(drainage))
p1 <- ggplot(data = drainage, groupe = mix)+
geom_histogram(aes(x = drainage, fill = mix), stat = "count")+
theme(legend.position="none")+
ggtitle("Absolute")
p2 <- ggplot(data = drainage, aes(drainage))+
geom_bar(aes(fill = mix), position = "fill")+
ggtitle("Relative")
grid.arrange(p1, p2, ncol=2)
quartz.save("~/Desktop/Dmixfinal.pdf", type = "pdf", device = dev.cur())

# texture
texture <- ddply(data, .(ID_PET_MES, mix), summarise, texture=unique(texture))
p1 <- ggplot(data = texture, groupe = mix)+
geom_histogram(aes(x = texture, fill = mix), stat = "count")+
theme(legend.position="none")+
ggtitle("Absolute")
p2 <- ggplot(data = texture, aes(texture))+
geom_bar(aes(fill = mix), position = "fill")+
ggtitle("Relative")
grid.arrange(p1, p2, ncol=2)
quartz.save("~/Desktop/Tmixfinal.pdf", type = "pdf", device = dev.cur())

####################################################
## save
####################################################

# save(data, file="~/owncloud/Work_directory/Analysis/chapitre_3/03_mixed_model/RUN_MODEL/dataBAI.rdata")


####################################################
## P & T
####################################################

ggplot(data, aes(Tannual, Pannual))+
geom_point(alpha = 0.1)+
stat_density2d(aes(fill = ESSENCE), geom="polygon", alpha = 0.2)+
geom_density2d(aes(colour = ESSENCE))+
guides(colour = "none")
ggsave ("~/Desktop/PTsp.pdf")

ggplot(data, aes(Tannual, Pannual))+
geom_point(alpha = 0.1)+
stat_density2d(aes(fill = mix), geom="polygon", alpha = 0.2)+
geom_density2d(aes(colour = mix))+
guides(colour = "none")
ggsave ("~/Desktop/PTmix.pdf")



####### plot3D
par(mfrow=c(2,2))
##  Create cuts:
x_c <- cut(data[data$ESSENCE == "SAB", "Tannual"], 50)
y_c <- cut(data[data$ESSENCE == "SAB", "Pannual"], 50)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z = z, border = "black", x.lab = "Tannual")
##  Plot as a 2D heatmap:
image2D(z=z, border="black")

##  Create cuts:
x_c <- cut(data[data$ESSENCE == "PET", "Tannual"], 50)
y_c <- cut(data[data$ESSENCE == "PET", "Pannual"], 50)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z = z, border = "black", x.lab = "Tannual")
##  Plot as a 2D heatmap:
image2D(z = z, border="black")



######

ggplot(data = data)+
# geom_histogram(aes(DHP_CM, fill = mix), position = "dodge", bins = 20)+
geom_density(aes(DHP_CM, fill = mix, colour = mix), alpha = 0.5)
