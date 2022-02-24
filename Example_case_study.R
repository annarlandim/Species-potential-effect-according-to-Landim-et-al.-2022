########################################################################################

library(ks)
library(ggpubr)
library(gridExtra)

########################################################################################

# Using real data with the 'ks' package
# For more details on the 'ks' package: https://cran.r-project.org/web/packages/ks/ks.pdf

# Creating the resource community functional space:

Hpi.Cr <- Hpi(x = dataframewithCrtraits)
Cr_kde <- kde(x = dataframewithCrtraits, H = Hpi.Cr, gridsize = c(n1, n2, n3), xmin = c(0,0,0), xmax = c(maxvalueoftrait1, maxvalueoftrait2, maxvalueoftrait3), binned = FALSE)

# Correcting for integral = 1
Cr_corr <- Cr_kde$estimate / (sum(Cr_kde$estimate) * (max(dataframewithCrtraits[,1])/length(Cr_kde$eval.points[[1]]) * max(dataframewithCrtraits[,2])/length(Cr_kde$eval.points[[2]]) * max(dataframewithCrtraits[,3])/length(Cr_kde$eval.points[[3]])))

# n1, n2 and n3 are the number of times the trait probability density will be split to estimate the integral.

## Function to obtain:

### Kernel density estimation: Focal species' kde [[1]]
### Sp_corr: TPDs estimation  corrected to integral = 1 [[2]]
### Volume: The volume to calculate the integral [[3]]

### ATENTION: In this function, the functional trait space is composed by three traits.
### If you want to add or remove traits you will have to do it manually. For two traits
### you need to remove n3 from the function, as well as other things related to it,
### for example, in gridsize, remove n3, in xmin remove one 0, in xmax remove max(Cr[,3])
### and so on. 

### I use the coordinates of the resource's community functional space for the volume 
### because it dictates which will be the maximum values of each trait, whereas in a 
### defaunated community the functional space of the consumers community might not have
### the maximum values of some or all traits.

Kdes <- function(Sp, Cr, n1, n2, n3){
  
  # Sp should be a data frame with the traits in the columns and trait values of the focal species along the lines
  # Cr should be a data frame with the traits in the columns and trait values of the resource community along the lines
  # n1, n2 and n3 are the number of times the trait probability density will be split to estimate the integral.
  # they have to be they must be chosen so that the integral is as close as possible to 1 
  
  # Building the focal species' TPDs
  
  Hpi.Sp <- Hpi(x = Sp)
  kde.Sp <- kde(x = Sp, H = Hpi.Sp, gridsize = c(n1,n2,n3), xmin = c(0,0,0), xmax = c(max(Cr[,1]), max(Cr[,2]), max(Cr[,3])), binned = FALSE)
  
  # The integral 
  int_Sp <- sum(kde.G1$estimate) * ((max(Cr[,1])/n1)
                                    * (max(Cr[,2])/n2)
                                    * (max(Cr[,3])/n3))
  
  # Correcting for integral = 1 
  
  Sp_corr <- kde.Sp$estimate / int_Sp
  
  
  volume <- ((max(Cr[,1])/n1) * (max(Cr[,2])/n2) * (max(Cr[,3])/n3))
  
  
  list(kde.Sp, Sp_corr, volume)
  
}

# Consumers community functional space:

Cc <- (Sp1_corr * relative quantitative contribution) + (Sp2_corr * relative quantitative contribution) +...+(Spn_corr * relative quantitative contribution)

# Function to estimate the species potential effect for different values of theta

# With the quantitative contribution

pe.qt <- function(Vic, Vc, Sp_corr, Cc, Cr_corr, volume, thetamax){
  
  # Vic is the quantitative contribution of the focal species i
  # Vc is the total quantitative contribution of the consumers community
  # thetamax is the maximum value of theta that you want to use to estimate the potential effect
  
  Cc.2 <- Cc[-(which(Cc == 0))] 
  Sp_corr.2 <- Sp_corr[-(which(Cc == 0))]
  Cr_corr.2 <- Cr_corr[-(which(Cc == 0))]
  
  Pe <- vector()
  
  for(i in 1:thetamax){
    
    Pe[i] <- (sum(((Sp_corr.2 * (Qt_Sp/Qt_Cc)/Cc.2)^i) * Cr_corr.2) * volume)^(1/i)
    
    
  }
  
  Pe
}

# Without the quantitative contribution

pe <- function(Sp_corr, Cc, Cr_corr, volume, thetamax){
  
  # thetamax is the maximum value of theta that you want to use to estimate the potential effect
  
  Cc.2 <- Cc[-(which(Cc == 0))] 
  Sp_corr.2 <- Sp_corr[-(which(Cc == 0))]
  Cr_corr.2 <- Cr_corr[-(which(Cc == 0))]
  
  Pe <- vector()
  
  for(i in 1:comprimento){
    
    Pe[i] <- (sum(((Sp_corr.2 /Cc.2)^i) * Cr_corr.2) * volume)^(1/i)
    
    
  }
  
  Pe
}

Pe_sp1 <- pe((Sp1_corr), Cc, Cr_corr, (max(dataframewithCrtraits[,1])/length(Cr_kde$eval.points[[1]]) * max(dataframewithCrtraits[,2])/length(Cr_kde$eval.points[[2]]) * max(dataframewithCrtraits[,3])/length(Cr_kde$eval.points[[3]])), 20)
Pe_sp2 <- pe((Sp2_corr), Cc, Cr_corr, (max(dataframewithCrtraits[,1])/length(Cr_kde$eval.points[[1]]) * max(dataframewithCrtraits[,2])/length(Cr_kde$eval.points[[2]]) * max(dataframewithCrtraits[,3])/length(Cr_kde$eval.points[[3]])), 20)
Pe_spn <- pe((Spn_corr), Cc, Cr_corr, (max(dataframewithCrtraits[,1])/length(Cr_kde$eval.points[[1]]) * max(dataframewithCrtraits[,2])/length(Cr_kde$eval.points[[2]]) * max(dataframewithCrtraits[,3])/length(Cr_kde$eval.points[[3]])), 20)

Pe_Cc <- as.data.frame(c(Pe_sp1, Pe_sp2, Pe_spn))
Pe_Cc$Species <- c(rep("Sp1", times = 20), rep("Sp2", times = 20), rep("Spn", times = 20))
Pe_Cc$Theta <- rep(c(1:20), times = n)
str(Pe_Cc)
colnames(Pe_Cc)[1] <- "Potential effect"
Pe_Cc$Species <- as.factor(Pe_Cc$Species)
Pe_Cc$Theta <- as.numeric(Pe_Cc$Theta)

ggplot(Pe_Cc, aes(x = Theta, y = `Potential effect`), fill = Species) +
  geom_line(aes(color = Species), size=1) +
  xlab("θ") +
  ylab("Potential effect") +
  scale_x_continuous(limits = c(1, 20), breaks = c(1, 5, 10, 15, 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA))

#############################################################################################

# Case Study

# 0 - Pre analisys --------------------------------------------------------

## Table with the traits of plants and frugivores of Tijuca National Park 

# Plants
plnts <- read.csv("PlantTraitsPNT.csv")
plnts <- na.omit(plnts)
pnt.plnts <- plnts[, c(4,5,7)]

# Animals

nicho.reintro <- read.csv("NichoPNT reintro.csv")
nicho.reintro.na <- nicho.reintro[,c(2,3,5,6,8)]
nicho.reintro <- na.omit(nicho.reintro.na)

## Animals' groups

frug <- unique(nicho.reintro$TYPE)
frug <- as.vector(frug)
frug[1] <- "Alouatta"
frug[16] <- "Dasyprocta"

Alouatta <- nicho.reintro[nicho.reintro$TYPE == "Alouatta guariba", 1:4]
Chiroptera <- nicho.reintro[nicho.reintro$TYPE == "Chiroptera", 1:4]
Passeriformes <- nicho.reintro[nicho.reintro$TYPE == "Passeriformes", 1:4]
Piciformes <- nicho.reintro[nicho.reintro$TYPE == "Piciformes", 1:4]
Coraciiformes <- nicho.reintro[nicho.reintro$TYPE == "Coraciiformes", 1:4]
Primates <- nicho.reintro[nicho.reintro$TYPE == "Primates", 1:4]
Carnivora <- nicho.reintro[nicho.reintro$TYPE == "Carnivora", 1:4]
Columbiformes <- nicho.reintro[nicho.reintro$TYPE == "Columbiformes", 1:4]
Dasyprocta <- nicho.reintro[nicho.reintro$TYPE == "Dasyprocta leporina", 1:4]
Didelphimorphia <- nicho.reintro[nicho.reintro$TYPE == "Didelphimorphia", 1:4]
Cuculiformes <- nicho.reintro[nicho.reintro$TYPE == "Cuculiformes", 1:4]
Galliformes <- nicho.reintro[nicho.reintro$TYPE == "Galliformes", 1:4]
Rodentia <- nicho.reintro[nicho.reintro$TYPE == "Rodentia", 1:4]

# Functions -----------------------------------------------------------------

## Creating a function to get: TPDs of the target species
## Depending on the number of traits you have you will need to add nx
## For e.g. we had 3 traits, so this function has n1, n2 and n3


Kdes <- function(Species, Resources, n1, n2, n3){
  
  # Focal species
  
  Hpi.G1 <- Hpi(x = Species)
  kde.G1 <- kde(x = Species, H = Hpi.G1, gridsize = c(n1,n2,n3), xmin = c(0,0,0), xmax = c(max(Resources[,1]), max(Resources[,2]), max(Resources[,3])), binned = FALSE)
  
  print("eval.points of the species")
  print(kde.G1$eval.points)
  
  int_G1 <- sum(kde.G1$estimate) * ((max(Resources[,1])/n1)
                                    * (max(Resources[,2])/n2)
                                    * (max(Resources[,3])/n3))
  
  print("Integral of the species' TPDs ")
  print(int_G1)
  
  # Correct so Integral = 1 
  
  G1_tpds <- kde.G1$estimate / int_G1
  
  
  
  list(G1_tpds)
  
}

## Creating a function to get the potential effect function of a species or group (Y_i,C)


pe.v <- function(Ab_sp, Ab_Consumers, Sp_tpds, Consumers_tpdc, Resources_tpdc, volume, theta_length){
  
  Consumers <- Consumers_tpdc[-(which(Consumers_tpdc == 0))] 
  Species <- Sp_tpds[-(which(Consumers_tpdc == 0))]
  Resources <- Resources_tpdc[-(which(Consumers_tpdc == 0))]
  
  Pe <- vector()
  
  for(i in 1:theta_length){
    
    Pe[i] <- (sum(((Species * (Ab_sp/Ab_Consumers)/Consumers)^i) * Resources) * volume)^(1/i)
    
    
  }
  
  Pe
}


###########################################################################################################################

# Resources community functional space
Hpi.plnts <- Hpi(x = pnt.plnts)
Plants_kde <- kde(x = pnt.plnts, H = Hpi.plnts, gridsize = c(61, 87, 84), xmin = c(0,0,0), xmax = c(600, 325, 63.84), binned = FALSE)

# The numbers in the grid size (in this case there are 3 numbers because we have 3 traits) should be selected so the integral 
# of all TPDs is as close to 1 as possible. 

#### Correcting integral = 1
Plants_tpdc <- Plants_kde$estimate / (sum(Plants_kde$estimate) * (max(pnt.plnts[,1])/length(Plants_kde$eval.points[[1]]) * max(pnt.plnts[,2])/length(Plants_kde$eval.points[[2]]) * max(pnt.plnts[,3])/length(Plants_kde$eval.points[[3]])))


# Total volume of the resource community functional space

volume <- ((max(pnt.plnts[,1])/61) * (max(pnt.plnts[,2])/87) * (max(pnt.plnts[,3])/84))


# 1) Groups potential effects

## Chiroptera

chiroptera <- Kdes(Species = Chiroptera[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Chiroptera_tpds <- chiroptera[[1]]


## Passeriformes

passeriformes <- Kdes(Species = Passeriformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Passeriformes_tpds <- passeriformes[[1]]


## Piciformes

piciformes <- Kdes(Species = Piciformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Piciformes_tpds <- piciformes[[1]]


## Coraciiformes

coraciiformes <- Kdes(Species = Coraciiformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Coraciiformes_tpds <- coraciiformes[[1]]

## Primates

primates <- Kdes(Species = Primates[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Primates_tpds <- primates[[1]]


## Carnivora

carnivora <- Kdes(Species = Carnivora[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Carnivora_tpds <- carnivora[[1]]


## Columbiformes

columbiformes <- Kdes(Species = Columbiformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Columbiformes_tpds <- columbiformes[[1]]

## Didelphimorphia

didelphimorphia <- Kdes(Species = Didelphimorphia[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Didelphimorphia_tpds <- didelphimorphia[[1]]

## Cuculiformes

cuculiformes <- Kdes(Species = Cuculiformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Cuculiformes_tpds <- cuculiformes[[1]]


## Galliformes

galliformes <- Kdes(Species = Galliformes[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Galliformes_tpds <- galliformes[[1]]


## Rodentia

rodentia <- Kdes(Species = Rodentia[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84) 
Rodentia_tpds <- rodentia[[1]]



# Consumers community Functional space (the sum of each groups' TPDs weighted by the number of species in the group, as in Figure 2A)
Frug_sum <- (1 * Rodentia_tpds) + (1 * Galliformes_tpds) + (2 * Cuculiformes_tpds) + (2 * Didelphimorphia_tpds) + (3 * Columbiformes_tpds) + (3 * Carnivora_tpds) + (2 * Primates_tpds) + (1 * Coraciiformes_tpds) + (6 * Piciformes_tpds) + (86 * Passeriformes_tpds) + (18 * Chiroptera_tpds)

# Consumers community Functional space without the groups' quantitative contribution (same weight for all groups)

Frug_sum.3 <- (Rodentia_tpds) + (Galliformes_tpds) + (Cuculiformes_tpds) + (Didelphimorphia_tpds) + (Columbiformes_tpds) + (Carnivora_tpds) + (Primates_tpds) + (Coraciiformes_tpds) + (Piciformes_tpds) + (Passeriformes_tpds) + (Chiroptera_tpds)


# Groups potential effects with quantitative contribution
Pe_chiroptera <- pe((18 * Chiroptera_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_passeriformes <- pe((86 * Passeriformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_piciformes <- pe((6 * Piciformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_coraciiformes <- pe((1 * Coraciiformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_primates <- pe((2 * Primates_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_carnivora <- pe((3 * Carnivora_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_columbiformes <- pe((3 * Columbiformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_didelphimorphia <- pe((2 * Didelphimorphia_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_cuculiformes <- pe((2 * Cuculiformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_galliformes <- pe((1 * Galliformes_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)
Pe_rodentia <- pe((1 * Rodentia_tpds), Frug_sum, Plants_tpdc, volume, theta_length = 20)


# Groups potential effects without quantitative contribution
Pe_chiroptera.3 <- pe(Chiroptera_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_passeriformes.3 <- pe(Passeriformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_piciformes.3 <- pe(Piciformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_coraciiformes.3 <- pe(Coraciiformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_primates.3 <- pe(Primates_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_carnivora.3 <- pe(Carnivora_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_columbiformes.3 <- pe(Columbiformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_didelphimorphia.3 <- pe(Didelphimorphia_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_cuculiformes.3 <- pe(Cuculiformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_galliformes.3 <- pe(Galliformes_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)
Pe_rodentia.3 <- pe(Rodentia_tpds, Frug_sum.3, Plants_tpdc, volume, theta_length = 20)


## Result

## Groups potential effects in function of theta accounting for the quantitative contribution
Pe_frug_pnt <- as.data.frame(c(Pe_chiroptera, Pe_passeriformes, Pe_piciformes, Pe_primates, Pe_carnivora, Pe_didelphimorphia, Pe_galliformes, Pe_rodentia))
Pe_frug_pnt$Frugivorous <- c(rep("Chiroptera", times = 20), rep("Passeriformes", times = 20), rep("Piciformes", times = 20), rep("Primates", times = 20), rep("Carnivora", times = 20), rep("Didelphimorphia", times = 20), rep("Galliformes", times = 20), rep("Rodentia", times = 20))
Pe_frug_pnt$Teta <- rep(c(1:20), times = 8)
str(Pe_frug_pnt)
colnames(Pe_frug_pnt)[1] <- "Potential effect"
Pe_frug_pnt$Frugivorous <- as.factor(Pe_frug_pnt$Frugivorous)
Pe_frug_pnt$Teta <- as.numeric(Pe_frug_pnt$Teta)

#png("pnt1.2.png", units="in", width=5, height=5, res=300)
ggplot(Pe_frug_pnt, aes(x = Teta, y = `Potential effect`), fill = Frugivorous) +
  geom_line(aes(color = Frugivorous), size=1) +
  xlab("θ") +
  ylab("Potential effect") +
  scale_x_continuous(limits = c(1, 20), breaks = c(1, 5, 10, 15, 20)) +
  scale_color_manual(values  = c("#FF99FF", "#666600", "#66CCCC", "#990066", "#66CC99", "#33CC00", "#FF9900", "#990000")) +  guides(col = guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA),
        axis.text=element_text(size=12), axis.title=element_text(size=14), legend.text = element_text(size = 12))
#dev.off()

# Groups potential effects in function of theta without the quantitative contribution
Pe_frug_pnt.3 <- as.data.frame(c(Pe_chiroptera.3, Pe_passeriformes.3, Pe_piciformes.3, Pe_primates.3, Pe_carnivora.3, Pe_didelphimorphia.3, Pe_galliformes.3, Pe_rodentia.3))
Pe_frug_pnt.3$Frugivorous <- c(rep("Chiroptera", times = 20), rep("Passeriformes", times = 20), rep("Piciformes", times = 20), rep("Primates", times = 20), rep("Carnivora", times = 20), rep("Didelphimorphia", times = 20), rep("Galliformes", times = 20), rep("Rodentia", times = 20))
Pe_frug_pnt.3$Teta <- rep(c(1:20), times = 8)
str(Pe_frug_pnt.3)
colnames(Pe_frug_pnt.3)[1] <- "Potential effect"
Pe_frug_pnt.3$Frugivorous <- as.factor(Pe_frug_pnt.3$Frugivorous)
Pe_frug_pnt.3$Teta <- as.numeric(Pe_frug_pnt.3$Teta)

#png("pnt2.2.png", units="in", width=5, height=5, res=300)
ggplot(Pe_frug_pnt.3, aes(x = Teta, y = `Potential effect`), fill = Frugivorous) +
  geom_line(aes(color = Frugivorous), size=1) +
  xlab("θ") +
  ylab("Potential effect") +
  scale_x_continuous(limits = c(1, 20), breaks = c(1, 5, 10, 15, 20)) +
  scale_color_manual(values  = c("#FF99FF", "#666600", "#66CCCC", "#990066", "#66CC99", "#33CC00", "#FF9900", "#990000")) +  guides(col = guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA),
        axis.text=element_text(size=14), axis.title=element_text(size=16), legend.text = element_text(size = 16))
#dev.off()


## For the reintroduced species

alouatta <- Kdes(Species = Alouatta[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84)
Alouatta_tpds <- alouatta[[1]]


Pe_alouatta <- pe(Alouatta_tpds, (Frug_sum + Alouatta_tpds), Plants_tpdc, volume, theta_length = 20)

dasyprocta <- Kdes(Species = Dasyprocta[,1:3], Resources = pnt.plnts, n1 = 61, n2 = 87, n3 = 84)
Dasyprocta_tpds <- dasyprocta[[1]]


Pe_dasyprocta <- pe(Dasyprocta_tpds, (Frug_sum + Dasyprocta_tpds), Plants_tpdc, volume, theta_length = 20)

# Plot

Pe_reintro <- as.data.frame(c(Pe_alouatta, Pe_dasyprocta))
Pe_reintro$Frugivorous <- c(rep("Alouatta guariba", times = length(Pe_alouatta)), rep("Dasyprocta leporina", times = length(Pe_dasyprocta)))
Pe_reintro$Teta <- rep(c(1:20), times = 2)
str(Pe_reintro)

colnames(Pe_reintro)[1] <- "Potential effect"
Pe_reintro$Frugivorous <- as.factor(Pe_reintro$Frugivorous)
Pe_reintro$Teta <- as.numeric(Pe_reintro$Teta)

#png("ad1.2.png", units="in", width=5, height=5, res=300)
ggplot(Pe_reintro, aes(x = Teta, y = `Potential effect`), fill = Frugivorous) +
  geom_line(aes(color = Frugivorous), size = 1) +
  xlab("θ") +
  ylab("Potential effect") +
  guides(col = guide_legend("")) +
  scale_x_continuous(limits = c(1, 20), breaks = c(1, 5, 10, 15, 20)) +
  scale_color_manual(values = c("#FF9900", "#990000"), labels = c("Howler monkeys", "Agoutis")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA),
        axis.text=element_text(size=14), axis.title=element_text(size=16), legend.text = element_text(size = 16))
#dev.off()


## Niche width 

Plants_tpdc[Plants_tpdc>0] <- 1

Alouatta_tpds[Alouatta_tpds>0] <- 1

Nw_alouatta <- (sum(Alouatta_tpds) * volume) / (sum(Plants_tpdc) * volume)

Dasyprocta_tpds[Dasyprocta_tpds>0] <- 1

Nw_dasyprocta <- (sum(Dasyprocta_tpds) * volume) / (sum(Plants_tpdc) * volume)

