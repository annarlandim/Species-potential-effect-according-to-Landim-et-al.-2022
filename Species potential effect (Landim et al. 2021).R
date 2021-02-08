library(ggplot2)
library(ggtext)
library(scales)
library(gridExtra)

## This script explains how to estimate species potential effects on a ecological process
## according to Landim et al. 2021. First, we present how the theoretical examples were
## created. Then, we present functions and the script to work with real data.

#################################################################################################

### THEORETICAL EXAMPLES (Figures 1, 2 and 3)

# Function to estimate the potential effect for different values of theta

spi <- function(G1_corrigido, Ff, Fp, volume, comprimento){
  
  Ft <- vector()
  
  for(i in 1:comprimento){
    
    Ft[i] <- (sum(((G1_corrigido/Ff)^i) * Fp) * volume)^(1/i)
    
    
  }
  
  Ft
}

# Function to estimate the potential effect for different values of theta when
# info on the quantitative contribution is available

spi_a <- function(G1_corrigido, Ff, Fp, volume, comprimento){
  
  Ff.2 <- Ff[-(which(Ff == 0))] 
  G1_corrigido.2 <- G1_corrigido[-(which(Ff == 0))]
  Fp_corrigido.2 <- Fp[-(which(Ff == 0))]
  
  Ft <- vector()
  
  for(i in 1:comprimento){
    
    Ft[i] <- (sum(((G1_corrigido.2 /Ff.2)^i) * Fp_corrigido.2) * volume)^(1/i)
    
    
  }
  
  Ft
}

##########################################################################################

# Conceptual model (Figure 1)

# Creating the trait values for species i, consumers community and resource community
traits.sic <- seq(from = 20, to = 90, by = 0.1) 
traits.cc <- seq(from = 0.1, to = 100, by = 0.1)
traits.cr <- seq(from = 0.1, to = 120, by = 0.1)

# Creating the trait probability densities for species i, consumers community and resource community
y.sic <- dgamma(traits.sic, shape = 22, scale = 2.3) 
y.cc <- dgamma(traits.cc, shape = 2.5, scale = 11)
y.cr <- dgamma(traits.cr, shape = 2.5, scale = 12)
y.cr <- y.cr * 2

# Organizing into data frames
ex2.sic <- as.data.frame(cbind(traits.sic, y.sic))
ex2.sic$Group <- rep("Interactions performed by consumer species", times = nrow(ex2.sic))
colnames(ex2.sic)[1] <- "Traits"
colnames(ex2.sic)[2] <- "Probability"
ex2.cc <- as.data.frame(cbind(traits.cc, y.cc))
ex2.cc$Group <- rep("Interactions performed by consumer community", times = nrow(ex2.cc))
colnames(ex2.cc)[1] <- "Traits"
colnames(ex2.cc)[2] <- "Probability"
ex2.cr <- as.data.frame(cbind(traits.cr, y.cr))
ex2.cr$Group <- rep("Interactions available within the resource community", times = nrow(ex2.cr))
colnames(ex2.cr)[1] <- "Traits"
colnames(ex2.cr)[2] <- "Probability"
ex.2 <- rbind(ex2.sic, ex2.cc, ex2.cr)
ex.2$Order <- c(rep(3, times = nrow(ex2.sic)), rep(2, times = nrow(ex2.cc)), rep(1, times = nrow(ex2.cr)))
ex.2$Group <- reorder(ex.2$Group, ex.2$Order)

sic.p_ra.ex2 <- ex.2[ex.2$Group == "Interactions performed by consumer species", 2] * 0.1
cc.p_ra <- (ex.2[ex.2$Group == "Interactions performed by consumer community", 2] * 0.9) 
cc.p_ra.ex2 <- c((cc.p_ra[c(1:199)]), ((cc.p_ra[c(200:900)]) + (sic.p_ra.ex2*0.1)), (cc.p_ra[c(901:1000)]))
cr.p_ra.ex2 <- ex.2[ex.2$Group == "Interactions available within the resource community", 2]

ex.2$Probability_ra <- c(sic.p_ra.ex2, cc.p_ra.ex2, cr.p_ra.ex2) 

# Plotting

ggplot(ex.2, aes(x = Traits, fill = Group)) +
  geom_ribbon(aes(ymin = 0, ymax = Probability_ra), alpha = 0.5) +
  xlab("Trait 1") +
  ylim(c(0, 0.08)) +
  scale_fill_manual(values = c("#00BA38", "#FF67A4", "#F8766D"), name = "",
                    labels = c("Interactions available within the resource community", "Interactions performed by the consumers community",
                               expression(paste("Interactions performed by ",italic("Species")[paste(italic("i"))])))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(),  
        axis.ticks = element_blank(), axis.title.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA))

###### Examples (Figures 2 and 3)

# Available data: species presence and abundance
# There are N other species in the Consumers Community (Cc), besides species 1-4
# Species 1-4 have the relative abundance of 0.05 and the N others, added together, 0.8.

# Step 1) TPDs
# Here, with real data, one would use the 'ks' package to build each species TPDs. 
# The commands for a  function on how to build species' and communities' functional 
# spaces using the ks package is available in the end of the script.

# Creating species' trait values

traits.s1c <- seq(from = 0.1, to = 40, by = 0.1)
traits.s2c <- seq(from = 10.1, to = 80, by = 0.1)
traits.s3c <- seq(from = 50.1, to = 120, by = 0.1)  
traits.s4c <- seq(from = 80.1, to = 120, by = 0.1) 
traits.other <- seq(from = 0.1, to = 100, by = 0.1)

# Creating the trait probability densities for focal species 1, 2, 3, 4 and the other species in the consumers community

y.s1c <- dgamma(traits.s1c, shape = 5.5, scale = 2.8) 
y.s2c <- dgamma(traits.s2c, shape = 20, scale = 2.3)  
y.s3c <- dgamma(traits.s3c, shape = 60, scale = 1.3) 
y.s4c <- dgamma(traits.s4c, shape = 245, scale = 0.4)
y.other <- dgamma(traits.other, shape = 3.5, scale = 8) 

# Estimating the integrals

int_s1c <- sum(y.s1c) * (120/1200)
int_s2c <- sum(y.s2c) * (120/1200)
int_s3c <- sum(y.s3c) * (120/1200)
int_s4c <- sum(y.s4c) * (120/1200)
int_other <- sum(y.other) * (120/1200)

# Correcting for integral = 1

y.s1c <- y.s1c/int_s1c
y.s2c <- y.s2c/int_s2c
y.s3c <- y.s3c/int_s3c
y.s4c <- y.s4c/int_s4c
y.other <- y.other/int_other

# Organizing into data frames

ex1.s1c <- as.data.frame(cbind(traits.s1c, y.s1c))
ex1.s1c$Group <- rep("Species 1 from Consumer community", times = nrow(ex1.s1c))
colnames(ex1.s1c)[1] <- "Traits"
colnames(ex1.s1c)[2] <- "Probability"

ex1.s2c <- as.data.frame(cbind(traits.s2c, y.s2c))
ex1.s2c$Group <- rep("Species 2 from Consumer community", times = nrow(ex1.s2c))
colnames(ex1.s2c)[1] <- "Traits"
colnames(ex1.s2c)[2] <- "Probability"

ex1.s3c <- as.data.frame(cbind(traits.s3c, y.s3c))
ex1.s3c$Group <- rep("Species 3 from Consumer community", times = nrow(ex1.s3c))
colnames(ex1.s3c)[1] <- "Traits"
colnames(ex1.s3c)[2] <- "Probability"

ex1.s4c <- as.data.frame(cbind(traits.s4c, y.s4c))
ex1.s4c$Group <- rep("Species 4 from Consumer community", times = nrow(ex1.s4c))
colnames(ex1.s4c)[1] <- "Traits"
colnames(ex1.s4c)[2] <- "Probability"

ex1.other <- as.data.frame(cbind(traits.other, y.other))
ex1.other$Group <- rep("Other species from Consumer community", times = nrow(ex1.other))
colnames(ex1.other)[1] <- "Traits"
colnames(ex1.other)[2] <- "Probability"


ex.1 <- rbind(ex1.s1c, ex1.s2c, ex1.s3c, ex1.s4c, ex1.other)
ex.1$Order <- c(rep(2, times = nrow(ex1.s1c)), rep(3, times = nrow(ex1.s2c)), rep(4, times = nrow(ex1.s3c)), rep(5, times = nrow(ex1.s4c)), rep(1, times = nrow(ex1.other)))
ex.1$Group <- reorder(ex.1$Group, ex.1$Order)

# Plotting

ggplot(ex.1, aes(x = Traits , fill = Group)) +
  geom_ribbon(aes(ymin = 0, ymax = Probability), alpha = 0.6) +
  ylab("Probability density") +
  xlab("Trait 1") +
  scale_fill_manual(values = c("#FF67A4", "#C09B00", "#9590FF", "#EA8331", "#00BFC4")) +
  guides(fill = guide_legend("TPDs")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.text.x = element_blank(),    
        axis.ticks = element_blank(), legend.key = element_rect(colour = NA, fill = NA))


# Step 2) Weighting by the relative quantitative contribution

s1c.p_ra <- ex.1[ex.1$Group == "Species 1 from Consumer community", 2] * 0.05
s2c.p_ra <- ex.1[ex.1$Group == "Species 2 from Consumer community", 2] * 0.05
s3c.p_ra <- ex.1[ex.1$Group == "Species 3 from Consumer community", 2] * 0.05
s4c.p_ra <- ex.1[ex.1$Group == "Species 4 from Consumer community", 2] * 0.05
other.p_ra <- ex.1[ex.1$Group == "Other species from Consumer community", 2] * 0.8

ex.1$Function <- c(s1c.p_ra, s2c.p_ra, s3c.p_ra, s4c.p_ra, other.p_ra)  

# ggplot(ex.1, aes(x = Traits , y = Function, fill = Group)) +
#   geom_ribbon(aes(ymin = 0, ymax = Function), alpha = 0.6) +
#   xlab("Trait 1") +
#   ylim(c(0, 0.03)) +
#   scale_fill_manual(values = c("#FF67A4", "#C09B00", "#9590FF", "#EA8331", "#00BFC4")) +
#   guides(fill = guide_legend("Functional niche")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"),
#         axis.line.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(),    
#         axis.ticks = element_blank(), axis.title.y = element_blank(), legend.key = element_rect(colour = NA, fill = NA))

# The new consumers community trait probability densities

y.cc <- c((other.p_ra[1:100] + s1c.p_ra[1:100]), # 0.1 a 10
          (other.p_ra[101:400] + s1c.p_ra[101:400] + s2c.p_ra[1:300]), # 10.1 a 40
          (other.p_ra[401:500] + s2c.p_ra[301:400]), # 40.1 a 50
          (other.p_ra[501:800] + s2c.p_ra[401:700] + s3c.p_ra[1:300]), # 50.1 a 80
          (other.p_ra[801:900] + s3c.p_ra[301:400] + s4c.p_ra[1:100]), # 80.1 a 90
          (other.p_ra[901:1000] + s3c.p_ra[401:500] + s4c.p_ra[101:200]), # 90.1 a 100
          (s3c.p_ra[501:700] + s4c.p_ra[201:400])) # 100.1 a 120

# Organizing into data frames

traits.cc <- seq(from = 0.1, to = 120, by = 0.1)

ex2.cc <- as.data.frame(cbind(traits.cc, y.cc))
ex2.cc$Group <- rep("Consumer community", times = length(ex2.cc))
colnames(ex2.cc)[1] <- "Traits"
colnames(ex2.cc)[2] <- "Function"

ex2.s1c <- as.data.frame(cbind(traits.s1c, s1c.p_ra))
ex2.s1c$Group <- rep("Species 1 from Consumer community", times = nrow(ex2.s1c))
colnames(ex2.s1c)[1] <- "Traits"
colnames(ex2.s1c)[2] <- "Function"

ex2.s2c <- as.data.frame(cbind(traits.s2c, s2c.p_ra))
ex2.s2c$Group <- rep("Species 2 from Consumer community", times = nrow(ex2.s2c))
colnames(ex2.s2c)[1] <- "Traits"
colnames(ex2.s2c)[2] <- "Function"

ex2.s3c <- as.data.frame(cbind(traits.s3c, s3c.p_ra))
ex2.s3c$Group <- rep("Species 3 from Consumer community", times = nrow(ex2.s3c))
colnames(ex2.s3c)[1] <- "Traits"
colnames(ex2.s3c)[2] <- "Function"

ex2.s4c <- as.data.frame(cbind(traits.s4c, s4c.p_ra))
ex2.s4c$Group <- rep("Species 4 from Consumer community", times = nrow(ex2.s4c))
colnames(ex2.s4c)[1] <- "Traits"
colnames(ex2.s4c)[2] <- "Function"

ex2.other <- as.data.frame(cbind(traits.other, other.p_ra))
ex2.other$Group <- rep("Other species from Consumer community", times = nrow(ex2.other))
colnames(ex2.other)[1] <- "Traits"
colnames(ex2.other)[2] <- "Function"

ex.2 <- rbind(ex2.cc, ex2.s1c, ex2.s2c, ex2.s3c, ex2.s4c, ex2.other)

ex.2$Order <- c(rep(1, times = nrow(ex2.cc)), rep(3, times = nrow(ex2.s1c)), rep(4, times = nrow(ex2.s2c)), rep(5, times = nrow(ex2.s3c)), rep(6, times = nrow(ex2.s4c)), rep(2, times = nrow(ex2.other)))
ex.2$Group <- reorder(ex.2$Group, ex.2$Order)

ggplot(ex.2, aes(x = Traits , y = Function, fill = Group)) +
  geom_ribbon(aes(ymin = 0, ymax = Function), alpha = 0.6) +
  ylab("Probability density") +
  xlab("Trait 1") +
  ylim(c(0, 0.03)) +
  scale_fill_manual(values = c("grey", "#FF67A4", "#C09B00", "#9590FF", "#EA8331", "#00BFC4"),
                    name = "Consumers community",
                    labels = c(expression(paste("All species  ", italic("(N)"))), expression(paste("All other species  ", italic("(N - 4)"))), 
                               expression(paste(italic("Species")[1])), expression(paste(italic("Species")[2])), 
                               expression(paste(italic("Species")[3])),  expression(paste(italic("Species")[4])))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.text.x = element_blank(),  
        axis.ticks = element_blank(), legend.key = element_rect(colour = NA, fill = NA))

# Step 3) Relating to the resources community

# Creating the resource community functional space

traits.cr <- seq(from = 0.1, to = 120, by = 0.1)
y.cr <- dgamma(traits.cr, shape = 3.2, scale = 16)

# Correcting integral = 1

int_cr <- sum(y.cr) * (120/1200)
y.cr <- y.cr/int_cr

# Organizing into data frame

ex3.cr <- as.data.frame(cbind(traits.cr, y.cr))
ex3.cr$Group <- rep("Resource community", times = length(ex3.cr))
colnames(ex3.cr)[1] <- "Traits"
colnames(ex3.cr)[2] <- "Function"

ex.3 <- rbind(ex.2[,c(1:3)], ex3.cr)

ex.3$Order <- c(rep(2, times = nrow(ex2.cc)), rep(4, times = nrow(ex2.s1c)), rep(5, times = nrow(ex2.s2c)), rep(6, times = nrow(ex2.s3c)), rep(7, times = nrow(ex2.s4c)), rep(3, times = nrow(ex2.other)), rep(1, times = nrow(ex3.cr)))
ex.3$Group <- reorder(ex.3$Group, ex.3$Order)

# Plotting

ggplot(ex.3[ex.3$Group %in% c("Consumer community", "Species 1 from Consumer community", "Species 2 from Consumer community", "Species 3 from Consumer community", "Species 4 from Consumer community", "Resource community"), ], aes(x = Traits , y = Function, fill = Group)) +
  geom_ribbon(aes(ymin = 0, ymax = Function), alpha = 0.6) +
  xlab("Trait 1") +
  ylim(c(0, 0.0266)) +
  scale_fill_manual(values = c("#00BA38", "grey", "#C09B00", "#9590FF", "#EA8331", "#00BFC4"), 
                    name = "Functional niche",
                    labels = c("Consumer community", "Resource community", 
                               expression(paste(italic("Species")[1])), expression(paste(italic("Species")[2])), 
                               expression(paste(italic("Species")[3])),  expression(paste(italic("Species")[4])))) +
  labs(y = "Probability density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), axis.text.x = element_blank(), 
        axis.ticks = element_blank())

# Extra step) Species originality x Resources community

# Including 0s in the species TPDs
 
 s1c.p_ra <- c(s1c.p_ra, rep(0, times = length(401:1200)))
 s2c.p_ra <- c(rep(0, times = length(1:100)), s2c.p_ra, rep(0, times = length(1:400)))
 s3c.p_ra <- c(rep(0, times = length(1:500)), s3c.p_ra)
 s4c.p_ra <- c(rep(0, times = length(1:800)), s4c.p_ra)  
   
# Estimating the originalities
  
 s1c.or <- s1c.p_ra/y.cc
 s2c.or <- s2c.p_ra/y.cc
 s3c.or <- s3c.p_ra/y.cc
 s4c.or <- s4c.p_ra/y.cc
 
# Organizing into data frames 
 
 ex4.s1c <- as.data.frame(cbind(traits.cc, s1c.or))
 ex4.s1c$Group <- rep("Species 1 from Consumer community", times = nrow(ex4.s1c))
 colnames(ex4.s1c)[1] <- "Traits"
 colnames(ex4.s1c)[2] <- "Function"
 
 ex4.s2c <- as.data.frame(cbind(traits.cc, s2c.or))
 ex4.s2c$Group <- rep("Species 2 from Consumer community", times = nrow(ex4.s2c))
 colnames(ex4.s2c)[1] <- "Traits"
 colnames(ex4.s2c)[2] <- "Function"
 
 ex4.s3c <- as.data.frame(cbind(traits.cc, s3c.or))
 ex4.s3c$Group <- rep("Species 3 from Consumer community", times = nrow(ex4.s3c))
 colnames(ex4.s3c)[1] <- "Traits"
 colnames(ex4.s3c)[2] <- "Function"
 
 ex4.s4c <- as.data.frame(cbind(traits.cc, s4c.or))
 ex4.s4c$Group <- rep("Species 4 from Consumer community", times = nrow(ex4.s4c))
 colnames(ex4.s4c)[1] <- "Traits"
 colnames(ex4.s4c)[2] <- "Function"
 
 ex4.cr <- as.data.frame(cbind(traits.cr, (y.cr*100)))
 ex4.cr$Group <- rep("Resources community", times = nrow(ex4.cr))
 colnames(ex4.cr)[1] <- "Traits"
 colnames(ex4.cr)[2] <- "Function"
 
 ex.4 <- rbind(ex4.s1c, ex4.s2c, ex4.s3c, ex4.s4c, ex4.cr)
 
 ex.4$Order <- c(rep(2, times = nrow(ex4.s1c)), rep(3, times = nrow(ex4.s2c)), rep(4, times = nrow(ex4.s3c)), rep(5, times = nrow(ex4.s4c)), rep(1, times = nrow(ex4.cr)))
 ex.4$Group <- reorder(ex.4$Group, ex.4$Order)
 
# Plotting 
 
 ggplot(ex.4, aes(x = Traits , y = Function, fill = Group)) +
   geom_ribbon(aes(ymin = 0, ymax = Function), alpha = 0.6) +
   xlab("Trait 1") +
   scale_fill_manual(values = c("#00BA38", "#C09B00", "#9590FF", "#EA8331", "#00BFC4"), 
                     name = "Cr functional niche and species originality",
                     labels = c("Resource community", 
                                expression(paste(italic("Species")[1])), expression(paste(italic("Species")[2])), 
                                expression(paste(italic("Species")[3])),  expression(paste(italic("Species")[4])))) +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         axis.line.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), 
         axis.ticks = element_blank(), axis.title.y = element_blank())
 
   
# 5) Species potential effect
 
# Attention: If you skipped the extra step you will need to run the next four command lines: 

#s1c.p_ra <- c(s1c.p_ra, rep(0, times = length(401:1200)))
#s2c.p_ra <- c(rep(0, times = length(1:100)), s2c.p_ra, rep(0, times = length(1:400)))
#s3c.p_ra <- c(rep(0, times = length(1:500)), s3c.p_ra)
#s4c.p_ra <- c(rep(0, times = length(1:800)), s4c.p_ra)  
 
# Estimating the potential effects using the function presented in the beginning of the script 

s1c.pi <- spi(G1_corrigido = s1c.p_ra, Ff = y.cc, Fp = y.cr,
              volume = (max(traits.cr)/length(y.cc)), comprimento = 10)
s2c.pi <- spi(G1_corrigido = s2c.p_ra, Ff = y.cc, Fp = y.cr,
              volume = (max(traits.cr)/length(y.cc)), comprimento = 10)
s3c.pi <- spi(G1_corrigido = s3c.p_ra, Ff = y.cc, Fp = y.cr,
              volume = (max(traits.cr)/length(y.cc)), comprimento = 10)
s4c.pi <- spi(G1_corrigido = s4c.p_ra, Ff = y.cc, Fp = y.cr,
              volume = (max(traits.cr)/length(y.cc)), comprimento = 10)

# Organizing into a data frame

Spi <- as.data.frame(c(s1c.pi, s2c.pi, s3c.pi, s4c.pi))
Spi$Group <- c(rep("Species 1 from Consumer community", times = length(s1c.pi)), rep("Species 2 from Consumer community", times = length(s2c.pi)), rep("Species 3 from Consumer community", times = length(s3c.pi)), rep("Species 4 from Consumer community", times = length(s4c.pi)))
Spi$Teta <- rep(c(1:10), times = 4)
colnames(Spi)[1] <- "Species potential effect"

# Plotting

ggplot(Spi, aes(x = Teta, y = `Species potential effect`), fill = Group) +
  geom_line(aes(color = Group), size = 1) +
  xlab("θ") +
  ylab("Species potential effect") +
  scale_x_continuous(limits = c(1, 10), breaks = c(1, 5, 10)) +
  scale_color_manual(values  = c("#C09B00", "#9590FF", "#EA8331", "#00BFC4"),
                     labels = c(expression(paste(italic("S")["1,c"])), expression(paste(italic("S")["2,c"])), expression(paste(italic("S")["3,c"])),  expression(paste(italic("S")["4,c"])))) +
  guides(col = guide_legend("")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text.y = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")


# 6) Niche amplitude

# Indicator function of the resource community

y.cr[y.cr>0] <- 1

# Indicator function of species 1

s1c.p_ra[s1c.p_ra>0] <- 1

# Niche amplitude of species 1

s1c.na <- (sum(s1c.p_ra) * (max(traits.cr)/length(y.cr))) / (sum(y.cr) * (max(traits.cr)/length(y.cr))) 

# Indicator function of species 2

s2c.p_ra[s2c.p_ra>0] <- 1

# Niche amplitude of species 2

s2c.nw <- (sum(s2c.p_ra) * (max(traits.cr)/length(y.cr))) / (sum(y.cr) * (max(traits.cr)/length(y.cr))) 

# Indicator function of species 3

s3c.p_ra[s3c.p_ra>0] <- 1

# Niche amplitude of species 3

s3c.nw <- (sum(s3c.p_ra) * (max(traits.cr)/length(y.cr))) / (sum(y.cr) * (max(traits.cr)/length(y.cr))) 

# Indicator function of species 4

s4c.p_ra[s4c.p_ra>0] <- 1

# Niche amplitude of species 4

s4c.nw <- (sum(s4c.p_ra) * (max(traits.cr)/length(y.cr))) / (sum(y.cr) * (max(traits.cr)/length(y.cr))) 

############################################################################################################

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

