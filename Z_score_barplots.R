rm(list = ls())
graphics.off()

require(raster)
require(sp)
library(ggplot2)
# library(dplyr)
# library(forcats)

# Load the ecosystem service data
Provision <- raster("~/Desktop/Data/CropProvision/actual flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

Provision <- resample(Provision,Timber)

#Load the Land cover types
Agriculture <- raster("~/Desktop/Land cover rasters to use/CorineAgriculture.tif")
Artificial <- raster("~/Desktop/Land cover rasters to use/CorineArtificial.tif")
Forest <- raster("~/Desktop/Land cover rasters to use/CorineForest.tif")
Grassland <- raster("~/Desktop/Land cover rasters to use/CorineGrassland.tif")
Heathland <- raster("~/Desktop/Land cover rasters to use/CorineHeathland.tif")
Sparselyvegetated <- raster("~/Desktop/Land cover rasters to use/CorineSprVeg.tif")
Wetlands <- raster("~/Desktop/Land cover rasters to use/CorineWetlands.tif")

# Remove to large values
Provision[Provision[]>quantile(Provision, .99)] <- NA
# Remove to large values
Timber[Timber[]>quantile(Timber, .99)] <- NA
# Remove to large values
Carbon[Carbon[]>quantile(Carbon, .99)] <- NA
# Remove to large values
Pollination[Pollination[]>quantile(Pollination, .99)] <- NA
# Remove to large values
Outdoor[Outdoor[]>quantile(Outdoor, .99)] <- NA


# Standarzied values #
SES.Provision <- (Provision-mean(Provision[],na.rm=T))/sd(Provision[],na.rm=T)
SES.Timber <- (Timber-mean(Timber[],na.rm=T))/sd(Timber[],na.rm=T)
SES.Carbon <- (Carbon-mean(Carbon[],na.rm=T))/sd(Carbon[],na.rm=T)
SES.Pollination <- (Pollination-mean(Pollination[],na.rm=T))/sd(Pollination[],na.rm=T)
SES.Outdoor <- (Outdoor-mean(Outdoor[],na.rm=T))/sd(Outdoor[],na.rm=T)


# Stack LC types #
LC <- stack(Agriculture, Artificial, Forest, Grassland, 
            Heathland, Sparselyvegetated, Wetlands)
names(LC) <- c("Agriculture", "Artificial", "Forest", "Grassland", 
               "Heathland", "Sparselyvegetated", "Wetlands")

LC2 <- resample(LC,Timber) # Resample the LC types to match the extend of the ES

# Create raster consisting of the dominating LC type.
# Locations can be extracted by 1-7
      # Agriculture = 1
      # Artificial = 2
      # Forest = 3
      # Grassland = 4
      # Heathland = 5
      # Sparselyvegetated = 6
      # Wetlands = 7

LC3.rast <- calc(LC2, function(x){if(sum(is.na(x))==7){out <- NA}
  else{out <- order(x, decreasing = T)[1]}
  return(out)
})

# Check distribution of the dominating LC types
#plot(LC3.rast)


# Agriculture
a <- LC3.rast==1
dominant_agri <- data.frame(SES.Provision_agri = SES.Provision[which(a[]==1)],
               SES.Timber_agri = SES.Timber[which(a[]==1)],
               SES.Carbon_agri = SES.Carbon[which(a[]==1)],
               SES.Pollination_agri = SES.Pollination[which(a[]==1)],
               SES.Outdoor_agri = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_agri,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50% agriculture
Agri_50 <- Agriculture>=50
percent_50 <- data.frame(SES.Provision_agri = SES.Provision[which(Agri_50[]==1)],
               SES.Timber_agri = SES.Timber[which(Agri_50[]==1)],
               SES.Carbon_agri = SES.Carbon[which(Agri_50[]==1)],
               SES.Pollination_agri = SES.Pollination[which(Agri_50[]==1)],
               SES.Outdoor_agri = SES.Outdoor[which(Agri_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% agriculture
Agri_75 <- Agriculture>=75
percent_75 <- data.frame(SES.Provision_agri = SES.Provision[which(Agri_75[]==1)],
                         SES.Timber_agri = SES.Timber[which(Agri_75[]==1)],
                         SES.Carbon_agri = SES.Carbon[which(Agri_75[]==1)],
                         SES.Pollination_agri = SES.Pollination[which(Agri_75[]==1)],
                         SES.Outdoor_agri = SES.Outdoor[which(Agri_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% agriculture
Agri_90 <- Agriculture>=90
percent_90 <- data.frame(SES.Provision_agri = SES.Provision[which(Agri_90[]==1)],
                         SES.Timber_agri = SES.Timber[which(Agri_90[]==1)],
                         SES.Carbon_agri = SES.Carbon[which(Agri_90[]==1)],
                         SES.Pollination_agri = SES.Pollination[which(Agri_90[]==1)],
                         SES.Outdoor_agri = SES.Outdoor[which(Agri_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

agri_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(agri_data) <- c("Dominant", "50%", "75%", "90%")
agri_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(agri_data, 
              direction = "long",
              varying = list(names(agri_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("A. Agriculture")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))

# Artificial
a <- LC3.rast==2
dominant_arti <- data.frame(SES.Provision_arti = SES.Provision[which(a[]==1)],
                            SES.Timber_arti = SES.Timber[which(a[]==1)],
                            SES.Carbon_arti = SES.Carbon[which(a[]==1)],
                            SES.Pollination_arti = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_arti = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_arti,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50%
arti_50 <- Artificial>=50
percent_50 <- data.frame(SES.Provision_arti = SES.Provision[which(arti_50[]==1)],
                         SES.Timber_arti = SES.Timber[which(arti_50[]==1)],
                         SES.Carbon_arti = SES.Carbon[which(arti_50[]==1)],
                         SES.Pollination_arti = SES.Pollination[which(arti_50[]==1)],
                         SES.Outdoor_arti = SES.Outdoor[which(arti_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75%
arti_75 <- Artificial>=75
percent_75 <- data.frame(SES.Provision_arti = SES.Provision[which(arti_75[]==1)],
                         SES.Timber_arti = SES.Timber[which(arti_75[]==1)],
                         SES.Carbon_arti = SES.Carbon[which(arti_75[]==1)],
                         SES.Pollination_arti = SES.Pollination[which(arti_75[]==1)],
                         SES.Outdoor_arti = SES.Outdoor[which(arti_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90%
arti_90 <- Artificial>=90
percent_90 <- data.frame(SES.Provision_arti = SES.Provision[which(arti_90[]==1)],
                         SES.Timber_arti = SES.Timber[which(arti_90[]==1)],
                         SES.Carbon_arti = SES.Carbon[which(arti_90[]==1)],
                         SES.Pollination_arti = SES.Pollination[which(arti_90[]==1)],
                         SES.Outdoor_arti = SES.Outdoor[which(arti_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

arti_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(arti_data) <- c("Dominant", "50%", "75%", "90%")
arti_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(arti_data, 
              direction = "long",
              varying = list(names(arti_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("B. Artificial")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))

# Forest

a <- LC3.rast==3
dominant_fore <- data.frame(SES.Provision_fore = SES.Provision[which(a[]==1)],
                            SES.Timber_fore = SES.Timber[which(a[]==1)],
                            SES.Carbon_fore = SES.Carbon[which(a[]==1)],
                            SES.Pollination_fore = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_fore = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_fore,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50%
fore_50 <- Forest>=50
percent_50 <- data.frame(SES.Provision_fore = SES.Provision[which(fore_50[]==1)],
                         SES.Timber_fore = SES.Timber[which(fore_50[]==1)],
                         SES.Carbon_fore = SES.Carbon[which(fore_50[]==1)],
                         SES.Pollination_fore = SES.Pollination[which(fore_50[]==1)],
                         SES.Outdoor_fore = SES.Outdoor[which(fore_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% 
fore_75 <- Forest>=75
percent_75 <- data.frame(SES.Provision_fore = SES.Provision[which(fore_75[]==1)],
                         SES.Timber_fore = SES.Timber[which(fore_75[]==1)],
                         SES.Carbon_fore = SES.Carbon[which(fore_75[]==1)],
                         SES.Pollination_fore = SES.Pollination[which(fore_75[]==1)],
                         SES.Outdoor_fore = SES.Outdoor[which(fore_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% 
fore_90 <- Forest>=90
percent_90 <- data.frame(SES.Provision_fore = SES.Provision[which(fore_90[]==1)],
                         SES.Timber_fore = SES.Timber[which(fore_90[]==1)],
                         SES.Carbon_fore = SES.Carbon[which(fore_90[]==1)],
                         SES.Pollination_fore = SES.Pollination[which(fore_90[]==1)],
                         SES.Outdoor_fore = SES.Outdoor[which(fore_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

fore_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(fore_data) <- c("Dominant", "50%", "75%", "90%")
fore_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(fore_data, 
              direction = "long",
              varying = list(names(fore_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("C. Forest")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))

# Grassland

a <- LC3.rast==4
dominant_gras <- data.frame(SES.Provision_gras = SES.Provision[which(a[]==1)],
                            SES.Timber_gras = SES.Timber[which(a[]==1)],
                            SES.Carbon_gras = SES.Carbon[which(a[]==1)],
                            SES.Pollination_gras = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_gras = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_gras,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50% 
gras_50 <- Grassland>=50
percent_50 <- data.frame(SES.Provision_gras = SES.Provision[which(gras_50[]==1)],
                         SES.Timber_gras = SES.Timber[which(gras_50[]==1)],
                         SES.Carbon_gras = SES.Carbon[which(gras_50[]==1)],
                         SES.Pollination_gras = SES.Pollination[which(gras_50[]==1)],
                         SES.Outdoor_gras = SES.Outdoor[which(gras_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% 
gras_75 <- Grassland>=75
percent_75 <- data.frame(SES.Provision_gras = SES.Provision[which(gras_75[]==1)],
                         SES.Timber_gras = SES.Timber[which(gras_75[]==1)],
                         SES.Carbon_gras = SES.Carbon[which(gras_75[]==1)],
                         SES.Pollination_gras = SES.Pollination[which(gras_75[]==1)],
                         SES.Outdoor_gras = SES.Outdoor[which(gras_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% 
gras_90 <- Grassland>=90
percent_90 <- data.frame(SES.Provision_gras = SES.Provision[which(gras_90[]==1)],
                         SES.Timber_gras = SES.Timber[which(gras_90[]==1)],
                         SES.Carbon_gras = SES.Carbon[which(gras_90[]==1)],
                         SES.Pollination_gras = SES.Pollination[which(gras_90[]==1)],
                         SES.Outdoor_gras = SES.Outdoor[which(gras_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

gras_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(gras_data) <- c("Dominant", "50%", "75%", "90%")
gras_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(gras_data, 
              direction = "long",
              varying = list(names(gras_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("D. Grassland")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))


# Heathland
a <- LC3.rast==5
dominant_heat <- data.frame(SES.Provision_heat = SES.Provision[which(a[]==1)],
                            SES.Timber_heat = SES.Timber[which(a[]==1)],
                            SES.Carbon_heat = SES.Carbon[which(a[]==1)],
                            SES.Pollination_heat = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_heat = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_heat,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50% 
heat_50 <- Heathland>=50
percent_50 <- data.frame(SES.Provision_heat = SES.Provision[which(heat_50[]==1)],
                         SES.Timber_heat = SES.Timber[which(heat_50[]==1)],
                         SES.Carbon_heat = SES.Carbon[which(heat_50[]==1)],
                         SES.Pollination_heat = SES.Pollination[which(heat_50[]==1)],
                         SES.Outdoor_heat = SES.Outdoor[which(heat_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% 
heat_75 <- Heathland>=75
percent_75 <- data.frame(SES.Provision_heat = SES.Provision[which(heat_75[]==1)],
                         SES.Timber_heat = SES.Timber[which(heat_75[]==1)],
                         SES.Carbon_heat = SES.Carbon[which(heat_75[]==1)],
                         SES.Pollination_heat = SES.Pollination[which(heat_75[]==1)],
                         SES.Outdoor_heat = SES.Outdoor[which(heat_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% 
heat_90 <- Heathland>=90
percent_90 <- data.frame(SES.Provision_heat = SES.Provision[which(heat_90[]==1)],
                         SES.Timber_heat = SES.Timber[which(heat_90[]==1)],
                         SES.Carbon_heat = SES.Carbon[which(heat_90[]==1)],
                         SES.Pollination_heat = SES.Pollination[which(heat_90[]==1)],
                         SES.Outdoor_heat = SES.Outdoor[which(heat_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

heat_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(heat_data) <- c("Dominant", "50%", "75%", "90%")
heat_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(heat_data, 
              direction = "long",
              varying = list(names(heat_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("E. Heathland")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))


# Sparselyvegetated 
a <- LC3.rast==6
dominant_spar <- data.frame(SES.Provision_spar = SES.Provision[which(a[]==1)],
                            SES.Timber_spar = SES.Timber[which(a[]==1)],
                            SES.Carbon_spar = SES.Carbon[which(a[]==1)],
                            SES.Pollination_spar = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_spar = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_spar,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50% 
spar_50 <- Sparselyvegetated>=50
percent_50 <- data.frame(SES.Provision_spar = SES.Provision[which(spar_50[]==1)],
                         SES.Timber_spar = SES.Timber[which(spar_50[]==1)],
                         SES.Carbon_spar = SES.Carbon[which(spar_50[]==1)],
                         SES.Pollination_spar = SES.Pollination[which(spar_50[]==1)],
                         SES.Outdoor_spar = SES.Outdoor[which(spar_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% 
spar_75 <- Sparselyvegetated>=75
percent_75 <- data.frame(SES.Provision_spar = SES.Provision[which(spar_75[]==1)],
                         SES.Timber_spar = SES.Timber[which(spar_75[]==1)],
                         SES.Carbon_spar = SES.Carbon[which(spar_75[]==1)],
                         SES.Pollination_spar = SES.Pollination[which(spar_75[]==1)],
                         SES.Outdoor_spar = SES.Outdoor[which(spar_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% 
spar_90 <- Sparselyvegetated>=90
percent_90 <- data.frame(SES.Provision_spar = SES.Provision[which(spar_90[]==1)],
                         SES.Timber_spar = SES.Timber[which(spar_90[]==1)],
                         SES.Carbon_spar = SES.Carbon[which(spar_90[]==1)],
                         SES.Pollination_spar = SES.Pollination[which(spar_90[]==1)],
                         SES.Outdoor_spar = SES.Outdoor[which(spar_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

spar_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(spar_data) <- c("Dominant", "50%", "75%", "90%")
spar_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(spar_data, 
              direction = "long",
              varying = list(names(spar_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("F. Sparsely vegetated")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))

# Wetlands 
a <- LC3.rast==7
dominant_wetl <- data.frame(SES.Provision_wetl = SES.Provision[which(a[]==1)],
                            SES.Timber_wetl = SES.Timber[which(a[]==1)],
                            SES.Carbon_wetl = SES.Carbon[which(a[]==1)],
                            SES.Pollination_wetl = SES.Pollination[which(a[]==1)],
                            SES.Outdoor_wetl = SES.Outdoor[which(a[]==1)])
mean_dominant <- apply(dominant_wetl,2,mean,na.rm=T)
# barplot(apply(d,2,mean,na.rm=T))

# 50% 
wetl_50 <- Wetlands>=50
percent_50 <- data.frame(SES.Provision_wetl = SES.Provision[which(wetl_50[]==1)],
                         SES.Timber_wetl = SES.Timber[which(wetl_50[]==1)],
                         SES.Carbon_wetl = SES.Carbon[which(wetl_50[]==1)],
                         SES.Pollination_wetl = SES.Pollination[which(wetl_50[]==1)],
                         SES.Outdoor_wetl = SES.Outdoor[which(wetl_50[]==1)])
mean_50 <- apply(percent_50,2,mean,na.rm=T)
# 75% 
wetl_75 <- Wetlands>=75
percent_75 <- data.frame(SES.Provision_wetl = SES.Provision[which(wetl_75[]==1)],
                         SES.Timber_wetl = SES.Timber[which(wetl_75[]==1)],
                         SES.Carbon_wetl = SES.Carbon[which(wetl_75[]==1)],
                         SES.Pollination_wetl = SES.Pollination[which(wetl_75[]==1)],
                         SES.Outdoor_wetl = SES.Outdoor[which(wetl_75[]==1)])
mean_75 <- apply(percent_75,2,mean,na.rm=T)
# 90% 
wetl_90 <- Wetlands>=90
percent_90 <- data.frame(SES.Provision_wetl = SES.Provision[which(wetl_90[]==1)],
                         SES.Timber_wetl = SES.Timber[which(wetl_90[]==1)],
                         SES.Carbon_wetl = SES.Carbon[which(wetl_90[]==1)],
                         SES.Pollination_wetl = SES.Pollination[which(wetl_90[]==1)],
                         SES.Outdoor_wetl = SES.Outdoor[which(wetl_90[]==1)])
mean_90 <- apply(percent_90,2,mean,na.rm=T)

wetl_data <- data.frame(mean_dominant, mean_50, mean_75, mean_90) 
names(wetl_data) <- c("Dominant", "50%", "75%", "90%")
wetl_data$ES <- c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec.")

long<-reshape(wetl_data, 
              direction = "long",
              varying = list(names(wetl_data)[1:4]),
              v.names = "Value",
              idvar = c("ES"))
c(rep("Dominant",5), rep("50%",5), rep("75%",5),rep("90%",5))
long[2] <- c(rep("Dominant",5), rep(">50%",5), rep(">75%",5),rep(">90%",5))
names(long)[names(long)=="time"] <- "LC"
long$LC=factor(long$LC, levels = c("Dominant", ">50%", ">75%", ">90%"))
long$ES=factor(long$ES, levels = c("Crop prov.", "Tim. prov.", "C seq.", "Crop pol.", "Nat. rec."))

ggplot(long, aes(fill=LC, y=Value, x=ES)) + 
  ggtitle("G. Wetlands")+
  geom_bar(position=position_dodge(.9), stat="identity",color="black")+
  scale_fill_brewer(palette="Greys")+
  xlab("Ecosystem service") + ylab("Z-score value")+
  theme_bw()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=14),
        plot.title = element_text(size=22, face = "bold"))+ 
  guides(fill=guide_legend(title="Land coverage"))




# Agriculture
# Artificial
# Forest
# Grassland
# Heathland
# Sparselyvegetated 
# Wetlands 

