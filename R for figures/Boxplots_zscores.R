# rm(list = ls())
# graphics.off()

require(raster)
require(sp)

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
# Maybe remove the last 1%
quantile(Provision, .99)
quantile(Timber, .99)
quantile(Carbon, .99)
quantile(Pollination, .99)
quantile(Outdoor, .99)

hist(Provision)
hist(Timber)
hist(Carbon)
hist(Pollination)
hist(Outdoor)

Provision[Provision[]>quantile(Provision, .99)] <- NA
# Remove to large values
Timber[Timber[]>quantile(Timber, .99)] <- NA
# Remove to large values
Carbon[Carbon[]>quantile(Carbon, .99)] <- NA
# Remove to large values
Pollination[Pollination[]>quantile(Pollination, .99)] <- NA
# Remove to large values
Outdoor[Outdoor[]>quantile(Outdoor, .99)] <- NA

hist(Provision)
hist(Timber)
hist(Carbon)
hist(Pollination)
hist(Outdoor)

LC <- stack(Agriculture, Artificial, Forest, Grassland, Heathland, Sparselyvegetated, Wetlands)
names(LC) <- c("Agriculture", "Artificial", "Forest", "Grassland", "Heathland", "Sparselyvegetated", "Wetlands")

# After loading the datafiles the next step is to match the extent of the Ecosystem Services and Land cover maps . This is done by resampling the land cover data to fit the ecosystem service data using the "nearest neighbor" method. This is done for each land cover type for each service.
LC2 <- resample(LC,Timber, method="ngb")

# Generate raster with the dominating LC type for any given area.
# Number 1-7 represent each of the LC types - only the one with the highest value (most dominating)
# gets saved in the output. That way a particular LC can be called by corresponding number (1-7).
LC3.rast <- calc(LC2, function(x){if(sum(is.na(x))==7){out <- NA}
  else{out <- order(x, decreasing = T)[1]}
  return(out)
})

SES.Provision <- (Provision-mean(Provision[],na.rm=T))/sd(Provision[],na.rm=T)
    SES.Provision[SES.Provision] # Print the SES.Provision values
SES.Timber <- (Timber-mean(Timber[],na.rm=T))/sd(Timber[],na.rm=T)
    SES.Timber[SES.Timber]
SES.Carbon <- (Carbon-mean(Carbon[],na.rm=T))/sd(Carbon[],na.rm=T)
    SES.Carbon[SES.Carbon]
SES.Pollination <- (Pollination-mean(Pollination[],na.rm=T))/sd(Pollination[],na.rm=T)
    SES.Pollination[SES.Pollination]
SES.Outdoor <- (Outdoor-mean(Outdoor[],na.rm=T))/sd(Outdoor[],na.rm=T)
    SES.Outdoor[SES.Outdoor]


#### Barplots for Agriculture ####

a <- LC3.rast==1
values_agri <- c(SES.Provision_agri = SES.Provision[which(a[]==1)],
                 SES.Timber_agri = SES.Timber[which(a[]==1)],
                 SES.Carbon_agri = SES.Carbon[which(a[]==1)],
                 SES.Pollination_agri = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_agri = SES.Outdoor[which(a[]==1)])
names_agri <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_agri <- data.frame(names_agri, values_agri)
data_agri$names_agri <- factor(data_agri$names_agri, levels=unique(names_agri))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_agri$values_agri ~ data_agri$names_agri, 
        main="Standardized Ecosystem Service Distribution for Agricultural Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_agri$values_agri ~ data_agri$names_agri, 
        main="Standardized Ecosystem Service Distribution for Agricultural Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_agri <- data.frame(SES.Provision_agri = SES.Provision[which(a[]==1)],
                        SES.Timber_agri = SES.Timber[which(a[]==1)],
                        SES.Carbon_agri = SES.Carbon[which(a[]==1)],
                        SES.Pollination_agri = SES.Pollination[which(a[]==1)],
                        SES.Outdoor_agri = SES.Outdoor[which(a[]==1)])
median_agri <- apply(stat_agri,2,median,na.rm=T)
quantile_agri <- apply(stat_agri,2,quantile,na.rm=T)
IQR_agri <- apply(stat_agri,2,IQR,na.rm=T)
Agriculture_stats <- data.frame(median_agri,IQR_agri)

#### Boxplot for Artificial ####
a <- LC3.rast==2
values_arti <- c(SES.Provision_arti = SES.Provision[which(a[]==1)],
                 SES.Timber_arti = SES.Timber[which(a[]==1)],
                 SES.Carbon_arti = SES.Carbon[which(a[]==1)],
                 SES.Pollination_arti = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_arti = SES.Outdoor[which(a[]==1)])
names_arti <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_arti <- data.frame(names_arti, values_arti)
data_arti$names_arti <- factor(data_arti$names_arti, levels=unique(names_arti))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_arti$values_arti ~ data_arti$names_arti, 
        main="Standardized Ecosystem Service Distribution for Artificial Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_arti$values_arti ~ data_arti$names_arti, 
        main="Standardized Ecosystem Service Distribution for Artificial Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_arti <- data.frame(SES.Provision_arti = SES.Provision[which(a[]==1)],
                        SES.Timber_arti = SES.Timber[which(a[]==1)],
                        SES.Carbon_arti = SES.Carbon[which(a[]==1)],
                        SES.Pollination_arti = SES.Pollination[which(a[]==1)],
                        SES.Outdoor_arti = SES.Outdoor[which(a[]==1)])
median_arti <- apply(stat_arti,2,median,na.rm=T)
quantile_arti <- apply(stat_arti,2,quantile,na.rm=T)
IQR_arti <- apply(stat_arti,2,IQR,na.rm=T)
Artificial_stats <- data.frame(median_arti,IQR_arti)



#### Boxplot for Forests ####
a <- LC3.rast==3
values_fore <- c(SES.Provision_fore = SES.Provision[which(a[]==1)],
                 SES.Timber_fore = SES.Timber[which(a[]==1)],
                 SES.Carbon_fore = SES.Carbon[which(a[]==1)],
                 SES.Pollination_fore = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_fore = SES.Outdoor[which(a[]==1)])
names_fore <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_fore <- data.frame(names_fore, values_fore)
data_fore$names_fore <- factor(data_fore$names_fore, levels=unique(names_fore))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_fore$values_fore ~ data_fore$names_fore, 
        main="Standardized Ecosystem Service Distribution for Forest Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_fore$values_fore ~ data_fore$names_fore, 
        main="Standardized Ecosystem Service Distribution for Forest Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_fore <- data.frame(SES.Provision_fore = SES.Provision[which(a[]==1)],
                        SES.Timber_fore = SES.Timber[which(a[]==1)],
                        SES.Carbon_fore = SES.Carbon[which(a[]==1)],
                        SES.Pollination_fore = SES.Pollination[which(a[]==1)],
                        SES.Outdoor_fore = SES.Outdoor[which(a[]==1)])
median_fore <- apply(stat_fore,2,median,na.rm=T)
quantile_fore <- apply(stat_fore,2,quantile,na.rm=T)
IQR_fore <- apply(stat_fore,2,IQR,na.rm=T)
Forest_stats <- data.frame(median_fore,IQR_fore)


#### Boxplot for Grasslands ####
a <- LC3.rast==4
values_grass <- c(SES.Provision_grass = SES.Provision[which(a[]==1)],
                 SES.Timber_grass = SES.Timber[which(a[]==1)],
                 SES.Carbon_grass = SES.Carbon[which(a[]==1)],
                 SES.Pollination_grass = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_grass = SES.Outdoor[which(a[]==1)])
names_grass <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_grass <- data.frame(names_grass, values_grass)
data_grass$names_grass <- factor(data_grass$names_grass, levels=unique(names_grass))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_grass$values_grass ~ data_grass$names_grass, 
        main="Standardized Ecosystem Service Distribution for Grassland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_grass$values_grass ~ data_grass$names_grass, 
        main="Standardized Ecosystem Service Distribution for Grassland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_grass <- data.frame(SES.Provision_grass = SES.Provision[which(a[]==1)],
                         SES.Timber_grass = SES.Timber[which(a[]==1)],
                         SES.Carbon_grass = SES.Carbon[which(a[]==1)],
                         SES.Pollination_grass = SES.Pollination[which(a[]==1)],
                         SES.Outdoor_grass = SES.Outdoor[which(a[]==1)])
median_grass <- apply(stat_grass,2,median,na.rm=T)
quantile_grass <- apply(stat_grass,2,quantile,na.rm=T)
IQR_grass <- apply(stat_grass,2,IQR,na.rm=T)
Grassland_stats <- data.frame(median_grass,IQR_grass)

#### Boxplots for Heathland ####
a <- LC3.rast==5
values_heath <- c(SES.Provision_heath = SES.Provision[which(a[]==1)],
                 SES.Timber_heath = SES.Timber[which(a[]==1)],
                 SES.Carbon_heath = SES.Carbon[which(a[]==1)],
                 SES.Pollination_heath = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_heath = SES.Outdoor[which(a[]==1)])
names_heath <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_heath <- data.frame(names_heath, values_heath)
data_heath$names_heath <- factor(data_heath$names_heath, levels=unique(names_heath))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_heath$values_heath ~ data_heath$names_heath, 
        main="Standardized Ecosystem Service Distribution for Heathland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_heath$values_heath ~ data_heath$names_heath, 
        main="Standardized Ecosystem Service Distribution for Heathland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_heath <- data.frame(SES.Provision_heath = SES.Provision[which(a[]==1)],
                         SES.Timber_heath = SES.Timber[which(a[]==1)],
                         SES.Carbon_heath = SES.Carbon[which(a[]==1)],
                         SES.Pollination_heath = SES.Pollination[which(a[]==1)],
                         SES.Outdoor_heath = SES.Outdoor[which(a[]==1)])
median_heath <- apply(stat_heath,2,median,na.rm=T)
quantile_heath <- apply(stat_heath,2,quantile,na.rm=T)
IQR_heath <- apply(stat_heath,2,IQR,na.rm=T)
Heathland_stats <- data.frame(median_heath,IQR_heath)

#### Boxplots for Sparselyvegetated ####
a <- LC3.rast==6
values_spveg <- c(SES.Provision_spveg = SES.Provision[which(a[]==1)],
                 SES.Timber_spveg = SES.Timber[which(a[]==1)],
                 SES.Carbon_spveg = SES.Carbon[which(a[]==1)],
                 SES.Pollination_spveg = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_spveg = SES.Outdoor[which(a[]==1)])
names_spveg <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_spveg <- data.frame(names_spveg, values_spveg)
data_spveg$names_spveg <- factor(data_spveg$names_spveg, levels=unique(names_spveg))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_spveg$values_spveg ~ data_spveg$names_spveg, 
        main="Standardized Ecosystem Service Distribution for Sparsely Vegetated Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_spveg$values_spveg ~ data_spveg$names_spveg, 
        main="Standardized Ecosystem Service Distribution for Sparsely Vegetated Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_spveg <- data.frame(SES.Provision_spveg = SES.Provision[which(a[]==1)],
                         SES.Timber_spveg = SES.Timber[which(a[]==1)],
                         SES.Carbon_spveg = SES.Carbon[which(a[]==1)],
                         SES.Pollination_spveg = SES.Pollination[which(a[]==1)],
                         SES.Outdoor_spveg = SES.Outdoor[which(a[]==1)])
median_spveg <- apply(stat_spveg,2,median,na.rm=T)
quantile_spveg <- apply(stat_spveg,2,quantile,na.rm=T)
IQR_spveg <- apply(stat_spveg,2,IQR,na.rm=T)
Spaselyvegetated_stats <- data.frame(median_spveg,IQR_spveg)

#### Boxplots for Wetlands ####
a <- LC3.rast==7
values_wet <- c(SES.Provision_wet = SES.Provision[which(a[]==1)],
                 SES.Timber_wet = SES.Timber[which(a[]==1)],
                 SES.Carbon_wet = SES.Carbon[which(a[]==1)],
                 SES.Pollination_wet = SES.Pollination[which(a[]==1)],
                 SES.Outdoor_wet = SES.Outdoor[which(a[]==1)])
names_wet <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
                rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
                rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
                rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
                rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
)
data_wet <- data.frame(names_wet, values_wet)
data_wet$names_wet <- factor(data_wet$names_wet, levels=unique(names_wet))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_wet$values_wet ~ data_wet$names_wet, 
        main="Standardized Ecosystem Service Distribution for Wetland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_wet$values_wet ~ data_wet$names_wet, 
        main="Standardized Ecosystem Service Distribution for Wetland Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)

stat_wet <- data.frame(SES.Provision_wet = SES.Provision[which(a[]==1)],
                SES.Timber_wet = SES.Timber[which(a[]==1)],
                SES.Carbon_wet = SES.Carbon[which(a[]==1)],
                SES.Pollination_wet = SES.Pollination[which(a[]==1)],
                SES.Outdoor_wet = SES.Outdoor[which(a[]==1)])
median_wet <- apply(stat_wet,2,median,na.rm=T)
quantile_wet <- apply(stat_wet,2,quantile,na.rm=T)
IQR_wet <- apply(stat_wet,2,IQR,na.rm=T)
Wetlands_stats <- data.frame(median_wet,IQR_wet)


# setwd("~/Desktop")
# tiff("Plot4b.tiff", width = 20, height = 15, pointsize = 1/300, units = 'in', res = 300)
# par(mgp=c(3,2,0)) #Location of the tick mark
# boxplot(data_wet$values_wet ~ data_wet$names_wet, 
#         main="Standardized Ecosystem Service Distribution for Wetland Land Cover", 
#         ylab="Z-score value", xlab=NA, col="white",
#         border = NA, frame = FALSE
# )
# grid(nx = NA,
#      ny = NULL)
# boxplot(data_wet$values_wet ~ data_wet$names_wet, 
#         main="Standardized Ecosystem Service Distribution for Wetland Land Cover", 
#         ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
#         add=TRUE)
# dev.off()
# 
# library(ggplot2)
# 
# ggsave("SES_Agriculture.jpg", dpi=400)




