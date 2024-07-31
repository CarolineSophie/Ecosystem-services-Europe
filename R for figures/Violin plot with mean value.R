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

Provision[Provision[]>quantile(Provision, .99)] <- NA
# Remove to large values
Timber[Timber[]>quantile(Timber, .99)] <- NA
# Remove to large values
Carbon[Carbon[]>quantile(Carbon, .99)] <- NA
# Remove to large values
Pollination[Pollination[]>quantile(Pollination, .99)] <- NA
# Remove to large values
Outdoor[Outdoor[]>quantile(Outdoor, .99)] <- NA

LC <- stack(Agriculture, Artificial, Forest, Grassland, Heathland, Sparselyvegetated, Wetlands)
names(LC) <- c("Agriculture", "Artificial", "Forest", "Grassland", "Heathland", "Sparselyvegetated", "Wetlands")

# After loading the datafiles the next step is to match the extent of the Ecosystem Services and Land cover maps . This is done by resampling the land cover data to fit the ecosystem service data using the "nearest neighbor" method. This is done for each land cover type for each service.
LC2 <- resample(LC,Timber)

# Generate raster with the dominating LC type for any given area.
# Number 1-7 represent each of the LC types - only the one with the highest value (most dominating)
# gets saved in the output. That way a particular LC can be called by corresponding number (1-7).
LC3.rast <- calc(LC2, function(x){if(sum(is.na(x))==7){out <- NA}
  else{out <- order(x, decreasing = T)[1]}
  return(out)
})

SES.Provision <- (Provision-mean(Provision[],na.rm=T))/sd(Provision[],na.rm=T)
SES.Timber <- (Timber-mean(Timber[],na.rm=T))/sd(Timber[],na.rm=T)
SES.Carbon <- (Carbon-mean(Carbon[],na.rm=T))/sd(Carbon[],na.rm=T)
SES.Pollination <- (Pollination-mean(Pollination[],na.rm=T))/sd(Pollination[],na.rm=T)
SES.Outdoor <- (Outdoor-mean(Outdoor[],na.rm=T))/sd(Outdoor[],na.rm=T)


# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)

a <- LC3.rast==1
values_agri <- c(SES.Provision_agri = na.omit(SES.Provision[which(a[]==1)]),
                 SES.Timber_agri = na.omit(SES.Timber[which(a[]==1)]),
                 SES.Carbon_agri = na.omit(SES.Carbon[which(a[]==1)]),
                 SES.Pollination_agri = na.omit(SES.Pollination[which(a[]==1)]),
                 SES.Outdoor_agri = na.omit(SES.Outdoor[which(a[]==1)]))
names_agri <- c(rep("Crop Provision",length(na.omit(SES.Provision[which(a[]==1)]))),
                rep("Timber Provision",length(na.omit(SES.Timber[which(a[]==1)]))),
                rep("Carbon Sequestration",length(na.omit(SES.Carbon[which(a[]==1)]))),
                rep("Crop Pollination",length(na.omit(SES.Pollination[which(a[]==1)]))),
                rep("Outdoor recreation",length(na.omit(SES.Outdoor[which(a[]==1)])))
)
data_agri <- data.frame(names_agri, values_agri)

# sample size
sample_size = data_agri %>% group_by(names_agri) %>% summarize(num=n())

p <- data %>%
  mutate(name = fct_relevel(name, 
                            "north", "north-east", "east", 
                            "south-east", "south", "south-west", 
                            "west", "north-west")) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity") +
  xlab("")

data_agri %>%
  left_join(sample_size) %>%
  mutate(names_agri = fct_relevel(names_agri, 
                                  "Crop Provision", "Timber Provision", "Carbon Sequestration", 
                                  "Crop Pollination", "Outdoor recreation"), 
         myaxis = paste0(names_agri, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=values_agri, fill=names_agri)) +
  geom_violin(width=1.4) +
  geom_boxplot(outlier.shape=NA,width=0.1, color="grey", alpha=0.2) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("") + ylab("Z-scores")



# Plot
data_agri %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(names_agri, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=values_agri, fill=names_agri)) +
  geom_violin(width=1.4) +
  geom_boxplot(outlier.shape=NA,width=0.1, color="grey", alpha=0.2) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("") + ylab("Z-scores")

