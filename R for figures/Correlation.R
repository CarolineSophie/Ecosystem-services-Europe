# rm(list = ls())
# graphics.off()

library(corrplot)
require(raster)
require(sp)

Provision <- raster("~/Desktop/Data/CropProvision/actual flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

Provision <- resample(Provision,Timber)

m <- data.frame((Provision[]),(Timber[]),(Carbon[]),(Pollination[]),(Outdoor[]))
names(m) <- c("Crop provision", "Timber provision", "Carbon sequestration", "Crop pollination", "Nature-based recreation")
M <- cor(m, use = "complete.obs", method = "spearman")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         
)

# s <- stack(Provision, Timber, Carbon, Pollination, Outdoor)
# cor<- cor(sampleRandom(s, size= ncell(Provision) * 0.05 ), method = "spearman")
# corrplot(cor, method = "number")
# 
# m <- data.frame((Provision[]),(Timber[]),(Carbon[]),(Pollination[]),(Outdoor[]))
# names(m) <- c("Crop provision", "Timber provision", "Carbon sequestration", "Crop pollination", "Nature-based Recreation")
# M <- cor(m, use = "complete.obs", method = "spearman")
# cor.test(x = (Provision[]), y = (Timber[]), method = "spearman", exact=T)
# 
# cor(x = (Provision[]), y = (Timber[]), use = "complete.obs", method = "spearman")
# cor.test(x = (Carbon[]), y = (Timber[]), method = "spearman", exact=F)


#### CITATION ####

# library(aplpack)
# packageVersion("corrplot")
# citation("packagename")
# 
# citation()
# 
# library(report)
# library(poorman)
# cite_packages()


