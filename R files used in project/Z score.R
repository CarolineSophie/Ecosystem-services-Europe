
rm(list = ls())
graphics.off()

require(raster)
require(sp)

# removeTmpFiles(h=1/60) # --> to delete the temp files


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

# # Remove to large values
# Provision[Provision[]>10] <- NA#10
# # Remove to large values
# Timber[Timber[]>1000] <- NA#1000
# # Remove to large values
# Carbon[Carbon[]>500] <- NA#500
# # Remove to large values
# Pollination[Pollination[]>50] <- NA#50000
# # Remove to large values
# Outdoor[Outdoor[]>100] <- NA#100

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
LC2 <- resample(LC,Timber)

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

# 
# boxplot(SES.Provision[])  
# median(SES.Provision[], na.rm = T)
# quantile(SES.Provision[], na.rm = T)
# 
# boxplot(SES.Provision[], SES.Timber[], SES.Carbon[], SES.Pollination[],SES.Outdoor[])  
#     
# When creating the a rasters - each a represent a given LC type. The a raster has the values 0-1:
# 1 if the LC type is equal to the LC type called (a <- LC3==x) and 0 if it isn't. Thereby a can be used
# to call only the locations where a certain LC is the most dominating.


# ### Boxplot for Agriculture
# a <- LC3.rast==1
# values_agri <- c(SES.Provision_agri = SES.Provision[which(a[]==1)],
#                 SES.Timber_agri = SES.Timber[which(a[]==1)],
#                 SES.Carbon_agri = SES.Carbon[which(a[]==1)],
#                 SES.Pollination_agri = SES.Pollination[which(a[]==1)],
#                 SES.Outdoor_agri = SES.Outdoor[which(a[]==1)])
# names_agri <- c(rep("Crop Provision"),length(SES.Provision[which(a[]==1)]), 
#                 rep("Timber Provision"),length(SES.Timber[which(a[]==1)]),
#                 rep("Carbon Sequestration"),length(SES.Carbon[which(a[]==1)]),
#                 rep("Crop Pollination"),length(SES.Pollination[which(a[]==1)]),
#                 rep("Outdoor recreation"),length(SES.Outdoor[which(a[]==1)])
#                 )
# data_agri <- data.frame(names_agri, values_agri)
# boxplot(data_agri$values_agri ~ data_agri$names_agri, main=paste("Z-score distribution for Agricultural Land Cover\n n=",length(SES.Provision[which(a[]==1)]), sep = " "), ylab="Z-score" , 
#         xlab="Standardized Ecosystem Service")
# 
# # Type 2 
# a <- LC3.rast==1
# values_agri <- c(SES.Provision_agri = SES.Provision[which(a[]==1)],
#                  SES.Timber_agri = SES.Timber[which(a[]==1)],
#                  SES.Carbon_agri = SES.Carbon[which(a[]==1)],
#                  SES.Pollination_agri = SES.Pollination[which(a[]==1)],
#                  SES.Outdoor_agri = SES.Outdoor[which(a[]==1)])
# names_agri <- c(rep(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),length(SES.Provision[which(a[]==1)])),
#                 rep(paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),length(SES.Timber[which(a[]==1)])),
#                 rep(paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),length(SES.Carbon[which(a[]==1)])),
#                 rep(paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),length(SES.Pollination[which(a[]==1)])),
#                 rep(paste("Outdoor recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " "),length(SES.Outdoor[which(a[]==1)]))
# )

# names(values_agri) <- c(paste("Crop Provision\n n=",length(na.omit(SES.Provision[which(a[]==1)])), sep = " "),
#                         paste("Timber Provision\n n=",length(na.omit(SES.Timber[which(a[]==1)])), sep = " "),
#                         paste("Carbon Sequestration\n n=",length(na.omit(SES.Carbon[which(a[]==1)])), sep = " "),
#                         paste("Crop Pollination\n n=",length(na.omit(SES.Pollination[which(a[]==1)])), sep = " "),
#                         paste("Outdoor Recreation\n n=",length(na.omit(SES.Outdoor[which(a[]==1)])), sep = " ")
# )
# boxplot(values_agri, main=paste("Z-score distribution for Agricultural Land Cover\n n=",length(SES.Provision[which(a[]==1)]), sep = " "), ylab="Z-score" , 
#         xlab="Standardized Ecosystem Service")

data_agri <- data.frame(names_agri, values_agri)
data_agri$names_agri <- factor(data_agri$names_agri, levels=unique(names_agri))

par(mgp=c(3,2,0)) #Location of the tick mark
boxplot(data_agri$values_agri ~ data_agri$names_agri, 
        main="Standardized Ecosystem Service distribution for Agricultural Land Cover", 
        ylab="Z-score value", xlab=NA, col="white",
        border = NA, frame = FALSE
)
grid(nx = NA,
     ny = NULL)
boxplot(data_agri$values_agri ~ data_agri$names_agri, 
        main="Standardized Ecosystem Service distribution for Agricultural Land Cover", 
        ylab="Z-score value", xlab=NA, col="white", outpch=1, outcex=.5,
        add=TRUE)



#barplot(apply(d,2,mean,na.rm=T))
boxplot(d[], na.rm=T)
#range(d[1])


### Barplot for Artificial
a <- LC3.rast==2
d<- data.frame(SES.Provision_arti = SES.Provision[which(a[]==1)],
               SES.Timber_arti = SES.Timber[which(a[]==1)],
               SES.Carbon_arti = SES.Carbon[which(a[]==1)],
               SES.Pollination_arti = SES.Pollination[which(a[]==1)],
               SES.Outdoor_arti = SES.Outdoor[which(a[]==1)])
barplot(apply(d,2,mean,na.rm=T))
  # 2 is for margins --> 1 indicates rows, 2 indicates columns
  # therefore within d, take the mean of the columns

### Barplot for forests
a <- LC3.rast==3
d<- data.frame(SES.Provision_fore = SES.Provision[which(a[]==1)],
               SES.Timber_fore = SES.Timber[which(a[]==1)],
               SES.Carbon_fore = SES.Carbon[which(a[]==1)],
               SES.Pollination_fore = SES.Pollination[which(a[]==1)],
               SES.Outdoor_fore = SES.Outdoor[which(a[]==1)])
#barplot(apply(d,2,mean,na.rm=T))


### Barplot for grasslands
a <- LC3.rast==4
d<- data.frame(SES.Provision_grass = SES.Provision[which(a[]==1)],
               SES.Timber_grass = SES.Timber[which(a[]==1)],
               SES.Carbon_grass = SES.Carbon[which(a[]==1)],
               SES.Pollination_grass = SES.Pollination[which(a[]==1)],
               SES.Outdoor_grass = SES.Outdoor[which(a[]==1)])
#barplot(apply(d,2,mean,na.rm=T))

### Boxplots for Heathland 
a <- LC3.rast==5
d<- data.frame(SES.Provision_grass = SES.Provision[which(a[]==1)],
               SES.Timber_grass = SES.Timber[which(a[]==1)],
               SES.Carbon_grass = SES.Carbon[which(a[]==1)],
               SES.Pollination_grass = SES.Pollination[which(a[]==1)],
               SES.Outdoor_grass = SES.Outdoor[which(a[]==1)])
#barplot(apply(d,2,mean,na.rm=T))

### Boxplots for sparselyvegetated
a <- LC3.rast==6
d<- data.frame(SES.Provision_spveg = SES.Provision[which(a[]==1)],
               SES.Timber_spveg = SES.Timber[which(a[]==1)],
               SES.Carbon_spveg = SES.Carbon[which(a[]==1)],
               SES.Pollination_spveg = SES.Pollination[which(a[]==1)],
               SES.Outdoor_spveg = SES.Outdoor[which(a[]==1)])
#barplot(apply(d,2,mean,na.rm=T))

### Boxplots for Wetlands
a <- LC3.rast==7
d<- data.frame(SES.Provision_wet = SES.Provision[which(a[]==1)],
               SES.Timber_wet = SES.Timber[which(a[]==1)],
               SES.Carbon_wet = SES.Carbon[which(a[]==1)],
               SES.Pollination_wet = SES.Pollination[which(a[]==1)],
               SES.Outdoor_wet = SES.Outdoor[which(a[]==1)])
#barplot(apply(d,2,mean,na.rm=T))

"Heathland", "Sparselyvegetated", "Wetlands"

SES.Provision
SES.Timber
SES.Carbon
SES.Pollination
SES.Outdoor



# setwd("/Users/caroline/Desktop")
# 
# ## LCtype VS Ecosystem Z scores
# for (LCType in c("Agriculture", "Artificial", "Forest", "Grass")){#(LCType <- "Agriculture") # Define the LC type
#   # One stack with Landcover and all ecosystem z scores
#   Z_score_stack <- stack(LC2[[LCType]],
#                    SES.Provision, SES.Timber, SES.Carbon, SES.Pollination, SES.Outdoor
#   )
#   # Define all the locations that information for all the variables
#   ToSample <- which(complete.cases(Z_score_stack[]))
# 
#   ## Generate a 1000 random samples of 10000 points
#   SamplForReg <- lapply(1:100, function(x){#(x <- 1)
#     ### get the value of 10000 points with vakues for all variables
#     out <- Z_score_stack[][sample(ToSample,size = 1000),]
#     ### Stadarize names of the variables
#     colnames(out) <- c(LCType,"Provision","Timber","Carbon","Pollination","Outdoor")
# 
#     ### BUild a model linking each lnad cover type to each ecosystem service
#     Out.3 <- sapply(c("Provision","Timber","Carbon","Pollination","Outdoor"),
#                     function(EVar){#(EVar <- "Provision")
#                       #### Temp Data frame withe the Land cover and ecosystem service only
#                       Out2 <- out[,c(LCType,EVar)]
#                       #### Set names
#                       colnames(Out2) <- c("LC","EVar")
#                       #### Build loess model
#                       mod1 <- loess(EVar~LC,data=as.data.frame(Out2))
#                       #### Generate prediction for each 1% increment in lnad cover
#                       predict(mod1,newdata=data.frame(LC = 1:100))
#                     }
#     )
#     ### Combine the estimates of Expected ecosystem function value per land cover Percentage
#     Out.3a <- data.frame(LCType,Sample=x,Out.3)
#     ### Save the Compiled table into a file
#     write.table(Out.3a,
#                 file = paste0("Out_",LCType,".csv"),
#                 sep = ",",
#                 append = c(x!=1),
#                 col.names = c(x==1))
#     return(x)
#   })
#   rm(list=c("SamplForReg","ToSample","Z_score_stack"))
#   gc()
# }
# 
# 
# 
# Agriculture_Z <- read.csv("~/Desktop/Z_score_Agriculture.csv")
# Artificial_Z <- read.csv("~/Desktop/Z_score_Artificial.csv")
# Forest_Z <- read.csv("~/Desktop/Z_score_Forest.csv")
# Grass_Z <- read.csv("~/Desktop/Z_score_Grassland.csv")
# 
# 
# SES.Provision
# SES.Timber
# SES.Carbon
# SES.Pollination
# SES.Outdoor
# 
# Agriculture_Zplot <- data.frame(LCPerc = 1:100,
#                             Agriculture.Provision = tapply(Agriculture_Z$Provision,Agriculture_Z$LCPercent,mean,na.rm=T),
#                             Agriculture.Timber = tapply(Agriculture_Z$Timber,Agriculture_Z$LCPercent,mean,na.rm=T),
#                             Agriculture.Carbon = tapply(Agriculture_Z$Carbon,Agriculture_Z$LCPercent,mean,na.rm=T),
#                             Agriculture.Pollination = tapply(Agriculture_Z$Pollination,Agriculture_Z$LCPercent,mean,na.rm=T),
#                             Agriculture.Outdoor = tapply(Agriculture_Z$Outdoor,Agriculture_Z$LCPercent,mean,na.rm=T)
# )
# 
# 
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
# 
# plot(y = Agriculture_Zplot$Agriculture.Provision,
#      x = 1:100,
#      ylim = range(Agriculture_Zplot[-1],na.rm=T),
#      col = "darkgoldenrod2",
#      type = "b",
#      pch = 20,
#      main = "Agriculture",
#      xlab = "Land Cover Percentage",
#      ylab = "Z-score"
# )
# 
# points(y = Agriculture_Zplot$Agriculture.Timber,
#        x = 1:100,
#        col = "Forestgreen",
#        type = "b",
#        pch = 20)
# 
# points(y = Agriculture_Zplot$Agriculture.Carbon,
#        x = 1:100,
#        col = "gray",
#        type = "b",
#        pch = 20)
# 
# points(y = Agriculture_Zplot$Agriculture.Pollination,
#        x = 1:100,
#        col = "yellow",
#        type = "b",
#        pch = 20)
# 
# points(y = Agriculture_Zplot$Agriculture.Outdoor,
#        x = 1:100,
#        col = "blue",
#        type = "b",
#        pch = 20)
# 
# 
# legend("right", fill = c("darkgoldenrod2","forestgreen", "gray", "yellow", "blue"),
#        legend = c("Crop Provision","Timber Provision","Carbon Sequestration","Crop Pollination", "Outdoor recreation"),
#        title = "Ecosystem service"
# )
#                             






