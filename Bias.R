rm(list=ls()); gc()
graphics.off()
require(raster)
require(maptools)
require(sp)
library(rworldmap)

# Transform the projection of the world map
newmap <- getMap(resolution = "high")
world_LAEA <- spTransform(newmap, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))


Provision <- raster("~/Desktop/Data/CropProvision/actual flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")
# CHANGE THE WD DIR 
setwd("/Users/caroline/Desktop")

# Make the Provision raster match the other rasters extents
Provision <- resample(Provision,Timber)

#Load the Land cover types:

Agriculture <- raster("~/Desktop/Land cover rasters to use/CorineAgriculture.tif")
Artificial <- raster("~/Desktop/Land cover rasters to use/CorineArtificial.tif")
Forest <- raster("~/Desktop/Land cover rasters to use/CorineForest.tif")
Grassland <- raster("~/Desktop/Land cover rasters to use/CorineGrassland.tif")
Heathland <- raster("~/Desktop/Land cover rasters to use/CorineHeathland.tif")
Sparselyvegetated <- raster("~/Desktop/Land cover rasters to use/CorineSprVeg.tif")
#Urban <- raster("~/Desktop/Land cover rasters to use/CorineUrban.tif")
Wetlands <- raster("~/Desktop/Land cover rasters to use/CorineWetlands.tif")

LC <- stack(Agriculture, Artificial, Forest, Grassland, Heathland, Sparselyvegetated, Wetlands)
names(LC) <- c("Agriculture", "Artificial", "Forest", "Grassland", "Heathland", "Sparselyvegetated", "Wetlands")

# After loading the datafiles the next step is to match the extent of the Ecosystem Services and Land cover maps . This is done by resampling the land cover data to fit the ecosystem service data using the "nearest neighbor" method. This is done for each land cover type for each service.
LC2 <- resample(LC,Timber, method="ngb")


# Agriculture
stack_agri <- stack(LC2$Agriculture,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_agri[]))) # It is these locations that I want to plot
p <- which(complete.cases(stack_agri[]))

l <- calc(stack_agri, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="A. Agriculture", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Agriculture, legend=T, main="A. Agriculture", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Artificial
stack_arti <- stack(LC2$Artificial,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_arti[]))) # It is these locations that I want to plot

l <- calc(stack_arti, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="B. Artificial", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Artificial, legend=T, main="B. Artificial", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Forest
stack_fore <- stack(LC2$Forest,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_fore[]))) # These are the locations I want to plot

l <- calc(stack_fore, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="C. Forest", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Forest, legend=T, main="C. Forest", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Grassland
stack_gras <- stack(LC2$Grassland,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_gras[]))) # These are the locations I want to plot

l <- calc(stack_gras, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="D. Grassland", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Grassland, legend=T, main="D. Grassland", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Heathland
stack_heat <- stack(LC2$Heathland,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_heat[]))) # These are the locations I want to plot

l <- calc(stack_heat, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="E. Heathland", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Heathland, legend=T, main="E. Heathland", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Sparsely vegetated
stack_spar <- stack(LC2$Sparselyvegetated,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_spar[]))) # These are the locations I want to plot

l <- calc(stack_spar, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="F. Sparsely vegetated", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Sparselyvegetated, legend=T, main="F. Sparsely vegetated", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

# Wetland
stack_wetl <- stack(LC2$Wetlands,Provision,Timber,Carbon,Pollination,Outdoor)
length(which(complete.cases(stack_wetl[]))) # These are the locations I want to plot

l <- calc(stack_wetl, function(x){if(sum(is.na(x))==0){out <- 1}
  else{out <- 0}
  return(out)
})
length(l[which(l[]==1)])

plot(l, legend=FALSE, main="G. Wetlands", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)

plot(LC2$Wetlands, legend=T, main="G. Wetlands", adj=0)
plot(world_LAEA,
     add = T,
     border = "black"
)
