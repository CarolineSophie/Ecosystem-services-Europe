# rm(list=ls()); gc()
# graphics.off()

# Load required packages
require(raster)
require(maptools)
require(sp)
library(rworldmap)
library(prettymapr)

# Transform the projection of the world map
newmap <- getMap(resolution = "high")
world_LAEA <- spTransform(newmap, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

# Load the ecosystem service data
Provision <- raster("~/Desktop/Data/CropProvision/actual flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

Provision <- resample(Provision,Timber)

# Crop provision
m <- c(0, 1, 1,  
       1, 2, 2,  
       2, 5, 3, 
       5, 10, 4, 
       10, Inf, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(Provision, rclmat)

plot(world_LAEA,
     col = "grey",
     border = "grey",
     main="A. Crop provision",
     adj=0,
     xlim=extent(Provision)[1:2], ylim=extent(Provision)[3:4]
)
box()
plot(rc,
     add = T,
     col = hcl.colors(5,"YlOrBr", rev = T),
     legend=FALSE
)
plot(world_LAEA,
     add = T,
     border = "black"
)
addnortharrow()
addscalebar()

legend("bottom", fill = hcl.colors(5,"YlOrBr", rev = T),
       legend = c("<1","1-2","2-5","5-10",">10"),
       title = "[Tonne/ha in 2012]",
       inset=c(0,-.15),
       bty = "n", xpd=TRUE,
       horiz = T
)

# Timber Provision
m <- c(0, 250, 1,  
       250, 350, 2,  
       350, 500, 3, 
       500, 1000, 4, 
       1000, Inf, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(Timber, rclmat)

plot(world_LAEA,
     col = "grey",
     border = "grey",
     main="B. Timber provision",
     adj=0,
     xlim=extent(Timber)[1:2], ylim=extent(Timber)[3:4]
)
box()
plot(rc,
     add = T,
     col = hcl.colors(5,"Greens", rev = T),
     legend=FALSE
)
plot(world_LAEA,
     add = T,
     border = "black"
)
addnortharrow()
addscalebar()
legend("bottom",
       fill = hcl.colors(5,"Greens", rev = T),
       legend = c("0-250","250-350","350-500","500-1000",">1000"),
       title = "[m3/km2 in 2012]",
       inset=c(0,-.15),
       bty = "n", xpd=TRUE,
       horiz = T
)

# Global Climate Regulation
m <- c(0, 100, 1,  
       100, 200, 2,  
       200, 300, 3, 
       300, 500, 4, 
       500, Inf, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(Carbon, rclmat)

plot(world_LAEA,
     col = "grey",
     border = "grey",
     main="C. Carbon sequestration",
     adj=0,
     xlim=extent(Carbon)[1:2], ylim=extent(Carbon)[3:4]
)
box()
plot(rc,
     add = T,
     col = hcl.colors(5,"Purples", rev = T),
     legend=FALSE
)
plot(world_LAEA,
     add = T,
     border = "black"
)
addnortharrow()
addscalebar()
legend("bottom",
       fill = hcl.colors(5,"Purples", rev = T),
       legend = c("0-100","100-200","200-300","300-500",">500"),
       title = "[Tonne CO2/km2 in 2012]",
       inset=c(0,-.15),
       bty = "n", xpd=TRUE,
       horiz = T
)


# Pollination
m <- c(#-Inf, 8, NA,
       0, 10, 1,
       10, 15, 2,
       15, 30, 3,
       30, 50, 4,
       50, Inf, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(Pollination, rclmat,
                 include.lowesty = T)


plot(world_LAEA,
     col = "grey",
     border = "grey",
     main="D. Crop pollination",
     adj=0,
     xlim=extent(Pollination)[1:2], ylim=extent(Pollination)[3:4]
)
box()
plot(rc,
     add = T,
     col = hcl.colors(5,"Peach", rev=T),
     legend=FALSE
)
plot(world_LAEA,
     add = T,
     border = "black"
)
addnortharrow()
addscalebar()
legend("bottom",
       fill = hcl.colors(5,"Peach", rev=T),
       legend = c("0-10","10-15","15-30","30-50",">50"),
       title = "[Tonne/km2 in 2012]",
       inset=c(0,-.15),
       bty = "n", xpd=TRUE,
       horiz = T
)

# Outdoor

m <- c(-Inf, 0, 1,
       0, 5, 2,
       5, 24, 3,
       24, 100, 4,
       100, Inf, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(Outdoor, rclmat)
plot(world_LAEA,
     col = "grey",
     border = "grey",
     main="E. Nature-based recreation",
     adj=0,
     xlim=extent(Outdoor)[1:2], ylim=extent(Outdoor)[3:4]
)
box()
plot(rc,
     add = T,
     col = hcl.colors(5,"ag_GrnYl",rev = T),
     legend=FALSE
)
plot(world_LAEA,
     add = T,
     border = "black"
)
addnortharrow()
addscalebar()
legend("bottom",
       fill = hcl.colors(5,"ag_GrnYl",rev = T),
       legend = c("0","1-5","6-24","25-100",">100"),
       title = "[Visits/km2 in 2012]",
       inset=c(0,-.15),
       bty = "n", xpd=TRUE,
       horiz = T
)
