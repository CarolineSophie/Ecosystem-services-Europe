rm(list = ls())

require(raster)
require(sp)

# # # Load the land cover data raster
# Corine <- raster("~/Desktop/Corine land cover data 2012/DATA/U2018_CLC2012_V2020_20u1.tif")
# 
# # Question: Here I do not understand why it is we do this?
# NAvalue(Corine) <- 128
# 
# 
# # Artificial surfaces [Grid code 1 to 11]
# CorineArtificial <- reclassify(Corine,
#                                rcl =  matrix(c(0, 11, 1,
#                                                11, Inf, NA),
#                                              ncol=3,
#                                              byrow=T))
# 
# # Agricultural areas [Grid code 12 to 22 with the exception of 18]
# CorineAgricultural <- reclassify(Corine,
#                                  rcl =  matrix(c(0, 11, NA,
#                                                  11, 17, 1,
#                                                  17,18,NA,
#                                                  18, 22, 1,
#                                                  22, Inf, NA),
#                                                ncol=3,
#                                                byrow=T))
# 
# # Forest areas [Grid code 23 to 25 and 29]
# CorineForest <- reclassify(Corine,
#                            rcl =  matrix(c(0, 22, NA,
#                                            22, 25, 1,
#                                            25,28,NA,
#                                            28, 29, 1,
#                                            29, Inf, NA),
#                                          ncol=3,
#                                          byrow=T))
# 
# 
# # Grass areas  [Grid code 18 to 26]
# CorineGrass <- reclassify(Corine,
#                           rcl =  matrix(c(0, 17, NA,
#                                           17, 18, 1,
#                                           18, 25, NA,
#                                           25, 26, 1,
#                                           26, Inf, NA),
#                                         ncol=3,
#                                         byrow=T))
# 
# ## Aggregation of the land cover data
# 
# CorineArtificialSumm <- aggregate(CorineArtificial,
#                                   fact = 10,
#                                   fun = sum,
#                                   filename='~/Desktop/CorineArtificial.tif',
#                                   format = "GTiff", overwrite = TRUE)
# 
# CorineAgriculturalSumm <- aggregate(CorineAgricultural,
#                                     fact = 10,
#                                     fun = sum,
#                                     filename='~/Desktop/CorineAgricultural.tif',
#                                     format = "GTiff", overwrite = TRUE)
# 
# CorineForestSumm <- aggregate(CorineForest,
#                               fact = 10,
#                               fun = sum,
#                               filename='~/Desktop/CorineForest.tif',
#                               format = "GTiff", overwrite = TRUE)
# 
# CorineGrassSumm <- aggregate(CorineGrass,
#                              fact = 10,
#                              fun = sum,
#                              filename='~/Desktop/CorineGrass.tif',
#                              format = "GTiff", overwrite = TRUE)

#______________________________________________________________________________
############### RUN FROM HERE WHEN RECLASSIFICATION IS DONE AND LOAD THE NEW RASTERS ############
#_____________________________________________________________________________


#### UPDATE THESE RASTERS!!!!!!!!!!

Provision <- raster("~/Desktop/Data/CropProvision/actual_flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

setwd("/Users/caroline/Desktop/Data")

# Make the Provision raster match the other rasters extents
Provision <- resample(Provision,Timber)

#Load the Land cover types:
Agriculture <-raster("~/Desktop/Land cover rasters/CorineAgricultural.tif")
Artificial <- raster("~/Desktop/Land cover rasters/CorineArtificial.tif")
Forest <- raster("~/Desktop/Land cover rasters/CorineForest.tif")
Grass <-raster("~/Desktop/Land cover rasters/CorineGrass.tif")


LC <- stack(Agriculture, Artificial, Forest, Grass)
names(LC) <- c("Agriculture", "Artificial", "Forest", "Grass")

# After loading the datafiles the next step is to match the extent of the Ecosystem Services and Land cover maps . This is done by resampling the land cover data to fit the ecosystem service data using the "nearest neighbor" method. This is done for each land cover type for each service.
LC2 <- resample(LC,Timber, method="ngb")

# After resampling the Land cover raster I stack each Land cover raster and ALL ecosystem Services maps, and take 1000 random samples of 10,000 points. Using these random samples I estimate a loess regression

## LCtype VS Ecosystem functions
for (LCType in c("Agriculture", "Artificial", "Forest", "Grass")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem functions  
  AgrVsEF <- stack(LC2[[LCType]], # Resampled Landcover raster
                   Provision,Timber,Carbon,Pollination,Outdoor # all the Ecosystem functions maps
  )
  # Define all the locations that information for all the variables
  ToSample <- which(complete.cases(AgrVsEF[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- AgrVsEF[][sample(ToSample,size = 10000),]  
    ### Stadarize names of the variables        
    colnames(out) <- c(LCType,"Provision","Timber","Carbon","Pollination","Outdoor")
    
    ### BUild a model linking each lnad cover type to each ecosystem service 
    Out.3 <- sapply(c("Provision","Timber","Carbon","Pollination","Outdoor"),
                    function(EVar){#(EVar <- "Provision")
                      #### Temp Data frame withe the Land cover and ecosystem service only                      
                      Out2 <- out[,c(LCType,EVar)]
                      #### Set names
                      colnames(Out2) <- c("LC","EVar")
                      #### Build loess model
                      mod1 <- loess(EVar~LC,data=as.data.frame(Out2))
                      #### Generate prediction for each 1% increment in lnad cover
                      predict(mod1,newdata=data.frame(LC = 1:100))
                    }
    )
    ### Combine the estimates of Expected ecosystem function value per land cover Percentage
    Out.3a <- data.frame(LCType,Sample=x,Out.3)
    ### Save the Compiled table into a file
    write.table(Out.3a,
                file = paste0("Out_",LCType,".csv"),
                sep = ",",
                append = c(x!=1), 
                col.names = c(x==1))      
    return(x)
  })
  rm(list=c("SamplForReg","ToSample","AgrVsEF"))
  gc()
}
