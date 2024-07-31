rm(list=ls()); gc()
graphics.off()
require(raster)

Provision <- raster("~/Desktop/Data/CropProvision/actual flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

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

# After resampling the Land cover raster I stack each Land cover raster and ALL ecosystem Services maps, and take 1000 random samples of 10,000 points. Using these random samples I estimate a loess regression

## LCtype VS Ecosystem functions
for (LCType in c("Agriculture", "Artificial", "Forest", "Grassland")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem functions  
  AgrVsEF <- stack(LC2[[LCType]], # Resampled Landcover raster
                   Provision,Timber,Carbon,Pollination,Outdoor # all the Ecosystem functions maps
  )
  # Define all the locations that have information for all the variables
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

for (LCType in c("Heathland", "Wetlands")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem functions  
  AgrVsEF <- stack(LC2[[LCType]], # Resampled Landcover raster
                   Provision,Timber,Carbon,Pollination,Outdoor # all the Ecosystem functions maps
  )
  # Define all the locations that have information for all the variables
  ToSample <- which(complete.cases(AgrVsEF[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- AgrVsEF[][sample(ToSample,size = 1000),]  
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

for (LCType in c("Sparselyvegetated")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem functions  
  AgrVsEF <- stack(LC2[[LCType]], # Resampled Landcover raster
                   Provision,Timber,Carbon,Pollination,Outdoor # all the Ecosystem functions maps
  )
  # Define all the locations that have information for all the variables
  ToSample <- which(complete.cases(AgrVsEF[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- AgrVsEF[][sample(ToSample,size = 500),]  
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

 #### Z scores ####

Provision[Provision[]>quantile(Provision, .99)] <- NA
# Remove too large values
Timber[Timber[]>quantile(Timber, .99)] <- NA
# Remove too large values
Carbon[Carbon[]>quantile(Carbon, .99)] <- NA
# Remove too large values
Pollination[Pollination[]>quantile(Pollination, .99)] <- NA
# Remove too large values
Outdoor[Outdoor[]>quantile(Outdoor, .99)] <- NA


# LC3.rast <- calc(LC2, function(x){if(sum(is.na(x))==7){out <- NA}
#   else{out <- order(x, decreasing = T)[1]}
#   return(out)
# })

SES.Provision <- (Provision-mean(Provision[],na.rm=T))/sd(Provision[],na.rm=T)
SES.Timber <- (Timber-mean(Timber[],na.rm=T))/sd(Timber[],na.rm=T)
SES.Carbon <- (Carbon-mean(Carbon[],na.rm=T))/sd(Carbon[],na.rm=T)
SES.Pollination <- (Pollination-mean(Pollination[],na.rm=T))/sd(Pollination[],na.rm=T)
SES.Outdoor <- (Outdoor-mean(Outdoor[],na.rm=T))/sd(Outdoor[],na.rm=T)

setwd("/Users/caroline/Desktop")

## LCtype VS Ecosystem Z scores
for (LCType in c("Agriculture", "Artificial", "Forest", "Grassland")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem z scores
  Z_score_stack <- stack(LC2[[LCType]],
                         SES.Provision, SES.Timber, SES.Carbon, SES.Pollination, SES.Outdoor
  )
  # Define all the locations that information for all the variables
  ToSample <- which(complete.cases(Z_score_stack[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- Z_score_stack[][sample(ToSample,size = 10000),]
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
                file = paste0("Z_score_",LCType,".csv"),
                sep = ",",
                append = c(x!=1),
                col.names = c(x==1))
    return(x)
  })
  rm(list=c("SamplForReg","ToSample","Z_score_stack"))
  gc()
}

## LCtype VS Ecosystem Z scores
for (LCType in c("Heathland", "Wetlands")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem z scores
  Z_score_stack <- stack(LC2[[LCType]],
                         SES.Provision, SES.Timber, SES.Carbon, SES.Pollination, SES.Outdoor
  )
  # Define all the locations that information for all the variables
  ToSample <- which(complete.cases(Z_score_stack[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- Z_score_stack[][sample(ToSample,size = 1000),]
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
                file = paste0("Z_score_",LCType,".csv"),
                sep = ",",
                append = c(x!=1),
                col.names = c(x==1))
    return(x)
  })
  rm(list=c("SamplForReg","ToSample","Z_score_stack"))
  gc()
}


## LCtype VS Ecosystem Z scores
for (LCType in c("Sparselyvegetated")){#(LCType <- "Agriculture") # Define the LC type
  # One stack with Landcover and all ecosystem z scores
  Z_score_stack <- stack(LC2[[LCType]],
                         SES.Provision, SES.Timber, SES.Carbon, SES.Pollination, SES.Outdoor
  )
  # Define all the locations that information for all the variables
  ToSample <- which(complete.cases(Z_score_stack[]))
  
  ## Generate a 1000 random samples of 10000 points
  SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
    ### get the value of 10000 points with vakues for all variables
    out <- Z_score_stack[][sample(ToSample,size = 500),]
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
                file = paste0("Z_score_",LCType,".csv"),
                sep = ",",
                append = c(x!=1),
                col.names = c(x==1))
    return(x)
  })
  rm(list=c("SamplForReg","ToSample","Z_score_stack"))
  gc()
}
