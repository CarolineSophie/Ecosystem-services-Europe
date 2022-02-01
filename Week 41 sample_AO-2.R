rm(list=ls());gc()
library(raster)
require(sp)
getwd()
setwd("/Users/caroline/Desktop/Data")
# 1. Confidence interval plots

# Load the Ecosystem service:
Provision <- raster("~/Desktop/Data/CropProvision/actual_flow/EC_flow12_tonha_rescaled.tif")
Timber <- raster("~/Desktop/Data/TimberProvision/actual_flow/Flow_timber_m3_2012_ecocon.tif")
Carbon <- raster("~/Desktop/Data/CarbonSequestration/actual_flow/CO2_Uptake_tonnekm2_2012.tif")
Pollination <- raster("~/Desktop/Data/CropPollination/actual_flow/pollination_actual_flow.tif")/1000
Outdoor <- raster("~/Desktop/Data/OutdoorRecreation/OutdoorRecreation_2012_VistPerkm2.tif")

# Make the Provision raster match the other rasters extents
Provision <- resample(Provision,Timber)

#Load the Land cover types:

Agriculture <-raster("~/Desktop/Archive/CorineAgricultural.tif ")
Artificial <- raster("~/Desktop/Archive/CorineArtificial.tif")
Forest <- raster("~/Desktop/Archive/CorineForest.tif")
Grass <-raster("~/Desktop/Archive/CorineGrassONLY.tif")

#Create a list of all the Land Cover types and ecosystem services for the loop.

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


## Samples for Wetland
# The problem with wetland is that it doesn't have enough complete cases to sample 10000 since it is only 3000 long
# Therefore I will only sample 1000

LCwet <- resample(Wetland,Timber, method="ngb")
LCType <- "Wetland"
WetlandVsES<- stack(LCwet,Provision,Timber,Carbon,Pollination,Flood,Outdoor)

# Define all the locations that information for all the variables
ToSample <- which(complete.cases(WetlandVsES[]))

## Generate a 1000 random samples of 1000 points
SamplForReg <- lapply(1:1000, function(x){#(x <- 1)
  ### get the value of 10000 points with vakues for all variables
  out <- WetlandVsES[][sample(ToSample,size = 1000),]  
  ### Stadarize names of the variables        
  colnames(out) <- c(LCType,"Provision","Timber","Carbon","Pollination","Flood","Outdoor")
  
  ### BUild a model linking each lnad cover type to each ecosystem service 
  Out.3 <- sapply(c("Provision","Timber","Carbon","Pollination","Flood","Outdoor"),
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



Forest <- read.csv("~/Dropbox/Aarhus Assistant Professor/Students/AU/BSc/Caroline Sophie Jessen/Matching the Valuation of biodiveristy in Europe via ecosystem services/Data/DATA/Resample/Out_Forest.csv")
Artificial <- read.csv("~/Dropbox/Aarhus Assistant Professor/Students/AU/BSc/Caroline Sophie Jessen/Matching the Valuation of biodiveristy in Europe via ecosystem services/Data/DATA/Resample/Out_Artificial.csv")
Agriculture <- read.csv("~/Dropbox/Aarhus Assistant Professor/Students/AU/BSc/Caroline Sophie Jessen/Matching the Valuation of biodiveristy in Europe via ecosystem services/Data/DATA/Resample/Out_Agriculture.csv")

TimberEval <- data.frame(LCPerc =1:100,
									  Forest.Timber = tapply(Forest$Timber,Forest$LandCovPerc,mean,na.rm=T),
									  Artificial.Timber = tapply(Artificial$Timber,Artificial$LandCovPerc,mean,na.rm=T),
									  Agriculture.Timber = tapply(Agriculture$Timber,Agriculture$LandCovPerc,mean,na.rm=T)) 

head(TimberEval)


plot(y= TimberEval$Forest.Timber,
		x=1:100,
		ylim = range(TimberEval[-1],na.rm=T),
		type="b",
		pch=19)

points(y= TimberEval$Artificial.Timber,
		  x=1:100,
		  col = "grey",
		  type="b",
		  pch=19)

points(y= TimberEval$Agriculture.Timber,
		  x=1:100,
		  col = "darkgrey",
		  type="b",
		  pch=19)


plot(y=tapply(Forest$Timber,
       Forest$LandCovPerc,
       mean,na.rm=T),
		x=1:100,
		type="b",
		pch=19)
# lines(y=tapply(Forest$Timber,
       # Forest$LandCovPerc,
       # quantile,0.25,
       # na.rm=T),
		# x=1:100,
		# col = "red")
# lines(y=tapply(Forest$Timber,
       # Forest$LandCovPerc,
       # quantile,0.975,
       # na.rm=T),
		# x=1:100,
		# col = "red")

Artificial <- read.csv("~/Dropbox/Aarhus Assistant Professor/Students/AU/BSc/Caroline Sophie Jessen/Matching the Valuation of biodiveristy in Europe via ecosystem services/Data/DATA/Resample/Out_Artificial.csv")


points(y=tapply(Artificial $Timber,
       Artificial $LandCovPerc,
       mean,
       na.rm=T),
		x=1:100,
		type="b",
		pch=19,
		col="grey")
# lines(y=tapply(Artificial $Timber,
       # Artificial $LandCovPerc,
       # quantile,0.25,
       # na.rm=T),
		# x=1:100,
		# col = "blue")
# lines(y=tapply(Artificial $Timber,
       # Artificial $LandCovPerc,
       # quantile,0.975,
       # na.rm=T),
		# x=1:100,
		# col = "blue")
       
Agriculture <- read.csv("~/Dropbox/Aarhus Assistant Professor/Students/AU/BSc/Caroline Sophie Jessen/Matching the Valuation of biodiveristy in Europe via ecosystem services/Data/DATA/Resample/Out_Agriculture.csv")

points(y=tapply(Agriculture $Timber,
       Agriculture $LandCovPerc,
       mean,
       na.rm=T),
		x=1:100,
		type="b",
		pch=19,
		col="darkgrey")


