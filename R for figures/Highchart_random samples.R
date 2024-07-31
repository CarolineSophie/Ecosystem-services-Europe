library(highcharter)

Agriculture_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Agriculture.csv")
Artificial_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Artificial.csv")
Forest_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Forest.csv")
Grassland_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Grassland.csv")
Heathland_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Heathland.csv")
Sparselyvegetated_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Sparselyvegetated.csv")
Wetlands_rnd <- read.csv("~/Desktop/Data/Random_samples/Out_Wetlands.csv")

#### Agriculture ####

Agriculture_Eval <- data.frame(LCPerc = 1:100,
                               Agriculture.Provision = tapply(Agriculture_rnd$Provision,Agriculture_rnd$LCPercent,mean,na.rm=T),
                               Agriculture.Timber = tapply(Agriculture_rnd$Timber,Agriculture_rnd$LCPercent,mean,na.rm=T),
                               Agriculture.Carbon = tapply(Agriculture_rnd$Carbon,Agriculture_rnd$LCPercent,mean,na.rm=T),
                               Agriculture.Pollination = tapply(Agriculture_rnd$Pollination,Agriculture_rnd$LCPercent,mean,na.rm=T),
                               Agriculture.Outdoor = tapply(Agriculture_rnd$Outdoor,Agriculture_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Agriculture_Eval$Agriculture.Provision[1],Agriculture_Eval$Agriculture.Provision)
x2 <- c(Agriculture_Eval$Agriculture.Timber[1],Agriculture_Eval$Agriculture.Timber)
x3 <- c(Agriculture_Eval$Agriculture.Carbon[1],Agriculture_Eval$Agriculture.Carbon)
x4 <- c(Agriculture_Eval$Agriculture.Pollination[1],Agriculture_Eval$Agriculture.Pollination)
x5 <- c(Agriculture_Eval$Agriculture.Outdoor[1],Agriculture_Eval$Agriculture.Outdoor)

highchart() %>% 
  hc_title(text = "A. Agriculture", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))


#### Artificial ####

Artificial_Eval <- data.frame(LCPerc = 1:100,
                               Artificial.Provision = tapply(Artificial_rnd$Provision,Artificial_rnd$LCPercent,mean,na.rm=T),
                               Artificial.Timber = tapply(Artificial_rnd$Timber,Artificial_rnd$LCPercent,mean,na.rm=T),
                               Artificial.Carbon = tapply(Artificial_rnd$Carbon,Artificial_rnd$LCPercent,mean,na.rm=T),
                               Artificial.Pollination = tapply(Artificial_rnd$Pollination,Artificial_rnd$LCPercent,mean,na.rm=T),
                               Artificial.Outdoor = tapply(Artificial_rnd$Outdoor,Artificial_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Artificial_Eval$Artificial.Provision[1],Artificial_Eval$Artificial.Provision)
x2 <- c(Artificial_Eval$Artificial.Timber[1],Artificial_Eval$Artificial.Timber)
x3 <- c(Artificial_Eval$Artificial.Carbon[1],Artificial_Eval$Artificial.Carbon)
x4 <- c(Artificial_Eval$Artificial.Pollination[1],Artificial_Eval$Artificial.Pollination)
x5 <- c(Artificial_Eval$Artificial.Outdoor[1],Artificial_Eval$Artificial.Outdoor)

highchart() %>% 
  hc_title(text = "B. Artificial", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))

#### Forest ####

Forest_Eval <- data.frame(LCPerc = 1:100,
                              Forest.Provision = tapply(Forest_rnd$Provision,Forest_rnd$LCPercent,mean,na.rm=T),
                              Forest.Timber = tapply(Forest_rnd$Timber,Forest_rnd$LCPercent,mean,na.rm=T),
                              Forest.Carbon = tapply(Forest_rnd$Carbon,Forest_rnd$LCPercent,mean,na.rm=T),
                              Forest.Pollination = tapply(Forest_rnd$Pollination,Forest_rnd$LCPercent,mean,na.rm=T),
                              Forest.Outdoor = tapply(Forest_rnd$Outdoor,Forest_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Forest_Eval$Forest.Provision[1],Forest_Eval$Forest.Provision)
x2 <- c(Forest_Eval$Forest.Timber[1],Forest_Eval$Forest.Timber)
x3 <- c(Forest_Eval$Forest.Carbon[1],Forest_Eval$Forest.Carbon)
x4 <- c(Forest_Eval$Forest.Pollination[1],Forest_Eval$Forest.Pollination)
x5 <- c(Forest_Eval$Forest.Outdoor[1],Forest_Eval$Forest.Outdoor)

highchart() %>% 
  hc_title(text = "C. Forest", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))

#### Grassland ####

Grassland_Eval <- data.frame(LCPerc = 1:100,
                              Grassland.Provision = tapply(Grassland_rnd$Provision,Grassland_rnd$LCPercent,mean,na.rm=T),
                              Grassland.Timber = tapply(Grassland_rnd$Timber,Grassland_rnd$LCPercent,mean,na.rm=T),
                              Grassland.Carbon = tapply(Grassland_rnd$Carbon,Grassland_rnd$LCPercent,mean,na.rm=T),
                              Grassland.Pollination = tapply(Grassland_rnd$Pollination,Grassland_rnd$LCPercent,mean,na.rm=T),
                              Grassland.Outdoor = tapply(Grassland_rnd$Outdoor,Grassland_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Grassland_Eval$Grassland.Provision[1],Grassland_Eval$Grassland.Provision)
x2 <- c(Grassland_Eval$Grassland.Timber[1],Grassland_Eval$Grassland.Timber)
x3 <- c(Grassland_Eval$Grassland.Carbon[1],Grassland_Eval$Grassland.Carbon)
x4 <- c(Grassland_Eval$Grassland.Pollination[1],Grassland_Eval$Grassland.Pollination)
x5 <- c(Grassland_Eval$Grassland.Outdoor[1],Grassland_Eval$Grassland.Outdoor)

highchart() %>% 
  hc_title(text = "D. Grassland", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))


#### Heathland ####

Heathland_Eval <- data.frame(LCPerc = 1:100,
                              Heathland.Provision = tapply(Heathland_rnd$Provision,Heathland_rnd$LCPercent,mean,na.rm=T),
                              Heathland.Timber = tapply(Heathland_rnd$Timber,Heathland_rnd$LCPercent,mean,na.rm=T),
                              Heathland.Carbon = tapply(Heathland_rnd$Carbon,Heathland_rnd$LCPercent,mean,na.rm=T),
                              Heathland.Pollination = tapply(Heathland_rnd$Pollination,Heathland_rnd$LCPercent,mean,na.rm=T),
                              Heathland.Outdoor = tapply(Heathland_rnd$Outdoor,Heathland_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Heathland_Eval$Heathland.Provision[1],Heathland_Eval$Heathland.Provision)
x2 <- c(Heathland_Eval$Heathland.Timber[1],Heathland_Eval$Heathland.Timber)
x3 <- c(Heathland_Eval$Heathland.Carbon[1],Heathland_Eval$Heathland.Carbon)
x4 <- c(Heathland_Eval$Heathland.Pollination[1],Heathland_Eval$Heathland.Pollination)
x5 <- c(Heathland_Eval$Heathland.Outdoor[1],Heathland_Eval$Heathland.Outdoor)

highchart() %>% 
  hc_title(text = "E. Heathland", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))


#### Sparselyvegetated ####

Sparselyvegetated_Eval <- data.frame(LCPerc = 1:100,
                              Sparselyvegetated.Provision = tapply(Sparselyvegetated_rnd$Provision,Sparselyvegetated_rnd$LCPercent,mean,na.rm=T),
                              Sparselyvegetated.Timber = tapply(Sparselyvegetated_rnd$Timber,Sparselyvegetated_rnd$LCPercent,mean,na.rm=T),
                              Sparselyvegetated.Carbon = tapply(Sparselyvegetated_rnd$Carbon,Sparselyvegetated_rnd$LCPercent,mean,na.rm=T),
                              Sparselyvegetated.Pollination = tapply(Sparselyvegetated_rnd$Pollination,Sparselyvegetated_rnd$LCPercent,mean,na.rm=T),
                              Sparselyvegetated.Outdoor = tapply(Sparselyvegetated_rnd$Outdoor,Sparselyvegetated_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Sparselyvegetated_Eval$Sparselyvegetated.Provision[1],Sparselyvegetated_Eval$Sparselyvegetated.Provision)
x2 <- c(Sparselyvegetated_Eval$Sparselyvegetated.Timber[1],Sparselyvegetated_Eval$Sparselyvegetated.Timber)
x3 <- c(Sparselyvegetated_Eval$Sparselyvegetated.Carbon[1],Sparselyvegetated_Eval$Sparselyvegetated.Carbon)
x4 <- c(Sparselyvegetated_Eval$Sparselyvegetated.Pollination[1],Sparselyvegetated_Eval$Sparselyvegetated.Pollination)
x5 <- c(Sparselyvegetated_Eval$Sparselyvegetated.Outdoor[1],Sparselyvegetated_Eval$Sparselyvegetated.Outdoor)

highchart() %>% 
  hc_title(text = "F. Sparsely vegetated", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))

#### Wetlands ####

Wetlands_Eval <- data.frame(LCPerc = 1:100,
                              Wetlands.Provision = tapply(Wetlands_rnd$Provision,Wetlands_rnd$LCPercent,mean,na.rm=T),
                              Wetlands.Timber = tapply(Wetlands_rnd$Timber,Wetlands_rnd$LCPercent,mean,na.rm=T),
                              Wetlands.Carbon = tapply(Wetlands_rnd$Carbon,Wetlands_rnd$LCPercent,mean,na.rm=T),
                              Wetlands.Pollination = tapply(Wetlands_rnd$Pollination,Wetlands_rnd$LCPercent,mean,na.rm=T),
                              Wetlands.Outdoor = tapply(Wetlands_rnd$Outdoor,Wetlands_rnd$LCPercent,mean,na.rm=T)
) 

x1 <- c(Wetlands_Eval$Wetlands.Provision[1],Wetlands_Eval$Wetlands.Provision)
x2 <- c(Wetlands_Eval$Wetlands.Timber[1],Wetlands_Eval$Wetlands.Timber)
x3 <- c(Wetlands_Eval$Wetlands.Carbon[1],Wetlands_Eval$Wetlands.Carbon)
x4 <- c(Wetlands_Eval$Wetlands.Pollination[1],Wetlands_Eval$Wetlands.Pollination)
x5 <- c(Wetlands_Eval$Wetlands.Outdoor[1],Wetlands_Eval$Wetlands.Outdoor)

highchart() %>% 
  hc_title(text = "G. Wetlands", align = "left", style = list(fontSize = "30")) %>% 
  hc_add_series(name = "Crop provision", data = x1, 
                color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
  hc_add_series(name = "Timber provision",data = x2, 
                yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
  hc_add_series(name = "Carbon sequestration",data = x3, 
                yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
  hc_add_series(name = "Crop pollination",data = x4, 
                yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5]) %>%
  hc_add_series(name = "Nature-based recreation",data = x5, 
                yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
  
  hc_xAxis(title = list(text = "Land Cover [%]", style=list(fontWeight ="bold", fontSize="20")), 
           labels = list(style = list(fontWeight = "", fontSize = "20")), 
           plotBands = list(list(from = 25, to = 50, 
                                 color = "rgba(100, 100, 100, 0.1)"),
                            list(from = 75, to = 100, 
                                 color = "rgba(100, 100, 100, 0.1)"))) %>%
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
         title=list(text="Crop provision [Tonne/ha in 2012]",
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
         title=list(text="Timber provision [m3/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
         title=list(text="Carbon sequestration [Tonne CO2/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
         title=list(text="Crop pollination [Tonne/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))),
    list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
         title=list(text="Nature-based recreation [Visits/km2 in 2012]", 
                    style=list(fontWeight ="bold", fontSize="19")),
         labels = list(rotation = 270,style = list(fontWeight = "", fontSize=20))))%>%
  hc_legend(align = "center",
            verticalAlign = "bottom",
            layout = "horizontal",
            itemStyle = list(fontSize = 20))


#_______________________________________________________________________________

# highchart() %>% 
#   hc_title(text = "Agriculture", align = "left") %>% 
#   hc_add_series(name = "Crop Provision Tonne/ha in 2012", data = x1, color = hcl.colors(5,"YlOrBr", rev = T)[3])%>% 
#   hc_add_series(name = "Timber Provision m3/km2 in 2012",data = x2, yAxis = 1, color = hcl.colors(5,"Greens", rev = T)[5]) %>% 
#   hc_add_series(name = "Carbon Sequestration Tonne CO2/km2 in 2012",data = x3, yAxis = 2, color = hcl.colors(5,"Purples", rev = T)[4]) %>%
#   hc_add_series(name = "Crop Pollination Tonne/km2 in 2012",data = x4, yAxis = 3, color = hcl.colors(5,"Peach", rev=T)[5],yAxis = 2) %>%
#   hc_add_series(name = "Outdoor Recreation Visits/km2 in 2012",data = x5, yAxis = 4, color = hcl.colors(5,"ag_GrnYl",rev = T)[2]) %>%
#   hc_xAxis(title = list(text = "Land Cover [%]", style="bold"), labels = list(style = list(fontWeight = "bold", fontSize = "15")),plotBands = list(
#     list(from = 25, to = 50, color = "rgba(100, 100, 100, 0.1)"),list(from = 75, to = 100, color = "rgba(100, 100, 100, 0.1)")))%>%
#   
#   hc_yAxis_multiples(
#     list(lineWidth = 3, lineColor=hcl.colors(5,"YlOrBr", rev = T)[3], 
#          labels = list(rotation = 270, style = list(color = "", fontWeight = "")), 
#          title=list(text="Tonne/ha in 2012", 
#                     style=list(fontWeight="bold", color = "orange"))),
#     
#     list(lineWidth = 3, lineColor=hcl.colors(5,"Greens", rev = T)[5], 
#          labels = list(rotation = 270,style = list(color =  hcl.colors(5,"Greens", rev = T)[5], fontWeight = "bold")), 
#          title=list(text="m3/km2 in 2012", 
#                     style="bold")),
#     
#     list(lineWidth = 3, lineColor=hcl.colors(5,"Purples", rev = T)[4], 
#          labels = list(rotation = 270,style = list(color =  hcl.colors(5,"Purples", rev = T)[4], fontWeight = "bold")), 
#          title=list(text="Tonne CO2/km2 in 2012", 
#                     style="bold")),
#     
#     list(lineWidth = 3, lineColor=hcl.colors(5,"Peach", rev=T)[5], 
#          labels = list(rotation = 270,style = list(color =  hcl.colors(5,"Peach", rev = T)[4], fontWeight = "bold")), 
#          title=list(text="Tonne/km2 in 2012", 
#                     style="bold")),
#     
#     list(lineWidth = 3, lineColor=hcl.colors(5,"ag_GrnYl",rev = T)[2], 
#          labels = list(rotation = 270,style = list(color =  hcl.colors(5,"ag_GrnYl", rev = T)[2], fontWeight = "bold")),
#          title=list(text="Visits/km2 in 2012", 
#                     style="bold"))
#   )


