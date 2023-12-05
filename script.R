library(tidyverse)
library(leaflet) # the main mapping library
library(htmltools) # use html to format popups
library(htmlwidgets) # same the map as an html page
library(geojsonio) # deal with geojson files
library(stringi) # deal with accents indifferent languages
library(readxl)

# Import data and preliminary standardization
Honduras <- read_xlsx("HND.xlsx",c(6))
Honduras<-filter(Honduras,threshold==30)
Honduras<-select(Honduras,c(1:3,30))
Honduras$subnational2<-stri_trans_general(Honduras$subnational2,
                                          id="Latin-ASCII")
Honduras$subnational2<-Honduras$subnational2%>%str_replace_all("Ojo de Agua","Ojos de Agua")
match("Macuelizo",Honduras$subnational2)
Honduras[262,4]<-115
Honduras[262,3]<-"Macuelizo and Nueva Frontera" #add Nueva Frontera and Macuelizo together in data set since Nueva Frontera isn't in geo file
Honduras<-Honduras[-c(264),] #remove Nueva Frontera
Lago_de_Yojoa<-tibble(country="Honduras",subnational1="Cortes",subnational2="Lago de Yojoa",tc_loss_ha_2022=NA)#add Lago de Yojoa since it's in geojson file
Honduras<-rbind(Honduras,Lago_de_Yojoa)

# Import geojson file and preliminary standardization
# Geojson file obtained from https://data.humdata.org/ 
Honduras_geo <- geojson_read("Honduras_adm2.geojson",
                           what = "sp") 
Honduras_geo$shapeName<-stri_trans_general(Honduras_geo$shapeName,
                                           id="Latin-ASCII")
match("Macuelizo",Honduras_geo$shapeName)
Honduras_geo@data[256,1]<-"Macuelizo and Nueva Frontera"

#Compare Geo and Data file names and cleaning data
Honduras_Adm2<-tibble(sort(Honduras$subnational2))
Honduras_geo_Adm2<-tibble(sort(Honduras_geo$shapeName))
GeoAdm2vsAdm2<-cbind(Honduras_Adm2,Honduras_geo_Adm2)
GeoAdm2vsAdm2$match<-ifelse(GeoAdm2vsAdm2$`sort(Honduras$subnational2)`==GeoAdm2vsAdm2$`sort(Honduras_geo$shapeName)`,
                            1,0)
match("Aguaqueterique",Honduras$subnational2)
Honduras[165,3]<-"Aguanqueterique"
match("Cabana",Honduras_geo$shapeName)
Honduras_geo@data[41,1]<-"Cabanas"
match("Juan Francisco  Bulnes",Honduras_geo$shapeName)
Honduras_geo@data[295,1]<-"Juan Francisco Bulnes"
match("San Francisco de La Paz",Honduras_geo$shapeName)
Honduras_geo@data[240,1]<-"San Francisco de la Paz"
match("San Marcos de Sierra",Honduras_geo$shapeName)
Honduras_geo@data[150,1]<-"San Marcos de la Sierra"

#Adding data to geojson
Honduras_geo@data$tc_loss_ha_2022 <- Honduras$tc_loss_ha_2022[match(Honduras_geo$shapeName,
                                                               Honduras$subnational2)]
colnames(Honduras_geo@data)[6]<-"Tree Cover Loss (Hectare) in 2022"

#Labels
labels <- ifelse(Honduras_geo$shapeName!="Lago de Yojoa",
                 sprintf("<strong>%s</strong><br>%g hectares of forest cover lost in 2022",
                 Honduras_geo$shapeName,Honduras_geo$`Tree Cover Loss (Hectare) in 2022`),
                 sprintf("<strong>%s</strong>",Honduras_geo$shapeName)
) %>% lapply(htmltools::HTML)

#Palette
pal <- colorBin(c("#FAFAFA", "#B31536"),
              domain = c(0, max(Honduras_geo$`Tree Cover Loss (Hectare) in 2022`,na.rm=TRUE)),
              bins=5,
              na.color = "#DFF1F8")

# leaflet builds the underlying map and adds our dataset to it
leaf<-leaflet(Honduras_geo, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron, 
                   options = providerTileOptions(minZoom = 7)) %>%
  addPolygons(
    fillColor = ~pal(`Tree Cover Loss (Hectare) in 2022`),
    weight = 0.1,
    opacity = 1,
    color = "#3B3B3B",
    fillOpacity = .6,
    highlightOptions = highlightOptions(
      weight = 2,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend("bottomleft", pal = pal, values = ~`Tree Cover Loss (Hectare) in 2022`)

leaf

htmlwidgets::saveWidget(leaf, "Honduras_tc_loss2022.html")
