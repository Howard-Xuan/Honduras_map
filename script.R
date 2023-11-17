library(tidyverse)

mi_counties<-map_data("county","michigan")%>%
  select(lon=long,lat,group,id=subregion)

ggplot(mi_counties,aes(lon,lat))+
  geom_point()+
  coord_quickmap()

ggplot(mi_counties,aes(lon,lat,group=group))+
  geom_polygon(fill="white",colour="grey50")+
  coord_quickmap()

library(ozmaps)
library(sf)

oz_states<-ozmaps::ozmap_states

ggplot(oz_states)+
  geom_sf()+
  coord_sf()
oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")
oz_votes <- rmapshaper::ms_simplify(ozmaps::abs_ced)

ggplot() + 
  geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE) +
  geom_sf(data = oz_votes, fill = NA) + 
  coord_sf()

sydney_map<-abs_ced%>%filter(NAME %in% c(
  "Sydney", "Wentworth", "Warringah", "Kingsford Smith", "Grayndler", "Lowe", 
  "North Sydney", "Barton", "Bradfield", "Banks", "Blaxland", "Reid", 
  "Watson", "Fowler", "Werriwa", "Prospect", "Parramatta", "Bennelong", 
  "Mackellar", "Greenway", "Mitchell", "Chifley", "McMahon"
))

ggplot(sydney_map)+
  geom_sf(aes(fill=NAME),show.legend = FALSE)+
  coord_sf(xlim=c(150.5,151.5),ylim=c(-33.5,-34))+
  geom_sf_label(aes(label=NAME),label.padding = unit(1,"mm"))

library('rjson')

honduras<-fromJSON(file='hondurasmunicipios.json')
honduras<-as.data.frame(honduras)
  