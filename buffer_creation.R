#Import datasets

setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/Shapefiles")

library(utils)
library(rgdal)
library(sf)
library(leaflet)
library(dplyr)
library(rgeos)

list_shape <- list.files(pattern = "_1.shp")

countries <- do.call(rbind, lapply(list_shape,sf::st_read)) %>%
  subset(., subset=NAME_0 %in% c("Malawi", "Zambia", "Kenya", "Tanzania", "Uganda"))

#shp_sub <- subset(shp, subset=NAME_0 %in% c("Malawi", "Zambia", "Kenya", "Tanzania", "Uganda"))
st_crs(countries)

pal <- colorFactor(palette = "Set2",
                   domain = countries$NAME_0)

great_lakes_list <-
  c("Lake Victoria",    "Malawi",
    "Tanganyika",    "Albert",
    "Edward",    "Kivu",
    "Turkana",    "Mweru",
    "Rukwa",    "Kyoga")

lakes <-
  sf::st_read(
    "C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/Shapefiles/GLWD-level1/glwd_1.shp") %>%
  #Only keep lakes of interest
  subset(., LAKE_NAME %in% great_lakes_list) %>%
  select(LAKE_NAME, LONG_DEG, LAT_DEG, geometry)
  #Change crs to match shp
  #st_set_crs(., 4326) %>%
  st_as_sf(coords = c("LONG_DEG", "LAT_DEG"), crs=4326)


#We can import GPS data from DHS surveys
setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS")

list_shp <- list.files(path='C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS',pattern="\\.shp$") 


villages <- do.call(rbind, lapply(list_shp, rgdal::readOGR)) %>%
  subset(., URBAN_RURA=="R") %>%
  subset(., subset=DHSCC %in% c("KE","MW", "TZ", "UG", "ZM")) %>%
  st_as_sf(coords = c("LONGNUM", "LATNUM"), crs=4326)

villages <-  villages %>%
  select(CCFIPS, DHSID, DHSCC, DHSYEAR, DHSCLUST, LONGNUM, LATNUM, URBAN_RURA, geometry) 

#Constructing buffers around the lakes to identify fishing villages
#https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea

#Copy co-ordinate system from countries to other files
st_crs(villages) <- st_crs(countries)
st_crs(lakes) <- st_crs(countries)

#Transform to metric coordinate systme
lakes_km = st_transform(lakes, "+proj=utm +zone=42N +datum=WGS84 +units=km")
villages_km = st_transform(villages, "+proj=utm +zone=42N +datum=WGS84 +units=km")

#Create 5km buffer zone
lakes_buffer <- st_buffer(lakes_km, 5)
lakes_villages <- st_intersection(lakes_buffer, villages_km)

fish_village_df <- as.data.frame(lakes_villages) %>% 
  select(DHSID, DHSCC, DHSYEAR, DHSCLUST) %>%
  mutate(fish=1)

fish_village_final <- villages %>%
  left_join(fish_village_df, by=c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST")) 
  
factpal <- colorFactor(topo.colors(2), fish_village_final$fish)


fish_map <- leaflet() %>%
  addPolygons(
    data = countries,
    color = ~ pal(NAME_0),
    fillOpacity = 0.4,
    stroke = FALSE,
    label = ~ NAME_0
  ) %>%
  addPolygons(
    data = lakes,
    label = ~ LAKE_NAME) %>%
  addCircles(data = fish_village_final,
             color = ~factpal(fish))

write.csv(lakes_villages,
          "C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/Shapefiles/Merged/rural_intersection_5km_20200121.csv")

