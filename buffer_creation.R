#Import datasets

#List all shapefiles
list_shape <- list.files(path="data//Shapefiles", pattern = "_1.shp", full.names = TRUE)

#Read in country files
countries <- do.call(rbind, lapply(list_shape,sf::st_read)) %>%
  subset(., subset=NAME_0 %in% c("Malawi", "Zambia", "Kenya", "Tanzania", "Uganda"))

#Check projection
st_crs(countries)

#Make colour palette
pal <- colorFactor(palette = "Set2",
                   domain = countries$NAME_0)

#List of lakes to keep
great_lakes_list <-
  c("Lake Victoria","Malawi",
    "Tanganyika","Albert",
    "Edward","Kivu",
    "Turkana","Mweru",
    "Rukwa","Kyoga")

#Read in lakes and format file
lakes <-
  sf::st_read(
    "C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/Shapefiles/GLWD-level1/glwd_1.shp") %>%
  #Only keep lakes of interest
  subset(., LAKE_NAME %in% great_lakes_list) %>%
  #Select variables of interest
  select(., LAKE_NAME , LONG_DEG, LAT_DEG, geometry) %>%
  #Change crs to match shp
  st_as_sf(coords = c("LONG_DEG", "LAT_DEG"), crs=4326)


#We can import GPS data from DHS surveys
list_shp <- list.files(path="data//DHS//gps_points",pattern="\\.shp$", full.names = TRUE) 

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

#Identify villages that fall within buffer
lakes_villages <- st_intersection(lakes_buffer, villages_km)

fish_village_df <- as.data.frame(lakes_villages) %>% 
  select(DHSID, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME) %>%
  mutate(fishing_community=1)

#Join list of villages to list of fishing villages
fish_village_final <- villages %>%
  left_join(fish_village_df, by=c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST")) 
  
factpal <- colorFactor(topo.colors(2), fish_village_final$fish)

#Check that data can map
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
             color = ~factpal(fishing_community))

write.csv(lakes_villages,  "data//rural_intersection_5km_20200121.csv")



