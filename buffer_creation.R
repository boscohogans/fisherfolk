#Import datasets

#List all shapefiles
list_shape <- list.files(path="data//Shapefiles", pattern = "_1.shp", full.names = TRUE)

#Read in country files
countries <- do.call(rbind, lapply(list_shape,sf::st_read)) %>%
  subset(., subset=NAME_0 %in% c("Malawi", "Zambia", "Kenya", "Tanzania", "Uganda"))

#Check projection
st_crs(countries)

#Make colour palette
pal <- colorFactor(palette = "Set1",
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
  subset(., subset=DHSYEAR %in% c("2008",  "2010",  "2011",  "2013",  "2014",  "2015",  "2016")) %>% 
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
lakes_buffer_5 <- st_buffer(lakes_km, 5)
lakes_buffer_25 <- st_buffer(lakes_km, 2.5)
lakes_buffer_125 <- st_buffer(lakes_km, 1.25)

#village_buffer <- st_buffer(villages_km, 5)

#Identify villages that fall within buffer
lakes_villages_5 <- st_intersection(lakes_buffer_5, villages_km)
lakes_villages_25 <- st_intersection(lakes_buffer_25, villages_km)
lakes_villages_125 <- st_intersection(lakes_buffer_125, villages_km)

#villages_lakes <- st_intersection(village_buffer, lakes_km)
fish_village_5 <- as.data.frame(lakes_villages_5) %>% 
  select(DHSID, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME) %>%
  mutate(fishing_community_5=1)
fish_village_25 <- as.data.frame(lakes_villages_25) %>% 
  select(DHSID, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME) %>%
  mutate(fishing_community_25=1)
fish_village_125 <- as.data.frame(lakes_villages_125) %>% 
  select(DHSID, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME) %>%
  mutate(fishing_community_125=1)

#Join list of villages to list of fishing villages
fish_village_final <- villages %>%
  left_join(fish_village_5, by=c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST")) %>% 
  left_join(fish_village_25, by=c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST")) %>% 
  left_join(fish_village_125, by=c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST")) %>% 
  select(starts_with("DHS"), starts_with("fishing_community"), LAKE_NAME.x)

# fish_pal <- colorFactor(palette = "Set3",
#                    domain = fish_village_final$fishing_community)
# 
# #Map for paper (Is this needed?)
# map_1 <- leaflet() %>%
#   addPolygons(
#     data = countries,
#     color = ~ pal(NAME_0),
#     fillOpacity = 0.4,
#     stroke = FALSE,
#     label = ~ NAME_0
#   ) %>%
#   addPolygons(
#     data = lakes,
#     label = ~ LAKE_NAME)
# 
# 
# #Check that data can map
# fish_map <- leaflet() %>%
#   addPolygons(
#     data = countries,
#     color = ~ pal(NAME_0),
#     fillOpacity = 0.4,
#     stroke = FALSE,
#     label = ~ NAME_0
#   ) %>%
#   addPolygons(
#     data = lakes,
#     label = ~ LAKE_NAME) %>%
#   addCircles(data = fish_village_final,
#              radius=5000,
#              color = ~fish_pal(fishing_community_5)) 
# 
# #write.csv(lakes_villages,  "data//rural_intersection_5km_20200121.csv")
# 
# fish_village <- as.data.frame(fish_village_final)
# 
# fish_village %>% 
#   mutate(count=1) %>% 
#   janitor::clean_names() %>% 
#   filter(!is.na(lake_name_x)) %>% 
#   count(dhscc,dhsid, lake_name_x) %>% 
#   mutate(Country=case_when(
#     dhscc=="KE" ~ "Kenya",
#     dhscc=="MW" ~ "Malawi",
#     dhscc=="TZ" ~ "Tanzania",
#     dhscc=="UG" ~ "Uganda",
#     dhscc=="ZM" ~ "Zambia")) %>% 
#   mutate(Lake=ifelse(lake_name_x=="Lake Victoria", "Victoria", as.character(lake_name_x))) %>% 
#   select(Lake, Country,n) %>% 
#   reshape2::dcast(Lake  ~ Country , value.var = "n") %>% 
#   janitor::adorn_totals("row","col") %>% 
#   janitor::adorn_totals("col") 
# 
# 
# 
# fish_lake_country[is.na(fish_lake_country)] <- 0
# 
# fish_lake_country %>% 
#   sjPlot::tab_df(title="Fishing community by lake and country", file="outputs//fish_lake_country.doc")


rm(list=ls()[! ls() %in% c("fish_village_final")])


