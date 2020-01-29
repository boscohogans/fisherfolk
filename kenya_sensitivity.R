#Import datasets
mr_sensitivty_analysis <- readstata13::read.dta13("data\\DHS\\MR\\KEMR70FL.DTA") %>% 
  #Select variables of interest
  select(mv025, mv716, mv000, mv007, mv001) %>% 
  #Filter on rural communities
  filter(mv025=="rural") %>% 
  #Generate new variables for identifying fisherfolk and country
  mutate(fishing=ifelse(grepl("^fishery",mv716),1,0),
         country=substr(mv000,1,2)) %>%
  #Merge with fishing clusters
  left_join(fish_village_final, by=c("country"="DHSCC", "mv007"="DHSYEAR", "mv001"="DHSCLUST")) %>% 
  tidyr::extract(geometry, c('lon','lat'), '\\((.*), (.*)\\)', convert = TRUE) %>% 
  filter(!is.na(mv716))

ir_sensitivty_analysis <- data.table::fread("data\\DHS\\IR\\keir72fl.csv") %>% 
  #Select variables of interest
  select(v025, v716, v000, v007, v001) %>% 
  #Filter on rural communities
  filter(v025=="rural") %>% 
  #Generate new variables for identifying fisherfolk and country
  mutate(fishing=ifelse(grepl("^fishery",v716),1,0),
         country=substr(v000,1,2)) %>% 
#Merge with fishing clusters
left_join(fish_village_final, by=c("country"="DHSCC", "v007"="DHSYEAR", "v001"="DHSCLUST")) %>% 
  tidyr::extract(geometry, c('lon','lat'), '\\((.*), (.*)\\)', convert = TRUE) 

ir_sensitivty_analysis[ir_sensitivty_analysis==""] <- NA

ir_sensitivty_analysis2 <- ir_sensitivty_analysis %>%  filter(!is.na(v716))

all_fisher <- dplyr::bind_rows(mr_sensitivty_analysis, ir_sensitivty_analysis2) %>% 
  filter(fishing==1) %>% 
  mutate(count=1) %>% 
  group_by(DHSID) %>% 
  mutate(cumulative=cumsum(count)) %>% 
  filter(cumulative==1)

rm(ir_sensitivty_analysis,ir_sensitivty_analysis2, mr_sensitivty_analysis)

library(dplyr)
library(sf)

#List all shapefiles
list_shape <- intersect(list.files(path="data//Shapefiles", pattern = "_2.shp", full.names = TRUE),
                        list.files(path="data//Shapefiles", pattern = "KE", full.names = TRUE))

#Read in country files
kenya <- do.call(rbind, lapply(list_shape,sf::st_read)) %>%
  subset(., subset=NAME_0 %in% c("Kenya"))


#Check projection
st_crs(kenya)

#Make colour palette
pal <- colorFactor(palette = "Set1",
                   domain = countries$NAME_0)

#Read in lakes and format file
lakes <-
  sf::st_read(
    "C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/Shapefiles/GLWD-level1/glwd_1.shp") %>% 
  subset(., grepl("Kenya",SEC_CNTRY) | COUNTRY=="Kenya") %>% 
  subset(., !is.na(LAKE_NAME)) %>% 
  #Select variables of interest
  select(., LAKE_NAME , LONG_DEG, LAT_DEG, geometry) %>%
  #Change crs to match shp
  st_as_sf(coords = c("LONG_DEG", "LAT_DEG"), crs=4326)

st_crs(lakes) <- st_crs(kenya)
st_crs(lakes)

#We can import GPS data from DHS surveys
list_shp <- intersect(list.files(path="data//DHS//gps_points",pattern="\\.shp$", full.names = TRUE),
                      list.files(path="data//DHS//gps_points",pattern="KEGE71FL", full.names = TRUE)) 

villages <- do.call(rbind, lapply(list_shp, rgdal::readOGR)) %>%
  subset(., URBAN_RURA=="R") %>%
  subset(., subset=DHSCC %in% c("KE")) %>% 
  subset(., subset=DHSYEAR %in% c("2014")) %>% 
  st_as_sf(coords = c("LONGNUM", "LATNUM"), crs=4326)

villages <-  villages %>%
  select(CCFIPS, DHSID, DHSCC, DHSYEAR, DHSCLUST, LONGNUM, LATNUM, URBAN_RURA, geometry) 

library(leaflet)
#Make colour palette
pal <- colorFactor(palette = "Set1",
                   domain = kenya$NAME_2)

library(htmltools)

leaflet(data = subset(fisher_only2, fishing==1)) %>% 
  addMarkers(~lon, ~lat, label=~htmlEscape(DHSID)) %>% 
  addPolygons(
    data = kenya,
    color = ~ pal(NAME_2),
    fillOpacity = 0.4,
    stroke = FALSE,
    label = ~ NAME_2) %>% 
  addPolygons(
    data = lakes,
    label = ~ LAKE_NAME) %>% 
  addScaleBar(., position = c("topright", "bottomright", "bottomleft",
                              "topleft"), options = scaleBarOptions()) 

##Would like to exclude points on the coast

#Want to get distance from each of the points to a Lake
#str(fisher_only)
#Need to transform it to SF object with came CRS as lakes
fisher_sf <- all_fisher %>% 
  #filter(!grepl("3081|0379", DHSID)) %>% 
  st_as_sf(., coords=c('lon', 'lat'), crs=st_crs(lakes))

str(lakes)

lake_distance <- st_distance(fisher_sf, lakes)

vars <- paste0("X",seq(1,7))

f1 <- function(x){
  y <- sub("([.-])|[[:punct:]]", "\\1", x)
  as.numeric(y)
}

fish_to_lake <- cbind(fisher_sf, lake_distance) %>% 
  mutate_at(vars,f1) %>% 
  select(DHSID, starts_with("X"))

fish_to_lakedf_VICTORIA <- as.data.frame(fish_to_lake) %>% 
  select(X1, DHSID) %>% 
  filter(X1<25000) %>% 
  distinct() %>% 
  #reshape2::melt(id="DHSID") %>% 
  mutate(distance= case_when(
    X1 <=1000 ~"<=1km",
    X1 >1000 & X1<=3000 ~ "1-3km",
    X1 >3000 & X1<=5000 ~ "3-5km",
    X1 >5000 & X1<=10000 ~ "5-10km",
    TRUE ~ "More than 10km"
  ))  %>% 
  rename(tolake=X1) %>% 
  select(DHSID, tolake) %>% 
  mutate(lake="VICTORIA")


fish_to_lakedf_TURKANA <- as.data.frame(fish_to_lake) %>% 
  select(X2, DHSID) %>% 
  filter(X2<25000) %>% 
  distinct() %>% 
  #reshape2::melt(id="DHSID") %>% 
  mutate(distance= case_when(
    X2 <=1000 ~"<=1km",
    X2 >1000 & X2<=3000 ~ "1-3km",
    X2 >3000 & X2<=5000 ~ "3-5km",
    X2 >5000 & X2<=10000 ~ "5-10km",
    TRUE ~ "More than 10km"
  )) %>% 
  rename(tolake=X2) %>% 
  select(DHSID, tolake) %>% 
  mutate(lake="TURKANA")
  
fish_to_lake_COMBINED <- rbind(fish_to_lakedf_TURKANA, fish_to_lakedf_VICTORIA)

t.test(fish_to_lake_COMBINED$tolake, conf.level = 0.95)  
sd(fish_to_lake_COMBINED$tolake)
summary(fish_to_lake_COMBINED$tolake)

library(ggplot2)
ggplot(fish_to_lake_COMBINED, aes(x=tolake, fill=lake)) +
  geom_histogram(bins=250) +
  labs(x="Distance to lake (metres)", 
       y="Number of clusters with at least 1 resident recorded as 'fishery and related workers'") +
  scale_y_continuous(breaks=c(0,1,2))

