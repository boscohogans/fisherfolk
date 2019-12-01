library(dplyr)

#Building DHS datasets
countries <- c("KE", "MW", "TZ", "UG", "ZM")
library(data.table)
#Read in all GPS Points
all_gps <- fread("data//gps_dhs_rural.csv")[DHSCC %in% countries]
#drop if longnum==0
#Read in all fishin communities 
rural_5km <- fread("data//rural_intersection_5km_20190815.csv")[DHSCC %in% countries]
#Generate fishing community variable
rural_5km$fishing_community <- 1

fish_gps <- all_gps %>%
  #Join fishing communities to all GPS points
  #left_join(select(rural_5km, DHSCC, DHSYEAR, DHSCLUST, fishing_community, LAKE_NAME), 
  left_join(rural_5km, 
           c("DHSCC", "DHSYEAR", "DHSCLUST"))%>%
  #Select columns for further analysis
  #select(DHSID.x, DHSCC, DHSYEAR, DHSCLUST, LAKE_NAME, LATNUM.x, LONGNUM.x, fishing_community) %>%
  #Generate hv000 based on survey year and country
  mutate(hv000 = ifelse((DHSYEAR==2003 & DHSCC=="KE"),"KE4",
         ifelse((DHSYEAR==2008 & DHSCC=="KE"),"KE5",
         ifelse((DHSYEAR==2014 & DHSCC=="KE"),"KE6",
         ifelse((DHSYEAR==2010 & DHSCC=="MW"),"MW5",
         ifelse((DHSYEAR==2015 & DHSCC=="MW"),"MW7",
         ifelse((DHSYEAR==2010 & DHSCC=="TZ"),"TZ5",
         ifelse((DHSYEAR==2015 & DHSCC=="TZ"),"TZ7",
         ifelse((DHSYEAR==2006 & DHSCC=="UG"),"UG5",
         ifelse((DHSYEAR==2011 & DHSCC=="UG"),"UG6",
         ifelse((DHSYEAR==2016 & DHSCC=="UG"),"UG7",
         ifelse((DHSYEAR==2007 & DHSCC=="ZM"),"ZM5",
         ifelse((DHSYEAR==2013 & DHSCC=="ZM"),"ZM6",
         NA ))))))))))))) %>%
  filter(DHSYEAR !="")

rm(all_gps, rural_5km)
#keep dhs* lat* long* lake_name

###Household data
#Import HR DHS data
hr <- list.files("data/DHS//HR", pattern=".csv", full.names = TRUE)

keep_hr <- c("zmhr6", "zmhr5", "ughr7", "ughr6", "tzhr7", "tzhr6", "mwhr7", "mwhr6", "kehr7", "kehr5")

hr_files <- hr[grep(paste(keep_hr, collapse="|"),hr)]
hr_vars_to_keep <- c("hhid","hv000","hv001","hv002","hv005","hv007","hv009","hv014",
                     "hv021","hv025","hv201","hv204","hv205","hv206", "hv207", "hv208", 
                     "hv243a", "hv221", "hv209", "sh110l", "sh110i", "sh110j", "sh110n", 
                     "sh110v", "hv243b", "sh118g", "hv210", "hv211", "hv243c", "hv212", 
                     "hv243d", "hv244", "hv213","hv214","hv215",
                     "hv216","hv225","hv243","hv246*","hv270","hv271",
                     "hv246a","hv246c", "hv246d", "hv246e","hv246f", "hv246g", "sh122a", "sh122b", "sh122c")

all_hr <- lapply(hr_files, fread, select=hr_vars_to_keep, header=TRUE)

names (all_hr) <- keep_hr

all_hr_combined <- rbindlist(all_hr, idcol="fill", fill=TRUE)

hr_data <- all_hr_combined %>%
  mutate(dhsclust=hv001,
         dhsyear=hv007,
         dhsyear= ifelse(hv000=="KE5", 2008,
                  ifelse(hv000=="MW4", 2005,
                  ifelse(hv000=="MW7", 2015,
                  ifelse(hv000=="TZ5", 2010,
                  ifelse(hv000=="TZ7", 2015,
                  ifelse(hv000=="UG4",2000,
                  ifelse(hv000=="ZM6", 2013,
                  dhsyear))))))),
         dhsround=substr(hv000,3,1),
         country=substr(hv000,1,2)) 

library(sqldf)
hr_fish_gps <- sqldf("Select hr_data.*, fish_gps.fishing_community
                     from hr_data
                     left join fish_gps
                     on hr_data.country=fish_gps.COUNTRY
                     and hr_data.dhsyear=fish_gps.dhsyear
                     and hr_data.dhsclust=fish_gps.DHSCLUST")

rm(all_hr, all_hr_combined, hr_data, hr, hr_files, hr_vars_to_keep)

setDT(hr_fish_gps, key="hhid")
###Make HR variables 
#Improved water source
hr_fish_gps$hv201 <- tolower(hr_fish_gps$hv201)

hr_fish_gps[, water_imp := ifelse(grepl("piped", hv201), 1,
                                  ifelse(grepl("\\bprotected\\b", hv201), 1,
                                         ifelse(
                                           grepl("\\brain\\b", hv201), 1,
                                           ifelse(
                                             grepl("\\bottled\\b", hv201),
                                             1,
                                             ifelse(hv201 == "public tap/standpipe", 1, 0)
                                           ))))]

#Time to water source
hr_fish_gps[, water_on_premises := ifelse((hv204 == 996 &
                                             water_imp == 1), 1, 0)]
hr_fish_gps[, less_than_5 := ifelse((hv204 <= 5 &
                                       water_imp == 1), 1, 0)]
hr_fish_gps[, less_than_30 := ifelse((hv204 <= 30 &
                                        water_imp == 1), 1, 0)]


#Improved sanitation
hr_fish_gps$hv205 <- tolower(hr_fish_gps$hv205)

hr_fish_gps[, san_imp :=   ifelse(grepl("\\bflush\\b", hv205), 1,
                                  ifelse(
                                    grepl("latrine", hv205),
                                    1,
                                    ifelse(hv205 == "composting toilet", 1, 0)
                                  ))]
#Improved housing
# https://www.nature.com/articles/s41586-019-1050-5.pdf
#Number of people per bedroom
hr_fish_gps[, ppl_per_bedroom := round((hv009 / hv216), 0)]
hr_fish_gps[, three_per_bedroom := ifelse((hv009 / hv216 > 3), 1, 0)]

#Roof finished
hr_fish_gps$hv215 <- tolower(hr_fish_gps$hv215)
hr_fish_gps[, roof_finished := ifelse(grepl("concrete", hv215), 1,
                                      ifelse(grepl("cement", hv215), 1,
                                             ifelse(
                                               grepl("asbestos", hv215), 1,
                                               ifelse(grepl("tiles", hv215), 1,
                                                      ifelse(
                                                        grepl("shingles", hv215), 1,
                                                        ifelse(grepl("iron", hv215), 1,
                                                               ifelse(grepl("ceramic", hv215), 1, 0))
                                                      )))))]

hr_fish_gps$hv214 <- tolower(hr_fish_gps$hv214)
hr_fish_gps[, wall_finished := ifelse(grepl("bricks", hv214), 1,
                                      ifelse(grepl("cement", hv214), 1,
                                             ifelse(
                                               grepl("metal", hv214), 1,
                                               ifelse(grepl("sheets", hv214), 1, 0)
                                             )))]

hr_fish_gps$hv213 <- tolower(hr_fish_gps$hv213)
hr_fish_gps[, floor_finished := ifelse(grepl("bricks", hv213), 1,
                                       ifelse(grepl("cement", hv213), 1,
                                              ifelse(
                                                grepl("metal", hv213), 1,
                                                ifelse(grepl("tiles", hv213), 1,
                                                       ifelse(
                                                         grepl("parquet", hv213), 1,
                                                         ifelse(grepl("vinyl", hv213), 1,
                                                                ifelse(grepl("sheets", hv213), 1, 0)))))))]

hr_fish_gps[, housing_unfinished_material := ifelse((roof_finished + wall_finished + floor_finished) <2, 1, 0)]

hr_fish_gps[, housing_unimp := ifelse((water_imp != 1 |  san_imp != 1 |  three_per_bedroom == 1 | housing_unfinished_material == 1),1,0)]

hr_fish_gps$housing_imp <- (1 - hr_fish_gps$housing_unimp)


####Childhood variables 
###Household data
#Import HR DHS data
kr <- list.files("data/DHS//KR", pattern=".csv", full.names = TRUE)

keep_kr <- c("zmkr6", "zmkr5", "ugkr7", "ugkr6", "tzkr7", "tzkr6", "mwkr7", "mwkr6", "kekr7", "kekr5")

kr_files <- kr[grep(paste(keep_kr, collapse="|"),kr)]
kr_vars_to_keep <- c("caseid", "v000", "v001", "v002", "v007",
                      "v009", "v012", "v021", "v106", "v133" ,
                     "v135", "v136", "v137", "h6", "h7", "h8", "h9",
                     "b5", "h10", "h11",  "h22", "h31",  "v404")


all_kr <- lapply(kr_files, fread, select=kr_vars_to_keep, header=TRUE)

names (all_kr) <- keep_kr

all_kr_combined <- rbindlist(all_kr, idcol="fill", fill=TRUE)

kr_data <- all_kr_combined %>%
  mutate(dhsclust=v001,
         dhsyear=v007,
         dhsyear= ifelse(v000=="KE5", 2008,
                         ifelse(v000=="MW4", 2005,
                                ifelse(v000=="MW7", 2015,
                                       ifelse(v000=="TZ5", 2010,
                                              ifelse(v000=="TZ7", 2015,
                                                     ifelse(v000=="UG4",2000,
                                                            ifelse(v000=="ZM6", 2013,
                                                                   dhsyear))))))),
         dhsround=substr(v000,3,1),
         country=substr(v000,1,2))  %>%
   #only keep record if child is alive and resident
        filter((b5=="Yes"|b5=="yes") & (v135=="usual resident"|v135=="Usual resident"))


library(sqldf)
kr_fish_gps <- sqldf("Select kr_data.*, fish_gps.fishing_community
                     from kr_data
                     left join fish_gps
                     on kr_data.country=fish_gps.COUNTRY
                     and kr_data.dhsyear=fish_gps.dhsyear
                     and kr_data.dhsclust=fish_gps.DHSCLUST")

rm(all_kr, all_kr_combined, kr_data, keep_kr, kr, kr_files, kr_vars_to_keep)

setDT(kr_fish_gps, key="caseid")



