#setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

#library(readstata13)
#HOUSEHOLD
#hr <- read.dta13("gps_hr_20190708_dk.dta")

#hr <- read.csv("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut/analysis_dataset_20190726.csv")

#hr <- subset(hr, subset = hv000 %in% c("KE6", "MW7", "TZ7", "UG7", "ZM6","KE5", "MW5", "TZ5", "UG6", "ZM5"))



kr1 <- as.data.frame(kr_fish_gps_upd)

kr2 <- kr1 %>% 
  mutate(unique_id=paste(v000,v001,v002),
         number=1)  %>% 
  group_by(unique_id) %>% 
  rename(mat_age=v012,
         mat_edu=v133) %>% 
  mutate(ticket=cumsum(number))

kr3 <- subset(kr2, ticket==1) %>% 
  select(unique_id, caseid, v000, v001, v002, mat_age, mat_edu) 


#Proportion of clusters 
data_table1 <- hr_fish_gps_upd %>% 
  mutate(fish_factor=factor(fishing_community_5,
                            levels=c(0,1),
                            labels=c("Non-fishing community", "Fishing community")),
         country_factor=factor(country,
                               levels = c("KE", "MW", "TZ", "UG", "ZM"),
                               labels = c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")),
         quintile_factor=factor(quintile_nowashnomat,
         levels = c("1", "2", "3", "4", "5"),
         labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))) %>% 
      mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  mutate(unique_id=paste(hv000,hv001,hv002)) %>% 
    rename(num_hhmembers=hv009) %>% 
  left_join(kr3, by="unique_id") %>% 
  #Join propensity score
    left_join(select(propensity_score,clusters,weights), by="clusters") 

    #left_join(kr_reduced, by=c("hv000"="v000", "hv001"="v001", "hv002"="v002")) 

data_table1 %>%  tab(weights, fish_factor)

# hr$housing_imp <- 1-(hr$housing_unimp)
# 
# variables <- c("water_piped", "water_imp", "san_imp", "less_than_5", "housing_imp")
# #hr_test <- hr
# 
# #hr_test[variables] <- lapply(hr_test[variables], factor)
# hr[variables] <- lapply(hr[variables], as.logical)

library(tableone)

#Propensity score matchin
#propensity_score <- read.csv("propensity_score_matching_20190723.csv")

#hr$clusters <- paste(hr$hv000, hr$hv021, sep="_")

#hr_propensity <- merge(hr,propensity_score, by="clusters")

#analysis_dataset <- subset(hr_propensity, weights==1)

# label(data_table1$country_fac) <- "Country"
# label(data_table1$quintile_nowash_fac) <- "Deprivation Quintile"
# label(data_table1$water_imp) <- "Improved source of drinking water"
# label(data_table1$less_than_5) <- "Less than 5 minutes to improved water source"
# label(data_table1$san_imp) <- "Improved sanitation"
# label(data_table1$housing_imp) <- "Improved housing"
# label(data_table1$num_hh_members) <- "Number of household members"
# label(data_table1$num_childrenunder5) <- "Number of children under 5"
# 
 # vars <- c("num_hhmembers",  "asset_index_nowashnomat", 
 #           "country_factor", "water_imp","less_than_5", "san_imp", "housing_imp")
 # 
 listVars <- c("num_hhmembers", "asset_index_nowashnomat", "mat_age", "mat_edu")
 catVars <- c("country_factor","water_imp","less_than_5", "san_imp", "housing_imp")
 

vars <- c("num_hhmembers", "asset_index_nowashnomat", "mat_age", "mat_edu", "country_factor")

tab1 <- CreateTableOne(vars = vars, strata = "fish_factor", 
                       data = subset(data_table1,weights==1))


#Need to get sum of childhod variables
table1 <- print(tab1)

# Load the packages
library(officer)
library(magrittr)

# The script
docx( ) %>% 
  addFlexTable(table1 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold( color = "white" ),
                           add.rownames = TRUE ) %>%
                 setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = paste("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Outputs/Graphics","table1.docx"))



write.csv(tab1, paste("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Outputs/Graphics","table1.csv"))

#setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Outputs/Graphics")
# write.csv(x, "table1.csv")
# levels(final_data_wide$variable) <- c("Access to a piped water supply", "Less than 5 minutes to obtain drinking water",
#                                       "Access to improved sanitation", "Improved housing")

# library(officer)
# library(magrittr)
# library(flextable)
# my_doc <- read_docx() 
# styles_info(my_doc)
# 
# read_docx( ) %>% 
#   body_add_flextable(tableOne %>%
#                  flextable(header.cell.props = cellProperties( background.color = "#003366"),
#                            header.text.props = textBold( color = "white" ),
#                            add.rownames = TRUE ) %>%
#                  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
#   writeDoc(file = "table1.docx")
# library(sjPlot)
# x <- as.data.frame(tableOne)
# tab_df(tableOne)
# tableOne
