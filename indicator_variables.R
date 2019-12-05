##Household variables
###Make HR variables 
#Piped water
hr_fish_gps[, water_piped := ifelse(grepl("piped", hv201), 1,0)]

#Improved water source
hr_fish_gps$hv201 <- tolower(hr_fish_gps$hv201)

hr_fish_gps[, water_imp := ifelse(grepl("piped", hv201), 1,
                                  ifelse(grepl("\\bprotected\\b", hv201), 1,
                                         ifelse(
                                           grepl("\\brain\\b", hv201), 1,
                                           ifelse(
                                             grepl("\\bborehole\\b", hv201), 1,
                                           ifelse(
                                             grepl("\\bbottled\\b", hv201),
                                             1,
                                             ifelse(hv201 == "public tap/standpipe", 1, 0)
                                           )))))]

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
hr_fish_gps[, san_imp :=   ifelse(grepl("uncovered", hv205), 0,
                                  ifelse(grepl("somewhere", hv205), 0,
                                         ifelse(grepl("no slab", hv205), 0,
                                                ifelse(grepl("without slab", hv205), 0,
                                                       ifelse(grepl("hanging", hv205), 0,
                                         ifelse(grepl("know where", hv205), 0,san_imp))))))]

#Not improved if shared
hr_fish_gps[, san_imp := ifelse(hv225==1,0,san_imp)]

#Hygiene variables 
#https://dhsprogram.com/Data/Guide-to-DHS-Statistics/index.htm#t=Handwashing.htm%23Percentage_of_households3bc-1&rhtocid=_5_7_0
#Place for washing AND water is available
hr_fish_gps[, hwashobs := ifelse((grepl("^observed", hv230a)) & (grepl("water is available", hv230b)), 1,0)]

#Place for washing AND water AND soap/detergent present
hr_fish_gps[, hwashobs_soap := ifelse(hwashobs==1 & (grepl("yes", hv232b)), 1,0)]


#Improved housing
# https://www.nature.com/articles/s41586-019-1050-5.pdf
#Number of people per bedroom
#hr_fish_gps[, ppl_per_bedroom := round((hv009 / hv216), 0)]
hr_fish_gps[, ppl_per_bedroom := ifelse((!is.na(hv009) & !is.na(hv216)),(hv009/hv216),0)]
hr_fish_gps$ppl_per_bedroom <- ifelse(!is.finite(hr_fish_gps$ppl_per_bedroom),0,hr_fish_gps$ppl_per_bedroom)

hr_fish_gps[, three_per_bedroom := ifelse((ppl_per_bedroom > 3), 1, 0)]

finished_materials <- c("concrete", "cement", "asbestos", "tiles", "shingles", "iron", 
                        "ceramic","metal", "sheets", "bricks", "parquet", "vinyl", "zinc","stone blocks", "carpet", "polished wood", "\\bcovered adobe\\b", "3")
#Tanzania has numbers instead of labels for round 7. Finished materials start with "3"

fa <- function(x) {
  y <- tolower(x)
  ifelse(grepl(paste(finished_materials, collapse="|"),y),1,0)
}

hr_fish_gps <- hr_fish_gps %>%
  mutate(roof_finished=fa(hv215),
         wall_finished=fa(hv214),
         floor_finished=fa(hv213),
        housing_unfinished=ifelse((roof_finished + wall_finished + floor_finished) <2,1,0),
         housing_unimp = ifelse((water_imp!=1 | san_imp!=1 | three_per_bedroom==1 | housing_unfinished==1),1,0),
         housing_imp = 1-housing_unimp)


#Need to keep a selection of variables for merging into childhood data

hr_outcomes <-
  c("water_imp",
    "water_piped",
    "hwashobs",
    "less_than_5",
    "san_imp",
    "housing_imp")

household_indicators <- select(hr_fish_gps, hv000, hv001, hv002, hr_outcomes)

##Childhood variables
f1 <- function(x) {
  y<- tolower(x)
  ifelse(grepl("yes",y),1,
         ifelse(grepl("vacc",y),1,0))
}
#Change blanks to NA
kr_fish_gps[kr_fish_gps==""] <- NA

kr_fish_gps <- kr_fish_gps %>%
  mutate(diarrhea=f1(h11),
         #*https://dhsprogram.com/data/Guide-to-DHS-Statistics/index.htm#t=Vaccination.htm%23Percentage_of_children9bc-1&rhtocid=_13_1_0
         vaccination=f1(h10),
         #*generating binary dpt variable* // includes self reported vaccination by caregiver as suggested by dhs https://dhsprogram.com/pubs/pdf/FR308/FR308.pdf
         dpt=f1(h7),
         #*generating binary polio variable* // includes self reported vaccination by caregiver as suggested by dhs https://dhsprogram.com/pubs/pdf/FR308/FR308.pdf
         polio=f1(h6),
         #*generating binary measles variable* // includes self reported vaccination by caregiver as suggested by dhs https://dhsprogram.com/pubs/pdf/FR308/FR308.pdf
         meas=f1(h9),
         #*generating immunisation variable per description in https://www.sciencedirect.com/science/article/pii/S1473309915700423?via%3Dihub*
         not_immun=ifelse((dpt!=1 & polio!=1 & meas!=1),1,0),
         h31b=tolower(h31b),
         h31c=tolower(h31c),
         ari=ifelse(h31b=="yes" & h31c %in% c("chest only", "both"),1,0),
         ari = ifelse(is.na(ari), 0, ari),
         fever=f1(h22),
         m4=tolower(m4),
         bfeeding=ifelse(m4 %in% c("never breastfed", "99", "dk", "don't know", "inconsistent"),0,
                         ifelse(is.na(m4),0,1))) %>%
  left_join(household_indicators, by=c("v000"="hv000", "v001"="hv001", "v002"="hv002"))

kr_outcomes <- c("diarrhea",
                 "not_immun",
                 "ari",
                 "fever")

rm(household_indicators, finished_materials, f1, fa)
