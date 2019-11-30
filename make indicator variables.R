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

#############################
#Childhood





