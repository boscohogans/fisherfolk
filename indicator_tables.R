##Household
water_piped_table <- hr_fish_gps %>% tab(water_piped, hv201)
water_imp_table <- hr_fish_gps %>% tab(water_imp, hv201)
hwashbos_table <- hr_fish_gps %>% tab(hwashobs, hv230a, hv230b)
less5_table <- hr_fish_gps %>% tab(less_than_5,water_imp, hv204) 
san_imp_table <- hr_fish_gps %>% tab(san_imp, hv205, hv225)
roof_finished_table <- hr_fish_gps %>% tab(roof_finished, hv215)
wall_finished_table <- hr_fish_gps %>% tab(roof_finished, hv214)
floor_finished_table <- hr_fish_gps %>% tab(roof_finished, hv213)
house_unfinished_table <- hr_fish_gps %>% tab(housing_unfinished, roof_finished, wall_finished, floor_finished)
three_per_bedroom_table <- hr_fish_gps %>% tab(three_per_bedroom, ppl_per_bedroom)
housing_imp_table <- hr_fish_gps %>% tab(housing_imp,water_imp, san_imp, three_per_bedroom, housing_unfinished)

#Childhood
diarrhea_table <- kr_fish_gps_upd %>% tab(diarrhea, h11)
dpt_table <- kr_fish_gps_upd %>% tab(dpt, h7)
polio_table <- kr_fish_gps_upd %>% tab(polio, h6)
meas_table <- kr_fish_gps_upd %>% tab(meas, h9)
immun_table <- kr_fish_gps_upd %>% tab(immun, dpt, polio, meas)
not_immun_table <- kr_fish_gps_upd %>% tab(not_immun, dpt, polio, meas)
ari_table <- kr_fish_gps_upd %>% tab(ari, h31b, h31c)
fever_table <- kr_fish_gps_upd %>% tab(fever, h22)
bfeeding_table <- kr_fish_gps_upd %>% tab(bfeeding, m4)

# require(openxlsx)
# list_of_datasets <- list("water_piped" = water_piped_table, "water_imp" = water_imp_table,
#                          "hwashobs" = hwashbos_table, "less5" = less5_table, "san_imp"=san_imp_table,
#                          "roof_finished" = roof_finished_table, "wall_finished"=wall_finished_table,
#                          "floor_finished" = floor_finished_table, "house_unfinished" = house_unfinished_table,
#                          "three_per_bedroom"=three_per_bedroom_table, "housing_imp"=housing_imp_table,
#                          "diarrhea"=diarrhea_table, "dpt"=dpt_table, "polio"=polio_table,
#                          "meas"=meas_table, "immun"=immun_table, "not_immun"=immun_table,
#                          "ari"=ari_table, "fever"=fever_table, "bfeeding"=bfeeding_table)
# write.xlsx(list_of_datasets, file = "outputs//indicator_variables.xlsx")

rm(list=ls(pattern="table"))
rm(list_of_datasets)
