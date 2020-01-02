##Household regressions

#All hr vars
all_hr_vars <- c("fish_factor", "num_hhmembers" ,"asset_index_nowashnomat")
hr_scale <- all_hr_vars[-1]


hr_select <- hr_fish_gps %>% 
  #Make clusters variable
  mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  rename(num_hhmembers=hv009) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community)) %>%
  mutate(fish_factor = as.factor(fishing_community)) %>%
  #Change variables to factors
  mutate_at(vars(hr_outcomes),factor) %>%
  select(clusters,fish_factor,country, num_hhmembers,asset_index_nowashnomat,hr_outcomes,
         water_piped, water_imp, san_imp, less_than_5, housing_imp)

#Scale variables
hr_select_scale <- hr_select
hr_select_scale[hr_scale] <- lapply(hr_select_scale[hr_scale],scale)

#Define random intercept
hr_intercept <- "(1|clusters)"

##Build full model with all variables
hr_models <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
     formula <-
       as.formula(paste(var, "~", fixed , "+", hr_intercept))
    print(formula)
    mod <-
      glmer(formula,
            data = hr_select_scale
            #,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
  })

names(hr_models) <- paste(hr_outcomes)

#Extract coefficients to table
for_exp <- c("estimate", "conf.low", "conf.high")
hr_models_tabl <- map(hr_models, tidy, conf.int=TRUE)
names(hr_models_tabl) <- paste(hr_outcomes)
#bind rows together
hr_results <- rbindlist(hr_models_tabl, idcol="id")
hr_results <- hr_results[, lapply(.SD, exp), by=c("id", "term", "p.value"), .SDcols = for_exp]


#Repeat for each country
hr_models_country <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", hr_intercept))
    hr_select_scale %>%
      split(.$country) %>%
      map(~glmer(formula,
                        data = .
                        ,nAGQ = 0
                        ,family = 'binomial'
                        ,control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5))))})

 
names(hr_models_country) <- paste(hr_outcomes)

all_hr_country <- modify_depth(hr_models_country,2,tidy, conf.int=TRUE)
water_imp_country <- rbindlist(all_hr_country$water_imp, idcol="id")
water_piped_country <- rbindlist(all_hr_country$water_piped, idcol="id")
hwashobs_country <- rbindlist(all_hr_country$hwashobs, idcol="id")
less5_country <- rbindlist(all_hr_country$less_than_5, idcol="id")
san_imp_country <- rbindlist(all_hr_country$san_imp, idcol="id")
housing_imp_country <- rbindlist(all_hr_country$housing_imp, idcol="id")

hr_list <- list(water_imp_country, water_piped_country, hwashobs_country, less5_country,
                san_imp_country, housing_imp_country)
names(hr_list) <- paste(hr_outcomes)
hr_country_data <- rbindlist(hr_list, idcol="outcome")

hr_country_data_results <-hr_country_data[complete.cases(hr_country_data[, for_exp, with=FALSE])]
hr_country_data_results <- hr_country_data[, lapply(.SD, exp), by=c("outcome", "id", "term", "p.value"), .SDcols = for_exp]

rm(all_hr_country, hr_list, water_imp_country, water_piped_country, hwashobs_country, less5_country,
   san_imp_country, housing_imp_country)

##################################################
##################################################
#Childhood data
#All kr vars
all_kr_vars <- c("fish_factor", "num_hhmembers", "asset_index_nowashnomat", "medu_yrs", "mat_age")
kr_scale <- all_kr_vars[-1]

kr_select <- kr_fish_gps %>% 
  #Make clusters variable
  mutate(clusters= paste(v000,v021,sep="_")) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  #Change fish community variable to factor
  mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community)) %>%
  mutate(fish_factor = as.factor(fishing_community)) %>%
  rename(num_hhmembers=v009) %>% 
  rename(medu_yrs=v133) %>%
  filter(!is.na(medu_yrs)) %>%
  mutate(mat_age = as.numeric(v012)) %>%
  #Change variables to factors
  mutate_at(vars(kr_outcomes),factor) %>%
  #Need to rescale variables
  #Make household variable
  mutate(hh_num_str = paste(clusters, v002,sep="_")) %>%
  select(clusters, hh_num_str,all_kr_vars,kr_outcomes, hr_outcomes, country)

#Scale variables
kr_select_scale <- kr_select
kr_select_scale[kr_scale] <- lapply(kr_select_scale[kr_scale],scale)

#Check missingness
#gg_miss_upset(kr_select_scale)

#Define random intercept
kr_intercept <- "(1|clusters) + (1|hh_num_str)"


kr_models <-
  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    print(formula)
    mod <-
      glmer(formula,
            data = kr_select_scale
            ,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
  })

names(kr_models) <- paste(kr_outcomes)

#Extract coefficients to table
kr_models_tabl <- map(kr_models, tidy, conf.int=TRUE)
names(kr_models_tabl) <- paste(kr_outcomes)
#bind rows together
kr_results <- rbindlist(kr_models_tabl, idcol="id")
kr_results <- kr_results[, lapply(.SD, exp), by=c("id", "term", "p.value"), .SDcols = for_exp]

rm(kr_models_tabl)

#Repeat for each country
kr_models_country <-
  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    kr_select_scale %>%
      split(.$country) %>%
      map(~glmer(formula,
                 data = .
                 ,nAGQ = 0
                 ,family = 'binomial'
                 ,control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5))))
  })

names(kr_models_country) <- paste(kr_outcomes)


all_kr_country <- modify_depth(kr_models_country,2,tidy, conf.int=TRUE)
diarrhea_country <- rbindlist(all_kr_country$diarrhea, idcol="id")
not_immun_country <- rbindlist(all_kr_country$not_immun, idcol="id")
ari_country <- rbindlist(all_kr_country$ari, idcol="id")
fever_country <- rbindlist(all_kr_country$fever , idcol="id")
#bfeeding_country <- rbindlist(all_kr_country$bfeeding, idcol="id")

kr_list <- list(diarrhea_country, not_immun_country, ari_country, fever_country)
names(kr_list) <- paste(kr_outcomes)
kr_country_data <- rbindlist(kr_list, idcol="outcome")

kr_country_data <-kr_country_data[complete.cases(kr_country_data[, for_exp, with=FALSE])]
kr_country_data_results <- kr_country_data[, lapply(.SD, exp), by=c("outcome", "id", "term", "p.value"), .SDcols = for_exp]

rm(all_kr_country, kr_list, diarrhea_country, not_immun_country, ari_country, fever_country, propensity_score, hr_scale, kr_scale, hr_intercept)

#We also want to look at how household indicators impact childhood illness
start_time <- Sys.time()
kr_with_hr <- c(all_kr_vars, hr_outcomes)

kr_with_hr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(kr_with_hr), collapse = " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_intercept))
  print(formula)
  mod <-
    glmer(
      formula,
      kr_select_scale,
      nAGQ = 0,
      family = 'binomial'
      ,control=glmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))
})
end_time <- Sys.time()

kr_with_hr_models_tabl <- map(kr_with_hr_models, tidy, conf.int=TRUE)
names(kr_with_hr_models_tabl) <- paste(kr_outcomes)
#bind rows together
kr_with_hr_results <- rbindlist(kr_with_hr_models_tabl, idcol="id")
kr_with_hr_results <- kr_with_hr_results[, lapply(.SD, exp), by=c("id", "term", "p.value"), .SDcols = for_exp]


#Extract coefficients to table
kr_hr_models_tabl <- map(kr_with_hr_models, tidy, conf.int=TRUE)
names(kr_hr_models_tabl) <- paste(kr_outcomes)
#bind rows together
kr_hr_results <- rbindlist(kr_hr_models_tabl, idcol="id")
kr_hr_results <- complete.cases(kr_hr_results[, lapply(.SD, exp), by=c("id", "term", "p.value"), .SDcols = for_exp])



#Repeat for each country
kr_hr_models_country <-
  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    kr_select_scale %>%
      split(.$country) %>%
      map(~glmer(formula,
                 data = .
                 ,nAGQ = 0
                 ,family = 'binomial'
                 ,control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5))))
  })

names(kr_hr_models_country) <- paste(kr_outcomes)


all_kr_hr_country <- modify_depth(kr_hr_models_country,2,tidy, conf.int=TRUE)
diarrhea_hr_country <- rbindlist(all_kr_hr_country$diarrhea, idcol="id")
not_immun_hr_country <- rbindlist(all_kr_hr_country$not_immun, idcol="id")
ari_hr_country <- rbindlist(all_kr_hr_country$ari, idcol="id")
fever_hr_country <- rbindlist(all_kr_hr_country$fever , idcol="id")
#bfeeding_country <- rbindlist(all_kr_country$bfeeding, idcol="id")

kr_hr_list <- list(diarrhea_hr_country, not_immun_hr_country, ari_hr_country, fever_hr_country)
names(kr_hr_list) <- paste(kr_outcomes)
kr_hr_country_data <- rbindlist(kr_hr_list, idcol="outcome")

kr_hr_country_data <-kr_hr_country_data[complete.cases(kr_hr_country_data[, for_exp, with=FALSE])]
kr_country_data_results <- kr_hr_country_data[, lapply(.SD, exp), by=c("outcome", "id", "term", "p.value"), .SDcols = for_exp]


