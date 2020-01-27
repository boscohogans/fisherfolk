# Household variables -----------------------------------------------------

#All hr vars
all_hr_vars <- c("fish_factor", "num_hhmembers" ,"asset_index_nowashnomat")

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

hr_select <- hr_fish_gps_upd %>% 
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
         water_piped, water_imp, san_imp, less_than_5, housing_imp) %>% 
  mutate_at(c("num_hhmembers","asset_index_nowashnomat"), scale2)

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
            data = hr_select
            #,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
    sjstats::odds_to_rr(mod)
  })

#Repeat for each country
hr_models_country <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", hr_intercept))
    hr_fit_country <- hr_select %>%
      split(.$country) %>%
      map(~glmer(formula,
                        data = .
                        ,nAGQ = 0
                        ,family = 'binomial'
                        ,control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)))) 
    summary <- map(hr_fit_country, sjstats::odds_to_rr)
    })


# Childhood variables -----------------------------------------------------
all_kr_vars <- c("fish_factor", "num_hhmembers", "asset_index_nowashnomat", "medu_yrs", "mat_age")

kr_select <- kr_fish_gps_upd %>% 
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
  select(clusters, hh_num_str,all_kr_vars,kr_outcomes, hr_outcomes, country) %>% 
  mutate_at(c("num_hhmembers", "asset_index_nowashnomat", "medu_yrs", "mat_age"), scale2)

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
            data = kr_select
            ,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
    sjstats::odds_to_rr(mod)
  })

#https://strengejacke.github.io/sjstats/reference/odds_to_rr.html

#Repeat for each country
kr_models_country <-
  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    kr_fit_country <- kr_select %>%
      split(.$country) %>%
      map(~glmer(formula,
                 data = .
                 ,nAGQ = 0
                 ,family = 'binomial'
                 ,control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))) 
    map(kr_fit_country, sjstats::odds_to_rr)
  })


# Childhood and household -------------------------------------------------


#We also want to look at how household indicators impact childhood illness
kr_with_hr <- c(all_kr_vars, hr_outcomes)


kr_with_hr_models <-
  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    print(formula)
    mod <-
      glmer(formula,
            data = kr_select
            ,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
    sjstats::odds_to_rr(mod)
  })


#Repeat for each country
kr_with_hr_country <-  lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
    fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", kr_intercept))
    kr_hr_fit_country <- kr_select %>%
      split(.$country) %>%
      map(~glmer(formula,
                 data = .
                 ,nAGQ = 0
                 ,family = 'binomial'
                 ,control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5))))
    map(kr_hr_fit_country, sjstats::odds_to_rr)
    #map(kr_hr_fit_country, summary)
    #coefficients <- map(kr_hr_fit_country,tidy, conf.int=TRUE)
  })
  
names(kr_hr_models_country) <- paste(kr_outcomes)

