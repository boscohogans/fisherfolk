
#Define custom functions
#To scale variables
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

#Unadjusted regression function
reg_unadj_fn <- function(x,y, df) {
  lapply(setNames(x, x), function(var) {
    formula <-
      as.formula(paste(var, "~", "fish_factor" , "+", y))
    mod <-
      glmer(formula,
            data = df
            ,nAGQ = 0
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
    odds <- sjstats::odds_to_rr(mod)
    p_value <- parameters::p_value(mod)
  cbind(odds, p_value) 
  })
}

#Adjusted Regression function
reg_adj_fn <- function(x,y, df) {
  lapply(setNames(x, x), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", y))
    mod <-
      glmer(formula,
            data = df
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
    odds <- sjstats::odds_to_rr(mod)
    p_value <- parameters::p_value(mod)
    cbind(odds, p_value) 
  })
}

#Regression function by country
reg_country_fn <- function(x,y, df) {
  lapply(setNames(x, x), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", y))
    hr_fit_country <- df %>%
      split(.$country) %>%
      map(~glmer(formula,
                 data = .
                 ,nAGQ = 0
                 ,family = 'binomial'
                 ,control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))) 
    odds <- map(hr_fit_country, sjstats::odds_to_rr)
    #p_value <- map(hr_fit_country, parameters::p_value)
    #cbind(odds, p_value) 
  })
}


# Household variables -----------------------------------------------------
all_hr_vars <- c("fish_factor", "num_hhmembers" ,"asset_index_nowashnomat")

#Household data assembly
hr_select <- hr_fish_gps_upd %>% 
  #Make clusters variable
  mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  rename(num_hhmembers=hv009) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  mutate(fishing_community_5 = ifelse(is.na(fishing_community_5), 0, fishing_community_5)) %>%
  mutate(fish_factor = as.factor(fishing_community_5)) %>%
  #Change variables to factors
  mutate_at(vars(hr_outcomes),factor) %>%
  select(clusters,fish_factor,country, num_hhmembers,asset_index_nowashnomat,hr_outcomes,
         water_piped, water_imp, san_imp, less_than_5, housing_imp) %>% 
  #Scale variables
  mutate_at(c("num_hhmembers","asset_index_nowashnomat"), scale2)



# Childhood data ----------------------------------------------------------
all_kr_vars <- c("fish_factor", "num_hhmembers", "asset_index_nowashnomat", "medu_yrs", "mat_age")

#Childhood data assembly
kr_select <- kr_fish_gps_upd %>% 
  #Make clusters variable
  mutate(clusters= paste(v000,v021,sep="_")) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  #Change fish community variable to factor
  mutate(fishing_community_5 = ifelse(is.na(fishing_community_5), 0, fishing_community_5)) %>%
  mutate(fish_factor = as.factor(fishing_community_5)) %>%
  rename(num_hhmembers=v009) %>% 
  rename(medu_yrs=v133) %>%
  filter(!is.na(medu_yrs)) %>%
  mutate(mat_age = as.numeric(v012)) %>%
  #Change variables to factors
  mutate_at(vars(kr_outcomes),factor) %>%
  #Make household variable
  mutate(hh_num_str = paste(clusters, v002,sep="_")) %>%
  select(clusters, hh_num_str,all_kr_vars,kr_outcomes, hr_outcomes, country) %>% 
  #Scale variables
  mutate_at(c("num_hhmembers", "asset_index_nowashnomat", "medu_yrs", "mat_age"), scale2)

kr_with_hr <- c(all_kr_vars, hr_outcomes)

#Define random intercepts
hr_intercept <- "(1|clusters)"
kr_intercept <- "(1|clusters) + (1|hh_num_str)"

# Run models --------------------------------------------------------------
#Unadjusted
hr_unadj_models <- reg_unadj_fn(hr_outcomes, hr_intercept, hr_select)
kr_unadj_models <- reg_unadj_fn(kr_outcomes, kr_intercept, kr_select)

#Adjusted
hr_adj_models <- reg_adj_fn(hr_outcomes, hr_intercept, hr_select)
kr_adj_models <- reg_adj_fn(kr_outcomes, kr_intercept, kr_select)

#By country
hr_country_models <- reg_country_fn(hr_outcomes, hr_intercept, hr_select)
kr_country_models <- reg_country_fn(kr_outcomes, kr_intercept, kr_select)



#kr_hr_models <- reg_adj_fn(kr_with_hr, kr_intercept, kr_select)

#Kr/hr models need to be run seperatly
kr_hr_models <- lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
  fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
  formula <-
    as.formula(paste(var, "~", fixed , "+", kr_intercept))
  kr_hr_fit <- glmer(formula,
               data = kr_select
               ,nAGQ = 0
               ,family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))
  odds <- map(kr_hr_fit, sjstats::odds_to_rr)
  p_value <- map(kr_hr_fit, parameters::p_value)
  cbind(odds, p_value) 
})


kr_hr_country_models <- lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
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
  odds <- map(kr_hr_fit_country, sjstats::odds_to_rr)
  p_value <- map(kr_hr_fit_country, parameters::p_value)
  cbind(odds, p_value) 
})

