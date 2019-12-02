##setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")


#HOUSEHOLD
#hr <- read.dta13("data//gps_hr_20191120_dk.dta")

##Need to orignial DHS asset index with generated asset index
##http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
library("PerformanceAnalytics")
my_data <- hr_fish_gps[, c("hv271","asset_index_nowashnomat")]
corr <- chart.Correlation(my_data, histogram=TRUE, pch=19)
#.86 and highly significant indicates strong correlation

#Import propensity score
# propensity_score <-
#   fread("data//propensity_score_matching.csv")

# hr_outcomes <-
#   c("water_imp",
#     "water_piped",
#     "less_than_5",
#     "san_imp",
#     "housing_imp")

#All hr vars
all_hr_vars <- c("fish_factor", "num_hh_censor" ,"asset_index_nowashnomat")

hr_select <- hr_fish_gps %>% 
  #Make clusters variable
  #mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  #mutate(fishing_community = ifelse(is.na(fishing_community), 0, fishing_community)) %>%
  #mutate(fish_factor = as.factor(fishing_community)) %>%
  #Censor large households by assigning them a max value
  mutate(num_hh_censor = ifelse(num_hh_members<=11,num_hh_members, 12)) %>%
  #Rescale SES
  #mutate_at(vars(num_hh_censor,asset_index_nowashnomat), funs(scale)) %>%
  #Change variables to factors
  mutate_at(vars(hr_outcomes),factor) %>%
  select(clusters,fish_factor,country, num_hh_censor,asset_index_nowashnomat,hr_outcomes,
         water_piped, water_imp, san_imp, less_than_5, housing_imp)

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
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
  })

names(hr_models) <- paste(hr_outcomes, 1:5)


#Repeat for each country

hr_models_country <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", hr_intercept))
    print(formula)
    lapply(setNames(countries, countries), function(k) {
      y <- subset(hr_select, country == k)
    mod <-
      glmer(formula,
            data = y
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
  })
  })

names(hr_models_country) <- paste(hr_outcomes, 1:5)


##################################################
##################################################
#Childhood data
#kr <- read.dta13("data//gps_kr_20191120_dk.dta")

kr_outcomes <-
  c("not_immun", "fever", "diarrhea",  "ari")

#All kr vars
all_kr_vars <- c("fish_factor", "num_hh_censor", "asset_index_nowashnomat", "medu_yrs_censor", "mat_age")

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
  #Censor large households by assigning them a max value
  mutate(num_hh_censor = ifelse(v009<=11,v009, 12)) %>%
  #Censor years of education to deal with outliers
  #Need to check what happens to the one missing value
  mutate(medu_yrs_censor =ifelse(v133<=12,v133, 13)) %>%
  mutate(mat_age = as.numeric(v012)) %>%
  #Change variables to factors
  mutate_at(vars(kr_outcomes),funs(factor)) %>%
  #Need to rescale variables
  mutate_at(vars(num_hh_censor,asset_index_nowashnomat, medu_yrs_censor, mat_age), funs(scale, center=TRUE, scale=TRUE)) %>%
  #Rename maternal age variable
  #mutate_at(vars(num_hh_censor, asset_index_nowashnomat, medu_yrs_censor, mat_age), scale(all_kr_vars)) %>%
  #Make household variable
  mutate(hh_num_str = paste(clusters, v002,sep="_")) %>%
  select(clusters, hh_num_str,all_kr_vars,kr_outcomes, hr_outcomes)


#Check missingness
gg_miss_upset(kr_select)

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
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
  })

names(kr_models) <- paste(kr_outcomes, 1:4)

# options(na.action = na.fail)
kr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_intercept))
  print(formula)
  mod <- glmer(formula, kr_select, family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa"))
})


names(kr_final_models) <- paste(kr_outcomes, 1:4)

#We also want to look at how household indicators impact childhood illness
kr_with_hr <- c("fish_factor", "num_hh_censor", "asset_index_nowashnomat", "medu_yrs_censor", "mat_age", 
                 "water_piped", "water_imp", "san_imp", "less_than_5", "housing_imp")


kr_with_hr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_intercept))
  print(formula)
  mod <- glmer(formula, kr_select, family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa"))
})



# #Labels for tables
# variable_labels <- c(
#   "fish_factorFishing_community" = "Fishing community",
#   "num_hh_censor" = "Number of household members" ,
#   "country_fac" = "Country",
#   "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest",
#   "medu_facPrimary" = "Maternal education - Primary",
#   "medu_facSecondary" = "Maternal education - Secondary",
#   "medu_facHigher" = "Maternal education - Higher"
# )
# 
# 
# hr_var_labels <- c(
#   "Improved water source",
#   "Piped water",
#   "<5 minutes improved water ",
#   "Improved sanitation",
#   "Improved housing"
# )
# 
# final_table <- tab_model(
#   hr_models,
#   pred.labels = hr_labels,
#   collapse.ci = TRUE,
#   dv.labels = hr_var_labels,
#   show.icc = FALSE,
#   show.re.var = FALSE,
#   show.r2 = FALSE,
#   prefix.labels = "varname",
#   p.style = "a",
#   order.terms=c(2,3,4,5,6,7,1),
#   #order.terms = c(1,2,8,3,4,5,6,7),
#   title='Household indicators'
#   #,  file= paste0(outputs,"hr_full_model.html", sep="")
# )
# 
# hr_country_table <- lapply(1:5, function(i) {
#   tab_model(
#     hr_models_country[[i]],
#     pred.labels = hr_labels,
#     collapse.ci = TRUE,
#     dv.labels = countries,
#     show.icc = FALSE,
#     show.re.var = FALSE,
#     show.r2 = FALSE,
#     terms = ("fish_factor1"),
#     p.style = "a",
#     title = hr_var_labels[i]
#   )
# })
# 
# 
# 
# 
# 
# 
# kr_var_labels <- c("Completed infant vaccination schedule",
#                    "Fever in last 2 weeks",
#                    "Diarrhea in last 2 weeks",
#                    "Acute Respiratory Infection")
# 
# kr_tables <- tab_model(
#   kr_final_models,
#   pred.labels = kr_labels,
#   collapse.ci = TRUE,
#   dv.labels = kr_var_labels,
#   show.reflvl = TRUE,
#   prefix.labels = "varname",
#   show.icc = FALSE,
#   show.re.var = FALSE,
#   show.r2 = FALSE,
#   p.style = "a",
#   #show.reflvl = TRUE, 
#   order.terms = c(2,12,13,14,7,8,3,4,5,6,9,10,11,1),
#   title='Childhood indicators'
# )
# 
# kr_tables_country <- lapply(1:4, function(i) {
#   tab_model(
#     kr_final_models_country[[i]],
#     pred.labels = hr_labels,
#     collapse.ci = TRUE,
#     dv.labels = countries,
#     show.icc = FALSE,
#     show.re.var = FALSE,
#     show.r2 = FALSE,
#     terms = ("fish_factorFishing_community"),
#     p.style = "a",
#     title = kr_var_labels[i]
#   )
# })
