##setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

library(readstata13)
library(dplyr)
library(sqldf)
library(ggplot2)
library(performance)
library(lme4)
library(MuMIn)
library(purrr)
library(sjPlot)
library(htmltools)
library(tableHTML)
library(naniar)
library(data.table)
library(scales)

#HOUSEHOLD
hr <- read.dta13("data//gps_hr_20191120_dk.dta")

##Need to orignial DHS asset index with generated asset index
##http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#library("PerformanceAnalytics")
#my_data <- hr[, c("hv271","asset_index_nowashnomat")]
#chart.Correlation(my_data, histogram=TRUE, pch=19)
#.77 and highly significant indicates strong correlation


#Import propensity score
propensity_score <-
  fread("data//propensity_score_matching.csv")

hr_outcomes <-
  c("water_imp",
    "water_piped",
    "less_than_5",
    "san_imp",
    "housing_imp")

hr_select <- hr %>% 
  #Make clusters variable
  mutate(clusters= paste(hv000,hv021,sep="_")) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  mutate(fish_factor = as.factor(fishing_community)) %>%
  #Censor large households by assigning them a max value
  mutate(num_hh_censor = ifelse(hv009<=11,hv009, 12)) %>%
  #Rescale SES
  mutate(ses_rescaled = rescale(asset_index_nowashnomat)) %>%
  #Change variables to factors
  mutate_at(vars(hr_outcomes),funs(factor)) %>%
  select(clusters,fish_factor,country_, num_hh_censor,ses_rescaled,hr_outcomes,
         water_piped, water_imp, san_imp, less_than_5, housing_imp)


#Define random intercept
hr_intercept <- "(1|clusters)"

#All hr vars
all_hr_vars <- c("fish_factor", "num_hh_censor" ,"ses_rescaled")

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
    #data.frame(coef(summary(mod)))
  })

names(hr_models) <- paste(hr_outcomes, 1:4)

#Make a list of all countries
countries <- c("KE", "MW", "TZ", "UG", "ZM")

hr_models_country <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", hr_intercept))
    print(formula)
    lapply(setNames(countries, countries), function(k) {
      y <- subset(hr_select, country_ == k)
    mod <-
      glmer(formula,
            data = y
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
    #data.frame(coef(summary(mod)))
  })
  })

names(hr_models_country) <- paste(hr_outcomes, 1:4)

#Labels for tables
hr_labels <- c(
  "fish_factorFishing_community" = "Fishing community",
  "num_hh_censor" = "Number of household members" ,
  "quintile_nowashnomat_fac2" = "SES - Poorer" ,
  "quintile_nowashnomat_fac3" = "SES - Middle" ,
  "quintile_nowashnomat_fac4" = "SES - Richer" ,
  "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest"
)

hr_var_labels <- c(
  "Piped water",
  "<5 minutes improved water ",
  "Improved sanitation",
  "Improved housing"
)

final_table <- tab_model(
  hr_models,
  pred.labels = hr_labels,
  collapse.ci = TRUE,
  dv.labels = hr_var_labels,
  show.icc = FALSE,
  show.re.var = FALSE,
  show.r2 = FALSE,
  prefix.labels = "varname",
  p.style = "a",
  order.terms=c(2,3,4,5,6,7,1),
  #order.terms = c(1,2,8,3,4,5,6,7),
  title='Household indicators'
  #,  file= paste0(outputs,"hr_full_model.html", sep="")
)

hr_country_table <- lapply(1:4, function(i) {
  tab_model(
    hr_models_country[[i]],
    pred.labels = hr_labels,
    collapse.ci = TRUE,
    dv.labels = countries,
    show.icc = FALSE,
    show.re.var = FALSE,
    show.r2 = FALSE,
    terms = ("fish_factorFishing_community"),
    p.style = "a",
    title = hr_var_labels[i]
    #,file= paste0(outputs,i,"hr_full_model.html", sep="")
  )
})


##################################################
##################################################
#Childhood data
kr <- read.dta13("data//gps_kr_20191120_dk.dta")

kr_outcomes <-
  c("not_immun", "fever_2weeks", "diarrhea",  "ari")

kr_select <- kr %>% 
  #Make clusters variable
  mutate(clusters= paste(v000,v021,sep="_")) %>%
  #Join propensity score
  left_join(select(propensity_score,clusters,weights), by="clusters") %>%
  #Keep if included in propensity score matching 
  filter(weights==1) %>%
  #Change fish community variable to factor
  mutate(fish_factor = as.factor(fishing_community)) %>%
  #Censor large households by assigning them a max value
  mutate(num_hh_censor = is.na(v009), NA, ifelse(v009<=11,v009, 12)) %>%
  #Censor years of education to deal with outliers
  #Need to check what happens to the one missing value
  mutate(medu_yrs_censor = is.na(v009), NA, ifelse(v012<=12,v012, 13)) %>%
  #Rescale SES
  mutate(ses_rescaled = rescale(asset_index_nowashnomat)) %>%
  #Change variables to factors
  mutate_at(vars(kr_outcomes),funs(factor)) %>%
  #Rename maternal age variable
  rename(mat_age=v012) %>%
  #Make household variable
  mutate(hh_num_str = paste(clusters, v002,sep="_")) %>%
  select(clusters,hh_num_str,fish_factor,num_hh_censor,medu_yrs_censor,ses_rescaled,kr_outcomes,mat_age,
         water_piped, water_imp, san_imp, less_than_5, housing_imp)

#Check missingness
gg_miss_upset(kr)

#Define random intercept
kr_levels <- "(1|clusters) + (1|hh_num_str)"

#All kr vars
all_kr_vars <- c("fish_factor", "num_hh_censor", "asset_index_nowashnomat", "medu_yrs_censor", "mat_age")

#Aggregate to household level 

# options(na.action = na.fail)
kr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_levels))
  print(formula)
  mod <- glmer(formula, kr_analysis_dataset, family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa"))
})


names(kr_final_models) <- paste(kr_outcomes, 1:4)

kr_final_models_country <- lapply(1:4, function(i) {
  fixed <- paste0(kr_response[i], collapse = "+")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", fixed, "+", kr_levels))
  print(formula)
  lapply(setNames(countries, countries), function(k) {
    mod <- glmer(formula, kr_analysis_dataset,control=glmerControl(optimizer="bobyqa"))
  })
})

names(kr_final_models_country) <- paste(kr_outcomes, 1:4)

#We also want to look at how household indicators impact childhood illness
kr_with_hr <- c("fish_factor", "num_hh_censor", "asset_index_nowashnomat", "medu_yrs_censor", "mat_age", 
                 "water_piped", "water_imp", "san_imp", "less_than_5", "housing_imp")


kr_with_hr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(kr_with_hr), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_levels))
  print(formula)
  mod <- glmer(formula, kr_analysis_dataset, family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa"))
})




#Labels for tables
kr_labels <- c(
  "fish_factorFishing_community" = "Fishing community",
  "country_fac" = "Country",
  #"num_childrenunder5" = "Number of children <5 yoa" ,
  "num_child_cat1" = "Children 5 and under in household (1)",
  "num_child_cat2+" = "Children 5 and under in household (>=2)",
  #"num_hh_members" = "Number of household members" ,
  "num_hh_cat4-5" = "Number of household members (4-5)",
  "num_hh_cat6-7" = "Number of household members (6-7)",
  "num_hh_cat7+" = "Number of household members (7+)",
  "quintile_nowashnomat_fac2" = "SES - Poorer" ,
  "quintile_nowashnomat_fac3" = "SES - Middle" ,
  "quintile_nowashnomat_fac4" = "SES - Richer" ,
  "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest",
  "medu_facPrimary" = "Maternal education - Primary",
  "medu_facSecondary" = "Maternal education - Secondary",
  "medu_facHigher" = "Maternal education - Higher"
)

kr_var_labels <- c("Completed infant vaccination schedule",
                   "Fever in last 2 weeks",
                   "Diarrhea in last 2 weeks",
                   "Acute Respiratory Infection")

kr_tables <- tab_model(
  kr_final_models,
  pred.labels = kr_labels,
  collapse.ci = TRUE,
  dv.labels = kr_var_labels,
  show.reflvl = TRUE,
  prefix.labels = "varname",
  show.icc = FALSE,
  show.re.var = FALSE,
  show.r2 = FALSE,
  p.style = "a",
  #show.reflvl = TRUE, 
  order.terms = c(2,12,13,14,7,8,3,4,5,6,9,10,11,1),
  #order.terms = c(1,2,7,8,3,4,5,6,9,10,11),
  title='Childhood indicators'
  #, file='C://Users//idcvdken//Dropbox (LSoHaTM)//DK//Fisherpeople//Outputs//Results//Models//20191025//kr_full_model.html'
)

kr_tables_country <- lapply(1:4, function(i) {
  tab_model(
    kr_final_models_country[[i]],
    pred.labels = hr_labels,
    collapse.ci = TRUE,
    dv.labels = countries,
    show.icc = FALSE,
    show.re.var = FALSE,
    show.r2 = FALSE,
    terms = ("fish_factorFishing_community"),
    p.style = "a",
    title = kr_var_labels[i]
  )
})
