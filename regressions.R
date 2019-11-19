##setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

library(readstata13)
library(dplyr)
library(sqldf)
library(ggplot2)
library(performance)
library(lme4)
library(MuMIn)
library(purr)
library(sjPlot)
library(htmltools)
library(tableHTML)
library(naniar)

#Propensity score matching
propensity_score <-
  read.csv("data//propensity_score_matching_20191119.csv")

#HOUSEHOLD
hr <- read.dta13("data//gps_hr_20191119_dk.dta")

hr_select <- hr

hr_select$fish_factor <- factor(
  hr_select$fishing_community,
  levels = c(0, 1),
  labels = c("Non-Fishing_community", "Fishing_community")
)
hr_select$country_fac <-
  factor(
    hr_select$country_,
    levels = c("KE", "MW", "TZ", "UG", "ZM"),
    labels = c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")
  )

hr_select$quintile_nowashnomat_fac <-
  factor(
    hr_select$quintile_nowashnomat,
    levels = c("1", "2", "3", "4", "5"),
    labels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")
  )

#Censor large households by assigning them a max value
hist(hr_select$num_hh_members)

hr_select <- hr_select %>% 
  mutate(num_hh_censor = ifelse(num_hh_members<=10,num_hh_members, 11))


hr_outcomes <-
  c("water_imp",
    "less_than_5",
    "san_imp",
    "housing_imp")

hr_select[hr_outcomes] <- lapply(hr_select[hr_outcomes], as.factor)

#Make clusters variable
hr_select$clusters <-
  paste(hr_select$hv000, hr_select$hv021, sep = "_")

hr_analysis_dataset <-
  sqldf(
    "Select hr_select.*, propensity_score.weights
    from hr_select
    left join propensity_score
    on hr_select.clusters=propensity_score.clusters
    where propensity_score.weights=1"
  )

#write.csv(hr_analysis_dataset,"hr_data_20191027.csv")

#Define random intercept
hr_intercept <- "(1|clusters)"

#All hr vars
all_hr_vars <- c("fish_factor", "num_hh_censor" ,"quintile_nowashnomat_fac")

##Build full model with all variables

hr_models <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
     formula <-
       as.formula(paste(var, "~", fixed , "+", hr_intercept))
    print(formula)
    mod <-
      glmer(formula,
            data = hr_analysis_dataset
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
    #dredge(mod, fixed = "fish_factor")
    #data.frame(coef(summary(mod)))
  })

names(hr_models) <- paste(hr_outcomes, 1:4)

#Make a list of all countries
countries <- c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")

hr_models_country <-
  lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
    fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
    formula <-
      as.formula(paste(var, "~", fixed , "+", hr_intercept))
    print(formula)
    lapply(setNames(countries, countries), function(k) {
      y <- subset(hr_analysis_dataset, country_fac == k)
    mod <-
      glmer(formula,
            data = y
            ,family = 'binomial'
            ,control=glmerControl(optimizer="bobyqa"))
    #dredge(mod, fixed = "fish_factor")
    #data.frame(coef(summary(mod)))
  })
  })



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
  "Improved water source",
  "<5 minutes improved water ",
  "Improved sanitation",
  "Improved housing"
)

tab_model(
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
kr <- read.dta13("gps_kr_20190923_dk.dta")

kr_select <- kr %>%
  dplyr::select(v000,
          fishing_community,
          country_,
          quintile_nowashnomat,
          v000,
          v002,
          v012,
          v021,
          v106,
          v135,
          v136,
          v137,
          diarrhea,
          immun,
          ari,
          fever_2weeks) %>%
  dplyr::rename(mat_age=v012,
         medu=v106,
         num_hh_members=v136,
         num_childrenunder5=v137
         )

write.csv(kr_select, "kr_select.csv")

#############################################################
#Children under 5
kr_select <- read.csv("kr_select.csv")

kr_select$fish_factor <- factor(
  kr_select$fishing_community,
  levels = c(0, 1),
  labels = c("Non-Fishing community", "Fishing_community")
)

kr_select$country_fac <-
  factor(
    kr_select$country_,
    levels = c("KE", "MW", "TZ", "UG", "ZM"),
    labels = c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")
  )

kr_select$quintile_nowashnomat_fac <-
  factor(
    kr_select$quintile_nowashnomat,
    levels = c("1", "2", "3", "4", "5"),
    labels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")
  )

kr_select$medu_fac <-
  factor(
    kr_select$medu,
    levels = c("no education", "primary", "secondary", "higher", ""),
    labels = c("No education", "Primary", "Secondary", "Higher", "Missing")
  )

kr_select$num_hh_cat <- cut(kr_select$num_hh_members , 
                            breaks = c(-Inf,4,6,8,Inf) ,
                            labels = c("1-3","4-5","6-7","7+"),
                            right = FALSE)

kr_select$num_child_cat <- cut(kr_select$num_childrenunder5,
                               breaks = c(0,1,2,Inf),
                               labels = c("0", "1", "2+"),
                               right = FALSE)

kr_outcomes <-
  c("immun", "fever_2weeks", "diarrhea",  "ari")

#Propensity score matching
kr_select$clusters <-
  paste(kr_select$v000, kr_select$v021, sep = "_")

kr_select$hh_num_str <-
  paste(kr_select$clusters, kr_select$v002, sep = "_")

kr_propensity <-
  merge(kr_select, propensity_score, by = "clusters")

#Check missingness
gg_miss_upset(kr_propensity)

#Drop 2 records with missing education status of mother
kr_analysis_dataset <- subset(kr_propensity,!(is.na(medu_fac)))

#Drop if visitor to household
#kr_analysis_dataset <- subset(kr_analysis_dataset,v135==1)

#Change to factors 
kr_select[kr_outcomes] <-
  lapply(kr_select[kr_outcomes], as.factor)

#Define random intercept
kr_levels <- "(1|clusters) + (1|hh_num_str)"
#kr_levels <- "(1|clusters)"

#kr_outcomes <- c("immun", "diarrhea", "fever_2weeks", "ari")
#All kr vars
all_kr_vars <- c("fish_factor", "num_hh_cat", "num_child_cat", "quintile_nowashnomat_fac", "medu_fac")

#Aggregate to household level 

 options(na.action = na.fail)
kr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_levels))
  print(formula)
  #cc <- kr_analysis_dataset %>% tidyr::drop_na(kr_outcomes[i])
  cc <- kr_analysis_dataset[!rowSums(is.na(kr_analysis_dataset[cols])), ]
  mod <- glmer(formula, cc, family = 'binomial'
               ,control=glmerControl(optimizer="bobyqa"))
  #dredge(mod, fixed = "fish_factor")
})
options(na.action = na.omit)
# 
# names(kr_models) <- paste(kr_outcomes, 1:4)
# 
# kr_dredge_table <- dplyr::bind_rows(kr_models, .id="colum_label")

#Model with lowest AIC
diarrhea_vars <-
  ("fish_factor + quintile_nowashnomat_fac + num_hh_cat + medu_fac")
immun_vars <-
  ("fish_factor + quintile_nowashnomat_fac + num_hh_cat + num_child_cat + medu_fac ")
fever_vars <-
  ("fish_factor + quintile_nowashnomat_fac   +  num_child_cat + medu_fac")
ari_vars <- 
  ("fish_factor + medu_fac")

kr_response <- list(fever_vars, diarrhea_vars, immun_vars, ari_vars)

kr_final_models <- lapply(1:4, function(i) {
  fixed <- paste0(kr_response[i], collapse = "+")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", fixed, "+", kr_levels))
  print(formula)
  cc <- kr_analysis_dataset %>% tidyr::drop_na(kr_outcomes[i])
   mod <-
     glmer(formula,
           cc,
           family = 'binomial',control=glmerControl(optimizer="bobyqa"))
  #data.frame(coef(summary(mod)))
})

names(kr_final_models) <- paste(kr_outcomes, 1:4)

ptm <- proc.time()
conf <- confint.merMod(kr_final_models$`immun 1`, devtol=1e-7)
proc.time() - ptm


kr_final_models_country <- lapply(1:4, function(i) {
  fixed <- paste0(kr_response[i], collapse = "+")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", fixed, "+", kr_levels))
  print(formula)
  lapply(setNames(countries, countries), function(k) {
    cc <- kr_analysis_dataset %>% tidyr::drop_na(kr_outcomes[i])
    y <- subset(cc, country_fac == k)
    mod <- glmer(formula, y,control=glmerControl(optimizer="bobyqa"))
    #data.frame(coef(summary(mod)))
  })
})

names(kr_final_models_country) <- paste(kr_outcomes, 1:4)

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
