setwd("C:/Users/idcvdken/Dropbox (LSoHaTM)/DK/Fisherpeople/Data/DHS/FileOut")

library(readstata13)
#HOUSEHOLD
# hr <- read.dta13("gps_hr_20190923_dk.dta")
#
# hr_select <-
#   dplyr::select(
#     hr,
#     hv000,
#     fishing_community,
#     country_,
#     quintile_nowashnomat,
#     num_hh_members,
#     num_childrenunder5,
#     hv000,
#     hv021,
#     water_piped,
#     water_imp,
#     san_imp,
#     less_than_5,
#     water_on_premises,
#     housing_unimp
#   )
#
# write.csv(hr_select, "hr_select.csv")
#hr_select <- read.csv("hr_select.csv")

#hr_select$housing_imp <- 1 - (hr$housing_unimp)
#write.csv(hr_select, "analysis_dataset_20190923.csv")

hr_select <- read.csv("analysis_dataset_20190923.csv")

library(dplyr)

hr_select <-
  subset(
    hr_select,
    subset = hv000 %in% c(
      "KE6",  "MW7",  "TZ7",  "UG7",  "ZM6",  "KE5",  "MW5",    "TZ5",    "UG6",      "ZM5"    )  )

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

hr_select$housing_imp <- 1 - (hr_select$housing_unimp)

hr_outcomes <-
  c(
    "water_imp",
    "san_imp",
    "less_than_5",
    "housing_imp"
  )

hr_select[hr_outcomes] <- lapply(hr_select[hr_outcomes], as.factor)

#Propensity score matching
propensity_score <-
  read.csv("propensity_score_matching_20190923.csv")

#Make clusters variable
hr_select$clusters <-
  paste(hr_select$hv000, hr_select$hv021, sep = "_")

library(sqldf)
hr_analysis_dataset <-
  sqldf(
    "Select hr_select.*, propensity_score.weights
    from hr_select
    left join propensity_score
    on hr_select.clusters=propensity_score.clusters
    where propensity_score.weights=1"
  )

##Build full model with all variables 
library(lme4)
hr_models <- lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
  form = paste( var, "~ fish_factor  + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
                (1 | clusters)")
  mod <- glmer(form, data=hr_analysis_dataset,nAGQ = 0,family = binomial, na.action = na.fail
  )
  data.frame(coef(summary(mod)))
})

# library(MuMIn)
# all_model_dredge <- purrr::map_df(hr_models,dredge)
# 
# test <- dredge(hr_models$water_imp, fixed='fish_factor')

#Model with lowest AIC
#Define random intercept
hr_intercept <- "(1|clusters)"
#Define models with lowest AIC
water_imp_vars <- ("fish_factor + num_childrenunder5 + quintile_nowashnomat_fac")
less5_vars <- ("fish_factor + num_hh_members + num_childrenunder5")
san_imp_vars <- ("fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac")
housing_imp_vars <- ("fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac")

#Make a list of all final models 
hr_response <- list(water_imp_vars, less5_vars, san_imp_vars, housing_imp_vars)

hr_final_models <- lapply(1:4, function(i){
  fixed <- paste0(hr_response[i], collapse= "+")
  formula <- as.formula(paste(hr_outcomes[i], "~", fixed, "+", hr_intercept))
  print(formula)
  mod <- glmer(formula, hr_analysis_dataset, family='binomial', nAGQ = 0)
  #data.frame(coef(summary(mod)))
})

names(hr_final_models) <- paste(hr_outcomes, 1:4)

#Make a list of all countries
countries <- c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")

hr_final_models_country <- lapply(1:4, function(i){
  fixed <- paste0(hr_response[i], collapse= "+")
  formula <- as.formula(paste(hr_outcomes[i], "~", fixed, "+", hr_intercept))
  print(formula)
  lapply(setNames(countries, countries), function(k) {
    y <- subset(hr_analysis_dataset, country_fac == k)
  mod <- glmer(formula, y, family='binomial', nAGQ = 0)
  #data.frame(coef(summary(mod)))
})
})
names(hr_final_models_country) <- paste(hr_outcomes, 1:4)

#Labels for tables
hr_labels <- c(
  "fish_factorFishing_community" = "Fishing community",
  "num_childrenunder5" = "Number of children <5 yoa" ,
  "num_hh_members" = "Number of household members" ,
  "quintile_nowashnomat_fac2" = "SES - Poorer" ,
  "quintile_nowashnomat_fac3" = "SES - Middle" ,
  "quintile_nowashnomat_fac4" = "SES - Richer" ,
  "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest")

hr_var_labels <- c(
    "Improved water source",
    "<5 minutes improved water ",
    "Improved sanitation",
    "Improved housing"
  )

library(sjPlot)
hr_tables <- tab_model(hr_final_models, 
                       pred.labels=hr_labels,
                       collapse.ci=TRUE,
                       dv.labels=hr_var_labels,
                       show.icc=FALSE,
                       show.re.var = FALSE,
                       show.r2 = FALSE,
                       prefix.labels = "varname",
                       p.style="a")

hr_tables_country <- lapply(1:4, function(i){
  tab_model(final_models_country[[i]],
            pred.labels=hr_labels,
            collapse.ci=TRUE,
            dv.labels=countries,
            show.icc=FALSE,
            show.re.var = FALSE,
            show.r2 = FALSE,
            terms = ("fish_factorFishing_community"),
            p.style="a")
})




#Childhood data
#kr <- read.dta13("gps_kr_20190923_dk.dta")

# kr_select <- kr %>%
#   dplyr::select(v000,
#           fishing_community,
#           country_,
#           quintile_nowashnomat,
#           v000,
#           v002,
#           v012,
#           v021,
#           v106,
#           v136,
#           v137,
#           diarrhea,
#           immun,
#           ari,
#           fever_2weeks) %>%
#   dplyr::rename(mat_age=v012,
#          medu=v106,
#          num_hh_members=v136,
#          num_childrenunder5=v137
#          )
#
# write.csv(kr_select, "kr_select.csv")

#############################################################
#Children under 5
kr_select <- read.csv("kr_select.csv")

kr_select$fish_factor <- factor(
  kr_select$fishing_community,
  levels = c(0, 1),
  labels = c("Non-Fishing community", "Fishing community")
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

kr_variables <-
  c("diarrhea", "immun", "ari", "fever_2weeks")

kr_select[kr_variables] <-
  lapply(kr_select[kr_variables], as.factor)

#Propensity score matching
kr_select$clusters <-
  paste(kr_select$v000, kr_select$v021, sep = "_")

kr_select$hh_num_str <-
  paste(kr_select$clusters, kr_select$v002, sep = "_")

kr_analysis_dataset <-
  merge(kr_select, propensity_score, by = "clusters")

kr_outcomes <- c("immun","diarrhea", "fever_2weeks", "ari")

##kr_analysis_dataset2 <-na.omit(kr_analysis_dataset)
kr_models = lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
  kr_form = paste(
    var,"~ fish_factor  + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac + 
       (1|clusters) + (1|hh_num_str)"   )
  glmer(
    kr_form,
    data = kr_analysis_dataset,
    nAGQ = 0,
    family = "binomial",
    na.action = "na.omit"
  )
})


# diarrhea_dredge <- dredge(kr_models$diarrhea, fixed="fish_factor")
# diarrhea_dredge$indicator <- "diarrhea"
# immun_dredge <- dredge(kr_models$immun, fixed="fish_factor")
# immun_dredge$indicator <- "immun"
# fever_dredge <- dredge(kr_models$fever_2weeks, fixed="fish_factor")
# fever_dredge$indicator <- "fever"
# ari_dredge <- dredge(kr_models$ari, fixed="fish_factor")
# ari_dredge$indicator <- "ari"
#Define random intercept 
kr_levels <- "(1|clusters) + (1|hh_num_str)"
#Model with lowest AIC
diarrhea_vars <- ("fish_factor + num_hh_members + quintile_nowashnomat_fac  + medu_fac")
immun_vars <- ("fish_factor + quintile_nowashnomat_fac  + medu_fac + num_childrenunder5")
fever_vars <- ("fish_factor + num_hh_members  +  num_childrenunder5 + medu_fac")
ari_vars <- ("fish_factor + medu_fac")

kr_response <- list(diarrhea_vars, immun_vars, fever_vars, ari_vars)

kr_final_models <- lapply(1:4, function(i){
  fixed <- paste0(kr_response[i], collapse= "+")
  formula <- as.formula(paste(kr_outcomes[i], "~", fixed, "+", kr_levels))
  print(formula)
  mod <- glmer(formula, kr_analysis_dataset, family='binomial', nAGQ = 0)
  #data.frame(coef(summary(mod)))
})

names(kr_final_models) <- paste(kr_outcomes, 1:4)

kr_final_models_country <- lapply(1:4, function(i){
  fixed <- paste0(kr_response[i], collapse= "+")
  formula <- as.formula(paste(kr_outcomes[i], "~", fixed, "+", random_intercepts))
  print(formula)
  lapply(setNames(countries, countries), function(k) {
    y <- subset(kr_analysis_dataset, country_fac == k)
    mod <- glmer(formula, y, family='binomial', nAGQ = 0)
    #data.frame(coef(summary(mod)))
  })
})

names(kr_final_models_country) <- paste(kr_outcomes, 1:4)


#Labels for tables
kr_labels <- c(
  "fish_factorFishing community" = "Fishing community",
  "country_fac" = "Country",
  "num_childrenunder5" = "Number of children <5 yoa" ,
  "num_hh_members" = "Number of household members" ,
  "quintile_nowashnomat_fac2" = "SES - Poorer" ,
  "quintile_nowashnomat_fac3" = "SES - Middle" ,
  "quintile_nowashnomat_fac4" = "SES - Richer" ,
  "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest",
  "medu_facPrimary" = "Maternal education - Primary",
  "medu_facSecondary" = "Maternal education - Secondary",
  "medu_facHigher" = "Maternal education - Higher"
)

kr_var_labels <- c(
  "Vaccinated",
  "Diarrhea",
  "Fever",
  "ARI"
)

kr_tables <- tab_model(kr_final_models, 
                       pred.labels=kr_labels,
                       collapse.ci=TRUE,
                       dv.labels=kr_var_labels,
                       show.reflvl = TRUE,
                       prefix.labels = "varname",
                       show.icc=FALSE,
                       show.re.var = FALSE,
                       show.r2 = FALSE,
                       p.style="a")

kr_tables_country <- lapply(1:4, function(i){
  tab_model(final_models_country[[i]],
            pred.labels=hr_labels,
            collapse.ci=TRUE,
            dv.labels=countries,
            show.icc=FALSE,
            show.re.var = FALSE,
            show.r2 = FALSE,
            terms = ("fish_factorFishing_community"),
            p.style="a")
})










