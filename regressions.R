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
      "KE6",
      "MW7",
      "TZ7",
      "UG7",
      "ZM6",
      "KE5",
      "MW5",
      "TZ5",
      "UG6",
      "ZM5"
    )
  )

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

variables <-
  c(
    "water_imp",
    "san_imp",
    "less_than_5",
    "housing_imp"
  )

hr_select[variables] <- lapply(hr_select[variables], as.factor)

library(table1)

#Propensity score matching
propensity_score <-
  read.csv("propensity_score_matching_20190923.csv")

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

hr_analysis_dataset$clusters <-
  paste(hr_analysis_dataset$hv000, hr_analysis_dataset$hv021,  sep = "_")


##Build full model with all variables 

options(na.action = na.exclude)

hr_outcomes <- c("water_imp", "less_than_5", "san_imp", "housing_imp")
library(lme4)
hr_models <- lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
  form = paste( var, "~ fish_factor  + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
                (1 | clusters)")
  glmer(form, data=hr_analysis_dataset,nAGQ = 0,family = binomial
  )
})

library(MuMIn)
water_imp_dredge <- dredge(hr_models$water_imp, fixed="fish_factor")
water_imp_dredge$indicator <- "water_imp"
less5_dredge <- dredge(hr_models$less_than_5, fixed="fish_factor")
less5_dredge$indicator <- "water_imp"
san_imp_dredge <- dredge(hr_models$san_imp, fixed="fish_factor")
san_imp_dredge$indicator <- "water_imp"
housing_imp_dredge <- dredge(hr_models$housing_imp, fixed="fish_factor")
housing_imp_dredge$indicator <- "water_imp"

#Model with lowest AIC
#Improved water source
water_imp_final <-
  glmer(
    water_imp ~ fish_factor  + num_childrenunder5 + quintile_nowashnomat_fac +
      (1 | clusters),
    nAGQ = 1,
    data = hr_analysis_dataset,
    family = "binomial"
  )

water_coefs <- data.frame(coef(summary(water_imp_final)))
water_imp_var <- unique(rownames(water_coefs))

summary(water_imp_final)

countries <- c("Kenya", "Malawi", "Tanzania", "Uganda", "Zambia")
water_imp_country <-
  lapply(setNames(countries, countries), function(k) {
    y <- subset(hr_analysis_dataset, country_fac == k)
    mod <-
      glmer(
        water_imp ~ fish_factor  + num_childrenunder5 + quintile_nowashnomat_fac +
          (1 | clusters),
        nAGQ = 0,
        data = y,
        family = binomial
      )
    data.frame(coef(summary(mod)))
  })

library(plyr)
water_imp_country_coefs <- ldply(water_imp_country, data.frame)
water_imp_country_coefs$vars <- water_imp_var

###WATER LESS THAN 5 MINUTES FROM HOUSEHOLD  
less5_final <-
  glmer(
    less_than_5 ~ fish_factor + num_hh_members + num_childrenunder5  +
      (1 | clusters),
    data = hr_analysis_dataset,
    nAGQ = 0,
    family = "binomial"
  )

less5_final_country <-
  lapply(setNames(countries, countries), function(k) {
    y <- subset(hr_analysis_dataset, country_fac == k)
    mod <-
      glmer(
        less_than_5 ~ fish_factor + num_hh_members + num_childrenunder5  +
          (1 | clusters),
        data = hr_analysis_dataset,
        nAGQ = 0,
        family = "binomial"
      )
    coef(summary(mod))
  })

less5_final_coefs <- ldply(less5_final_country, data.frame)

#IMPROVED SANITATION
san_imp_final <-
  glmer(
    san_imp ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
      (1 | clusters),
    data = hr_analysis_dataset,
    nAGQ = 0,
    family = "binomial"
  )

san_imp_final_country <-
  lapply(setNames(countries, countries), function(k) {
    y <- subset(hr_analysis_dataset, country_fac == k)
    mod <-
      glmer(
        san_imp ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
          (1 | clusters),
        data = hr_analysis_dataset,
        nAGQ = 0,
        family = "binomial"
      )
    coef(summary(mod))
  })

san_imp_final_coefs <- ldply(san_imp_final_country, data.frame)

###Improved housing 
housing_imp_final <-
  glmer(
    housing_imp ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
      (1 | clusters)  ,
    data = hr_analysis_dataset,
    nAGQ = 0,
    family = "binomial"
  )

housing_imp_final_country <-
  lapply(setNames(countries, countries), function(k) {
    y <- subset(hr_analysis_dataset, country_fac == k)
    mod <-
      glmer(
        housing_imp ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac +
          (1 | clusters)  ,
        data = hr_analysis_dataset,
        nAGQ = 0,
        family = "binomial"
      )
    coef(summary(mod))
  })

housing_imp_final_coefs <- ldply(housing_imp_final_country, data.frame)

library(sjPlot)
plot_model(housing_imp_final)


library(GLMMadaptive)

fm <- mixed_model(fixed= housing_imp ~ fish_factor, random = ~fish_factor | country_fac, data=hr_analysis_dataset,
                  family=binomial)

plot_data <- effectPlotData(fm,hr_analysis_dataset)

library(lattice)

xyplot(pred + low + upp ~ fish_factor | country_fac, data=plot_data)

library(effects)
plot(predictorEffect("fish_factor", fm), type="response")

all_hr_dredge <- rbind(water_imp_dredge, less_slope_dredge, san_imp_dredge, housing_imp_dredge)


p1 <- c(
  "fish_factorFishing community" = "Fishing community",
  "num_childrenunder5" = "Number of children <5 yoa" ,
  "num_hh_members" = "Number of household members" ,
  "quintile_nowashnomat_fac2" = "SES - Poorer" ,
  "quintile_nowashnomat_fac3" = "SES - Middle" ,
  "quintile_nowashnomat_fac4" = "SES - Richer" ,
  "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest"
)

hr_tables <- tab_model(
  water_imp_final,
  water_imp_noslope,
  less5_final,
  less5_noslope,
  san_imp_final,
  san_imp_noslope,
  housing_imp_final,
  housing_imp_noslope,
  pred.labels = p1,
  collapse.ci = TRUE,
  dv.labels = c(
    "Improved water source (random slope)",
    "Improved water source",
    "<5 minutes improved water (random slope)",
    "<5 minutes improved water ",
    "Improved sanitation (random slope)",
    "Improved sanitation",
    "Improved housing (random slope)",
    "Improved housing"
  ),
  p.style = "a"
)


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

kr_outcomes <- c("diarrhea", "immun", "fever_2weeks", "ari")

kr_models = lapply(setNames(kr_outcomes, kr_outcomes), function(var) {
  kr_form = paste(
    var,
    "~ fish_factor  + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac +
    (1 | clusters) + (1 | hh_num_str) + (fish_factor | country_fac)"
  )
  glmer(
    kr_form,
    data = kr_analysis_dataset,
    nAGQ = 0,
    family = "binomial",
    na.action = na.omit
  )
})

diarrhea_dredge <- dredge(kr_models$diarrhea, fixed="fish_factor")
diarrhea_dredge$indicator <- "diarrhea"
immun_dredge <- dredge(kr_models$immun, fixed="fish_factor")
immun_dredge$indicator <- "immun"
fever_dredge <- dredge(kr_models$fever_2weeks, fixed="fish_factor")
fever_dredge$indicator <- "fever"
ari_dredge <- dredge(kr_models$ari, fixed="fish_factor")
ari_dredge$indicator <- "ari"

#Model with lowest AIC
#DIARRHEA
diarrhea_final<-
  glmer(
    diarrhea ~ fish_factor + num_hh_members + quintile_nowashnomat_fac  + medu_fac +
      (1 | clusters) + (1 | hh_num_str) + (1 + fish_factor | country_fac),
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

diarrhea_noslope<-
  glmer(
    diarrhea ~ fish_factor + num_hh_members + quintile_nowashnomat_fac  + medu_fac +
      (1 | clusters) + (1 | hh_num_str) ,
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

diarrhea_margins <-
  ggpredict(diarrhea_final, c("country_fac"), type = "re")
diarrhea_margins$indicator <- "diarrhea"


#VACCINATION
immun_final <-
  glmer(
    immun ~ fish_factor + quintile_nowashnomat_fac  + medu_fac + num_childrenunder5 +
      (1 | clusters) + (1 | hh_num_str) + (1 + fish_factor | country_fac),
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

immun_noslope <-
  glmer(
    immun ~ fish_factor + quintile_nowashnomat_fac  + medu_fac + num_childrenunder5 +
      (1 | clusters) + (1 | hh_num_str) ,
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )


immun_margins <-
  ggpredict(immun_final, c("country_fac"), type = "re")
immun_margins$indicator <- "vaccine"


#FEVER
fever_final <-
  glmer(
    fever_2weeks ~ fish_factor + num_hh_members  +  num_childrenunder5 + medu_fac +
      (1 | clusters) + (1 | hh_num_str) + (1+ fish_factor | country_fac),
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

#FEVER
fever_noslope <-
  glmer(
    fever_2weeks ~ fish_factor + num_hh_members  +  num_childrenunder5 + medu_fac +
      (1 | clusters) + (1 | hh_num_str),
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

fever_margins <-
  ggpredict(fever_final, c("country_fac"), type = "re")
fever_margins$indicator <- "fever"

#ARI
ari_final <-
  glmer(
    ari ~ fish_factor + medu_fac +
      (1 | clusters) + (1 | hh_num_str) + (1 + fish_factor | country_fac),
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

ari_noslope <-
  glmer(
    ari ~ fish_factor + medu_fac +
      (1 | clusters) + (1 | hh_num_str) ,
    data=kr_analysis_dataset,  nAGQ = 0,family = "binomial", na.action=na.omit
  )

ari_margins <-
  ggeffect(ari_final, c("country_fac"), type = "re")
ari_margins$indicator <- "ari"

library(effects)

effs <- as.data.frame(effect(c("fish_factor | country_fac"),ari_slope))

preds <- merTools::predictInterval(ari_slope)

lmerTest::ranova(ari_slope)



kr_analysis_dataset %>% 
  # save predicted values
  mutate(pred_dist = fitted(ari_slope)) %>% 
  # graph
  ggplot(aes(x=fish_factor, y=pred_dist, group=country_fac, color=country_fac)) + theme_classic() +
  geom_line(size=1) 


#Append dataframes
all_kr_margins <- rbind(diarrhea_margins, immun_margins, fever_margins, ari_margins)

labels <- c(
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


kr_tables <- tab_model(
  diarrhea_final,
  diarrhea_noslope,
  immun_final,
  immun_noslope,
  fever_final,
  fever_noslope,
  ari_final, 
  ari_noslope,
  pred.labels = p1,
  collapse.ci = TRUE,
  dv.labels = c(
    "Diarrhea (random slope)",
    "Diarrhea",
    "Vaccinated (random slope)",
    "Vaccinated",
    "Fever (random slope)",
    "Fever",
    "ARI (random slope)",
    "ARI"
  ),
  p.style = "a"
)





all_models <- tab_model(
  water_imp_final,
  less5_slope,
  san_imp_slope,
  housing_imp_slope,
  diarrhea_slope,
  immun_slope,
  fever_slope,
  ari_slope,
  pred.labels = labels,
  collapse.ci = TRUE,
  dv.labels = c("Improved water source","<5 minutes to improved water source","Improved sanitation","Improved housing",
                "Diarrhea", "Vaccinations", "ARI", "Fever"),
  rm.terms = "(Intercept)"
)

# p2 <- c(
#   "fish" = "Fishing community",
#   "num_childrenunder5" = "Number of children <5 yoa" ,
#   "num_hh_members" = "Number of household members" ,
#   "quintile_nowashnomat_fac2" = "SES - Poorer" ,
#   "quintile_nowashnomat_fac3" = "SES - Middle" ,
#   "quintile_nowashnomat_fac4" = "SES - Richer" ,
#   "quintile_nowashnomat_fac5 (least deprived)" = "SES - Richest",
#   "medu_facPrimary" = "Maternal education - Primary",
#   "medu_facSecondary" = "Maternal education - Secondary",
#   "medu_facHigher" = "Maternal education - Higher"
# )
#
# tab_model(
#   diarrhea,
#   immun,
#   ari,
#   fever,
#   pred.labels = p2,
#   collapse.ci = TRUE,
#   dv.labels = c("Diarrhea", "Vaccinations", "ARI", "Fever"),
#   p.style = "a",
#   rm.terms = "(Intercept)"
# )
