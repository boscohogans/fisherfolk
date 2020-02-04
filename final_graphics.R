
#Define custom functions
 table_fn <- function(x, df) {
   plyr::ldply(df, data.frame) %>% 
     janitor::clean_names() %>% 
     #mutate_at(c("risk_ratio", "ci_low", "ci_high", "p"), round_fn) %>% 
     mutate_at(c("risk_ratio", "ci_low", "ci_high"), round,2) %>% 
     mutate(p=round(p,3)) %>% 
     mutate(rr=paste0(risk_ratio, ' (', ci_low, ' - ', ci_high, ')')) %>% 
     select(id, parameter,rr,p) %>% 
     reshape2::melt(id.vars=c("id", "parameter"))  %>% 
     filter(parameter=="fish_factor1") %>% 
     mutate(type=x)
 }
 
 #Household 
 hr_unadj <- table_fn("Unadjusted", hr_unadj_models)
 hr_adj <- table_fn("Adjusted", hr_adj_models)
 
 hr_final_table <- rbind(hr_unadj, hr_adj)  %>% 
   select(-c("parameter")) %>% 
   rename(measure=variable) %>% 
   reshape2::melt(id.vars=c("id","measure", "type")) %>% 
   select(-variable) %>% 
   reshape2::dcast(forcats::fct_rev(id) ~ measure  + forcats::fct_rev(type), value.var="value") 
 
 #Childhood
 kr_unadj <- table_fn("Unadjusted", kr_unadj_models)
 kr_adj <- table_fn("Adjusted", kr_adj_models)
 
 kr_final_table <- rbind(kr_unadj, kr_adj)  %>% 
   select(-c("parameter")) %>% 
   rename(measure=variable) %>% 
   reshape2::melt(id.vars=c("id","measure", "type")) %>% 
   select(-variable) %>% 
   reshape2::dcast(forcats::fct_rev(id) ~ measure  + forcats::fct_rev(type), value.var="value") 
 
 rm(kr_unadj, kr_adj)

 #Combine all tables
rbind(hr_final_table, kr_final_table) %>% 
  janitor::clean_names() %>% 
  mutate(outcome=case_when(
    forcats_fct_rev_id == "water_piped" ~ "Piped water source",
    forcats_fct_rev_id == "water_imp" ~ "Improved water source",
    forcats_fct_rev_id == "san_imp" ~ "Improved sanitation",
    forcats_fct_rev_id == "less_than_5" ~ "<5 minutes to improved water",
    forcats_fct_rev_id == "hwashobs" ~ "Place for handwashing observed",
    forcats_fct_rev_id == "housing_imp" ~ "Improved housing",
    forcats_fct_rev_id == "not_immun" ~ "Not full vaccinated",
    forcats_fct_rev_id == "fever" ~ "Fever",
    forcats_fct_rev_id == "diarrhea" ~ "Diarrhea",
    forcats_fct_rev_id == "ari" ~ "Acute Respiratory Infection")) %>% 
  select(outcome, rr_unadjusted, p_unadjusted, rr_adjusted, p_adjusted) %>% 
  sjPlot::tab_df(title="Regression results", file="outputs//reg_table.doc")


##Childhood with household variables
odds <- map(kr_hr_models, sjstats::odds_to_rr)
p_value <- map(kr_hr_models, parameters::p_value)

odds_table <- rbindlist(odds, idcol = "id")
p_value_table <- rbindlist(p_value, idcol = "id")

 cbind(odds_table, p_value_table) %>% 
  janitor::clean_names() %>% 
  mutate_at(c("risk_ratio", "ci_low", "ci_high"), round,2) %>% 
  mutate(p=round(p,3)) %>% 
  mutate(rr=paste0(risk_ratio, ' (', ci_low, ' - ', ci_high, ')')) %>% 
  select(id, parameter,rr,p) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  #filter(parameter=="fish_factor1")  
  #select(-c("parameter")) %>% 
  rename(measure=variable)  %>% 
  reshape2::dcast(parameter  ~ forcats::fct_rev(id) + measure, value.var="value") 
  sjPlot::tab_df(title="Regression results", file="outputs//kr_hr_reg_table.doc")


rm(odds, p_value, odds_table, p_value_table)


# Household variables - BY COUNTRY ----------------------------------------
plyr::ldply(hr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter")) %>% 
  #select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round_fn(value)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  mutate(rr=paste0(risk_ra, ' (', ci_low, ' - ', ci_high, ')')) %>% 
  select(id, country, parameter , rr) %>% 
  reshape2::dcast(country + parameter ~ id, value.var = "rr") %>% 
  mutate(parameter= case_when(
    parameter == "(Intercept)" ~ "Intercept",
    parameter == "fish_factor1" ~ "Fishing community",
    parameter == "num_hhmembers" ~ "Number of household members",
    parameter == "asset_index_nowashnomat" ~ "Wealth asset index"),
    country = case_when(
    country == "ke" ~ "Kenya",
    country == "mw" ~ "Malawi",
    country == "tz" ~ "Tanzania",
    country == "ug" ~ "Uganda",
    country == "zm" ~ "Zambia"
    )) %>%
  filter(parameter=="Fishing community") %>% 
  select(country, parameter, water_imp, water_piped, hwashobs, less_than_5, san_imp, housing_imp) %>% 
  sjPlot::tab_df(title="Household variables by country", file = "outputs//hr_country_table.doc")


# Household variables - GRAPH ---------------------------------------------------------

hr_graph <- plyr::ldply(hr_adj_models, data.frame) %>% 
  filter(!grepl("Intercept",Parameter)) %>%
  mutate(Parameter=factor(Parameter, levels=c("asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Asset Index", "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = Risk.Ratio, y = Parameter, col=.id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_high, xmin = CI_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(Risk.Ratio,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~.id) 

# Household variables by country - GRAPH ----------------------------------

plyr::ldply(hr_country_models, data.frame) %>% 
  janitor::clean_names()  %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter"))  %>% 
  filter(id!="water_piped") %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round(value,2)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  filter(parameter=="fish_factor1") %>% 
  mutate(parameter= case_when(
    parameter == "fish_factor1" ~ "Fishing community"),
     country = case_when(
      country == "ke" ~ "Kenya",
      country == "mw" ~ "Malawi",
      country == "tz" ~ "Tanzania",
      country == "ug" ~ "Uganda",
      country == "zm" ~ "Zambia"
    )) %>% 
  ggplot(aes(x=risk_ra, y=forcats::fct_rev(id), col=country)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~forcats::fct_rev(country))




# Childhood variables - BY COUNTRY ----------------------------------------

plyr::ldply(kr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round_fn(value)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  mutate(rr=paste0(risk_ra, ' (', ci_low, ' - ', ci_high, ')')) %>% 
  select(id, country, parameter , rr) %>% 
  reshape2::dcast(country + parameter ~ id, value.var = "rr") %>% 
  mutate(parameter= case_when(
    parameter == "(Intercept)" ~ "Intercept",
    parameter == "fish_factor1" ~ "Fishing community",
    parameter == "num_hhmembers" ~ "Number of household members",
    parameter == "asset_index_nowashnomat" ~ "Wealth asset index",
    parameter == "medu_yrs" ~ "Maternal education (years)",
    parameter == "mat_age" ~ "Maternal age (years)"),
    country = case_when(
    country == "ke" ~ "Kenya",
    country == "mw" ~ "Malawi",
    country == "tz" ~ "Tanzania",
    country == "ug" ~ "Uganda",
    country == "zm" ~ "Zambia"
    )) %>%
  filter(parameter=="Fishing community") %>% 
  select(country, parameter, diarrhea, not_immun,ari, fever) %>% 
  sjPlot::tab_df(title="Childhood variables by country", file = "outputs//kr_country_table.doc")




# Childhood variables - GRAPH ---------------------------------------------


kr_graph <- plyr::ldply(kr_models, data.frame) %>% 
  filter(!grepl("Intercept",Parameter)) %>%
  mutate(Parameter=factor(Parameter, levels=c("asset_index_nowashnomat","num_hhmembers","fish_factor1", "medu_yrs", "mat_age"),
                          labels=c("Asset Index", "Number of household members", "Fishing community", "Maternal education (years)", "Maternal age (years"))) %>%
  ggplot(aes(x = Risk.Ratio, y = Parameter, col=.id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_high, xmin = CI_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(Risk.Ratio,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~.id) 


# Childhood variables by country - GRAPH ----------------------------------

plyr::ldply(kr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round(value,2)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  filter(parameter=="fish_factor1") %>% 
  mutate(parameter= case_when(
    parameter == "(Intercept)" ~ "Intercept",
    parameter == "fish_factor1" ~ "Fishing community",
    parameter == "num_hhmembers" ~ "Number of household members",
    parameter == "asset_index_nowashnomat" ~ "Wealth asset index",
    parameter == "medu_yrs" ~ "Maternal education (years)",
    parameter == "mat_age" ~ "Maternal age (years)"),
    country = case_when(
      country == "ke" ~ "Kenya",
      country == "mw" ~ "Malawi",
      country == "tz" ~ "Tanzania",
      country == "ug" ~ "Uganda",
      country == "zm" ~ "Zambia"
    )) %>% 
  ggplot(aes(x=risk_ra, y=forcats::fct_rev(id), col=country)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~country)


###Combine household and childhood
hhold_country <- plyr::ldply(hr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round(value,2)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  filter(parameter=="fish_factor1") %>% 
  mutate(parameter= case_when(
    parameter == "fish_factor1" ~ "Fishing community"),
     country = case_when(
      country == "ke" ~ "Kenya",
      country == "mw" ~ "Malawi",
      country == "tz" ~ "Tanzania",
      country == "ug" ~ "Uganda",
      country == "zm" ~ "Zambia"))


child_country <- plyr::ldply(kr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round(value,2)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  filter(parameter=="fish_factor1") %>% 
  mutate(parameter= case_when(
    parameter == "fish_factor1" ~ "Fishing community"),
    country = case_when(
      country == "ke" ~ "Kenya",
      country == "mw" ~ "Malawi",
      country == "tz" ~ "Tanzania",
      country == "ug" ~ "Uganda",
      country == "zm" ~ "Zambia"))

all_country <- rbind(hhold_country, child_country)

all_country %>% 
  filter(id %in% c("water_imp", "housing_imp", "not_immun", "fever") ) %>% 
ggplot(aes(x=risk_ra, y=forcats::fct_rev(country), col=country)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~forcats::fct_rev(id))

  


# Childhood variables with household variables ----------------------------

plyr::ldply(kr_hr_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-odds_ratio) %>% 
  mutate_at(c("risk_ratio", "ci_low", "ci_high"), round_fn) %>% 
  mutate(rr=paste0(risk_ratio, ' (', ci_low, ' - ', ci_high, ')')) %>% 
  select(id, parameter,rr) %>% 
  reshape2::melt(id.vars=c("id", "parameter"))  %>% 
  mutate(parameter= case_when(
    parameter == "(Intercept)" ~ "Intercept",
    parameter == "fish_factor1" ~ "Fishing community",
    parameter == "num_hhmembers" ~ "Number of household members",
    parameter == "asset_index_nowashnomat" ~ "Wealth asset index",
    parameter == "medu_yrs" ~ "Maternal education (years)",
    parameter == "mat_age" ~ "Maternal age (years)",
    parameter == "water_imp" ~ "Improved water source",
    parameter == "water_piped" ~ "Piped water source",
    parameter == "hwashobs" ~ "Place for handwashing observed",
    parameter == "less_than_5" ~ "5 minutes to improved water",
    parameter == "san_imp" ~ "Improved sanitation facilities",
    parameter == "housing_imp" ~ "Improved housing"
    )) %>% 
  reshape2::dcast(parameter~ forcats::as_factor(id)+variable, value.var="value") %>% 
  sjPlot::tab_df(title="Childhood variables", file = "outputs//kr_hr_table.doc")

plyr::ldply(kr_hr_country_models, data.frame) %>% 
  janitor::clean_names() %>% 
  select(-contains("odds")) %>% 
  select(-c("mw_parameter", "tz_parameter", "ug_parameter", "zm_parameter")) %>% 
  rename(parameter=ke_parameter) %>% 
  reshape2::melt(id.vars=c("id", "parameter")) %>% 
  mutate(country=stringr::str_sub(variable,1,2),
         variable=stringr::str_sub(variable,4,10),
         value=round_fn(value)) %>% 
  reshape2::dcast(parameter + country +id ~  variable, value.var="value") %>% 
  mutate(rr=paste0(risk_ra, ' (', ci_low, ' - ', ci_high, ')')) %>% 
  select(id, country, parameter , rr) %>% 
  reshape2::dcast(country + parameter ~ id, value.var = "rr") %>% 
  mutate(parameter= case_when(
    parameter == "(Intercept)" ~ "Intercept",
    parameter == "fish_factor1" ~ "Fishing community",
    parameter == "num_hhmembers" ~ "Number of household members",
    parameter == "asset_index_nowashnomat" ~ "Wealth asset index",
    parameter == "medu_yrs" ~ "Maternal education (years)",
    parameter == "mat_age" ~ "Maternal age (years)"),
    country = case_when(
      country == "ke" ~ "Kenya",
      country == "mw" ~ "Malawi",
      country == "tz" ~ "Tanzania",
      country == "ug" ~ "Uganda",
      country == "zm" ~ "Zambia"
    )) 
  filter(parameter %in% "fish") 
  #select(country, parameter, diarrhea, not_immun,ari, fever) 

  sjPlot::tab_df(title="Childhood variables by country", file = "outputs//kr_country_table.doc")


# Childhood variables with household variables - GRAPH --------------------

kr_hr_graph <- plyr::ldply(kr_hr_models, data.frame) %>% 
  filter(!grepl("Intercept",Parameter)) %>%
  mutate(term=factor(Parameter, levels=c("housing_imp", "san_imp", "less_than_5", "hwashobs", "water_piped","water_imp",
                                    "mat_age", "medu_yrs", "asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Improved housing", "Improved sanitation", "Improved water(<5 mins)", "Handwashing observed", "Piped water", 
                              "Improved water source", "Maternal age (years)", "Maternal education (years)", 
                              "Asset Index", "Number of household members", "Fishing community"))) %>% 
  ggplot(aes(x = Risk.Ratio, y = Parameter, col=.id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = CI_high, xmin = CI_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(Risk.Ratio,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10() +
  ylab("") +
  xlab("Risk ratio (log scale)") +
  facet_wrap(~.id) 


