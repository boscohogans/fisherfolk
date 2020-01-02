#Labels for tables
variable_labels <- c(
  "fish_factor1" = "Fishing community",
  "num_hhmembers" = "Number of household members" ,
  "asset_index_nowashnomat" = "SES (continous)",
  "medu_yrs" = "Maternal education (years)",
  "mat_age" = "Maternal age (ses)",
  "water_imp" = "Improved water source",
  "water_piped" = "Piped water",
  "hwashobs" = "Handwashing observed",
  "less_than_5" = "Improved water (<5 minutes from household)",
  "san_imp" = "Improved sanitation",
  "housing_imp" = "Improved housing")

#Household table
hr_var_labels <- c(
  "Improved water source",
  "Piped water",
  "Handwashing observed",
  "Improved water(<5 min)",
  "Improved sanitation",
  "Improved housing"
)

setwd("outputs//")

hr_reg_table <- tab_model(
  hr_models,
  pred.labels = variable_labels,
  collapse.ci = TRUE,
  dv.labels = hr_var_labels,
  show.icc = FALSE,
  show.re.var = FALSE,
  show.r2 = FALSE,
  prefix.labels = "varname",
  p.style = "a",
  order.terms=c(2,3,4,1),
  title='Household indicators',
  file="hr_reg_table.html"
)


#Childhood table
kr_var_labels <- c("Diarrhea (previous 2 weeks",
                   "Incomplete vaccination schedule",
                   "ARI (previous 2 weeks)",
                   "Fever (previous 2 weeks)")

kr_reg_table <- tab_model(
  kr_models,
  pred.labels = variable_labels,
  collapse.ci = TRUE,
  dv.labels = kr_var_labels,
  show.reflvl = TRUE,
  prefix.labels = "varname",
  show.icc = FALSE,
  show.re.var = FALSE,
  show.r2 = FALSE,
  p.style = "a",
  #show.reflvl = TRUE,
  order.terms = c(2,3,4,5,6,1),
  title='Childhood indicators',
  file="kr_reg_table.html"
)

kr_hr_reg_table <- tab_model(
  kr_with_hr_models,
  pred.labels = variable_labels,
  collapse.ci = TRUE,
  dv.labels = kr_var_labels,
  show.reflvl = TRUE,
  prefix.labels = "varname",
  show.icc = FALSE,
  show.re.var = FALSE,
  show.r2 = FALSE,
  p.style = "a",
  #show.reflvl = TRUE,
  order.terms = c(2,3,4,5,6,7,8,9,10,11,12,1),
  title='Childhood indicators with household indicators',
  file="kr_hr_reg_table.html"
)


hr_country_table <- lapply(1:6, function(i) {
  tab_model(
    hr_models_country[[i]],
    pred.labels = variable_labels,
    collapse.ci = TRUE,
    dv.labels = countries,
    show.icc = FALSE,
    show.re.var = FALSE,
    show.r2 = FALSE,
    terms = ("fish_factor1"),
    p.style = "a",
    title = hr_var_labels[i]
  )
})


kr_tables_country <- lapply(1:4, function(i) {
  tab_model(
    kr_models_country[[i]],
    pred.labels = variable_labels,
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
