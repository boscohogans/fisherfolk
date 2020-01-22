#Tables

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

##setwd("outputs//")

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
  title='Household indicators'
)


#Childhood table
kr_var_labels <- c("Diarrhea (previous 2 weeks",
                   "Incomplete vaccination schedule",
                   "ARI (previous 2 weeks)",
                   "Fever (previous 2 weeks)")

tab_model(
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
  title='Childhood indicators'
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
  title='Childhood indicators with household indicators'
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
    terms = ("fish_factor1"),
    p.style = "a",
    title = kr_var_labels[i]
  )
})

kr_hr_tables_country <- lapply(1:4, function(i) {
  tab_model(
    kr_with_hr_models[[i]],
    pred.labels = variable_labels,
    collapse.ci = TRUE,
    dv.labels = countries,
    show.icc = FALSE,
    show.re.var = FALSE,
    show.r2 = FALSE,
    terms = ("fish_factor1"),
    p.style = "a",
    title = kr_with_hr_models[i]
  )
})


#Graphics

#Household variables
hr_graph <- hr_results %>%
  filter(!grepl("Intercept",term)) %>%
  mutate(term=factor(term, levels=c("asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Asset Index", "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(estimate,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_wrap(~id) 

ggsave("outputs//hr_reg_results.pdf", plot = last_plot(), width=297, height=210, units="mm", device="pdf")

#Childhood variables 
kr_graph <- kr_results %>%
  filter(!grepl("Intercept",term)) %>%
  mutate(term=factor(term, levels=c("mat_age", "medu_yrs", "asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Maternal age (years)", "Maternal education (years)", "Asset Index", 
                              "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(estimate,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_wrap(~id)

ggsave("outputs//kr_reg_results.pdf", plot = last_plot(), width=297, height=210, units="mm", device="pdf")


#Household by country
hr_graph_country <- hr_country_data_results %>%
  filter(!grepl("Intercept",term)) %>%
  filter(outcome!="water_piped") %>%
  mutate(term=factor(term, levels=c("asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Asset Index", "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(estimate,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_grid(id~outcome)

ggsave("outputs//hr_country_reg_results.pdf", plot = last_plot(), width=400, height=210, units="mm", device="pdf")

kr_graph_country <- kr_country_data_results %>%
  filter(!grepl("Intercept",term)) %>%
  mutate(term=factor(term, levels=c("mat_age", "medu_yrs", "asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Maternal age (years)", "Maternal education (years)", "Asset Index", 
                              "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(estimate,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_grid(id~outcome)

ggsave("outputs//kr_country_reg_results.pdf", plot = last_plot(), width=297, height=210, units="mm", device="pdf")


kr_with_hr_graph <- kr_with_hr_results %>%
  filter(!grepl("Intercept",term)) %>%
  #filter(id!="ari") %>%
  mutate(term=factor(term, levels=c("housing_imp", "san_imp", "less_than_5", "hwashobs", "water_piped","water_imp",
                                    "mat_age", "medu_yrs", "asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Improved housing", "Improved sanitation", "Improved water(<5 mins)", "Handwashing observed", "Piped water", 
                              "Improved water source", "Maternal age (years)", "Maternal education (years)", 
                              "Asset Index", "Number of household members", "Fishing community"))) %>%
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  geom_text(aes(label=round(estimate,2)), vjust=-0.5) +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_wrap(~id, ncol=2)

ggsave("outputs//kr_hr_reg_results.pdf", plot = last_plot(), width=297, height=210, units="mm", device="pdf")


kr_with_hr_graph_country <- kr_hr_country_data_results %>%
  filter(!grepl("Intercept",term)) %>% 
  na.omit () %>% 
  mutate(term=factor(term, levels=c("housing_imp", "san_imp", "less_than_5", "hwashobs", "water_piped","water_imp",
                                    "mat_age", "medu_yrs", "asset_index_nowashnomat","num_hhmembers","fish_factor1"),
                     labels=c("Improved housing", "Improved sanitation", "Improved water(<5 mins)", "Handwashing observed", "Piped water", 
                              "Improved water source", "Maternal age (years)", "Maternal education (years)", 
                              "Asset Index", "Number of household members", "Fishing community"))) %>% 
  ggplot(aes(x = estimate, y = term, col=id)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2, fill = "orange") +
  theme_bw()+
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  facet_grid(id~outcome,scales="free")

ggsave("outputs//kr_hr_country_reg_results.pdf", plot = last_plot(), width=297, height=210, units="mm", device="pdf")

library(gridExtra)

all_plots <- list(hr_graph, hr_graph_country, kr_graph, kr_graph_country, kr_with_hr_graph, kr_with_hr_graph_country)

plots = do.call(marrangeGrob, c(all_plots, list(nrow = 2, ncol = 1)))
ggsave("outputs//multipage_plot.pdf", plots, width = 21, height = 29.7, units = "cm")

library(pdftools)

pdf("outputs//all_plots.pdf")
pdf_combine(c("outputs//hr_reg_results.pdf", "outputs//hr_country_reg_results.pdf",
              "outputs//kr_reg_results.pdf", "outputs//kr_country_reg_results.pdf",
              "outputs//kr_hr_reg_results.pdf","outputs//kr_hr_country_reg_results.pdf", outputs="outputs//all_plots.pdf"))
dev.off()


# pdf("outputs//all_plots.pdf", onefile = TRUE)
# plots
# dev.off()
# 
# 
# pdf("outputs//all_plots.pdf", onefile=TRUE)
# for (i in seq(length(all_plots))) {
#   do.call("grid.arrange", all_plots[[i]])  
# }
# dev.off()
# 
# for(i in all_plots){
#   
#   pdf(paste0(i, ".pdf"))
#   print(final.plot)
#   dev.off()
# }
# 
# 
