#Concentration curve

#Definte function
conc <- function(df, var) {
  var <- enquo(var)
  
  df %>%
    arrange(asset_index_nowashnomat) %>%
    group_by(fish_factor) %>%
    mutate(cumulative=cumsum(!!var)/sum(!!var),
           ses_group=ntile(asset_index_nowashnomat,100),
           ses_group=(ses_group/100)) %>%
    group_by(fish_factor, ses_group) %>%
    select(fish_factor, !!var, ses_group, cumulative) %>%
    slice(n()) %>%
    ggplot(aes(x=ses_group, y=cumulative, group=fish_factor, col=factor(fish_factor))) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, colour = "black") +
    theme(legend.title=element_blank()) +
    #labs(title = var) +
    ylab("Cumulative share of variable") + 
    xlab("Cumulative % of population, ranked by SES")
}

#Make factor variable for fishing communities
hr_fish_gps$fish_factor <- factor(hr_fish_gps$fishing_community, labels=c("Non-fishing community", "Fishing community"))

#Run function over each variable
water_imp <- conc(hr_fish_gps, water_imp)+ labs(title="Improved water source")
water_piped <- conc(hr_fish_gps,water_piped) + labs(title="Piped water source")
less5 <- conc(hr_fish_gps,less_than_5) + labs(title="<5 minutes to improved water source")
hwashobs <- conc(hr_fish_gps,hwashobs) + labs(title="Place for handwashing observed")
san_imp <- conc(hr_fish_gps,san_imp) + labs(title="Improved sanitation")
housing_imp <- conc(hr_fish_gps,housing_imp) + labs(title="Improved housing")

#Arrange each plot
hr_conc_plots <- ggarrange(water_imp, water_piped, less5, hwashobs, san_imp, housing_imp, ncol=2, nrow=3, common.legend = TRUE, legend="top")
#hr_conc_plots
ggsave("outputs//hr_conc_plots.pdf", plot = last_plot(), width=20, height=20, units="cm", device="pdf")


###Childhood variables
kr_fish_gps_upd$fish_factor <- factor(kr_fish_gps_upd$fishing_community, labels=c("Non-fishing community", "Fishing community"))

childhood <- map2(kr_fish_gps_upd, kr_outcomes, conc)

not_immun <- conc(kr_fish_gps_upd, not_immun) + labs(title="Not fully vaccinate")
diarrhea <- conc(kr_fish_gps_upd, diarrhea) + labs(title="Diarrhea in last 2 weeks")
ari <- conc(kr_fish_gps_upd, ari) + labs(title="ARI in last 2 weeks")
fever <- conc(kr_fish_gps_upd, fever) + labs(title="Fever in last 2 weeks")
breastfed <- conc(kr_fish_gps_upd, bfeeding) + labs(title="Ever breastfed")

kr_conc_plots <- ggarrange(not_immun, diarrhea, ari, fever, breastfed, ncol=2, nrow=3, common.legend = TRUE, legend="top")
ggsave("outputs//kr_conc_plots.pdf", plot = last_plot(), width=20, height=20, units="cm", device="pdf")

rm(water_imp,water_piped, less5, hwashobs, san_imp, housing_imp, not_immun, diarrhea, ari, fever, breastfed)


# hr_test <- hr_fish_gps
# 
# hr_fish_gps %>%
#   group_by(fishing_community) %>%
#   summarize(sum(water_imp))
# 
# kr_fish_gps %>%
#   arrange(asset_index_nowashnomat) %>%
#   group_by(fish_factor) %>%
#   mutate(cumulative=cumsum(not_immun)/sum(not_immun),
#          ses_group=ntile(asset_index_nowashnomat,10),
#          ses_group=(ses_group/10)) %>%
#   group_by(fish_factor, ses_group) %>%
#   select(fish_factor, not_immun, ses_group, cumulative) %>%
#   slice(n()) %>%
#   arrange(ses_group) %>%
#   ggplot(aes(x=ses_group, y=cumulative, group=fish_factor, col=factor(fish_factor))) +
#   geom_line() +
#   geom_abline(intercept = 0, slope = 1, colour = "black") +
#   theme(legend.title=element_blank()) +
#   #labs(title = var) +
#   ylab("Cumulative share of variable") + 
#   xlab("Cumulative % of population, ranked by SES")
#   
# 
#   
#   arrange(asset_index_nowashnomat) %>%
#   mutate(ses_group=ntile(asset_index_nowashnomat,10))
#   group_by(fish_factor, ses_group) %>%
#   mutate(cumulative=cumsum(water_imp)/sum(water_imp)) %>% 
#   slice(n()) %>%
#     
#   
#          ses_group=ntile(asset_index_nowashnomat,10),
#          ses_group=(ses_group/10)) 
#   #group_by(fish_factor, ses_group) %>%
#   #select(fish_factor, ses_group, cumulative) %>%
#   #arrange(ses_group, cumulative)
# 
# 
#   ggplot(aes(x=ses_group, y=cumulative, group=fish_factor, col=factor(fish_factor))) +
#   geom_line() +
#   geom_abline(intercept = 0, slope = 1, colour = "black") +
#   theme(legend.title=element_blank()) +
#   #labs(title = var) +
#   ylab("Cumulative share of variable") + 
#   xlab("Cumulative % of population, ranked by SES")
# 
