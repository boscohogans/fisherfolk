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
    select(fish_factor, water_imp, ses_group, cumulative) %>%
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
hr_conc_plots
ggsave("outputs//hr_conc_plots.pdf", plot = last_plot(), width=20, height=20, units="cm", device="pdf")


###Childhood variables
kr_fish_gps$fish_factor <- factor(kr_fish_gps$fishing_community, labels=c("Non-fishing community", "Fishing community"))

not_immun <- conc(kr_fish_gps, not_immun) + labs(title="Not fully vaccinate")
diarrhea <- conc(kr_fish_gps, diarrhea) + labs(title="Diarrhea in last 2 weeks")
ari <- conc(kr_fish_gps, ari) + labs(title="ARI in last 2 weeks")
fever <- conc(kr_fish_gps, fever) + labs(title="Fever in last 2 weeks")
breastfed <- conc(kr_fish_gps, bfeeding) + labs(title="Ever breastfed")

kr_conc_plots <- ggarrange(not_immun, diarrhea, ari, fever, breastfed, ncol=2, nrow=3, common.legend = TRUE, legend="top")

rm(water_imp,water_piped, less5, hwashobs, san_imp, housing_imp, not_immun, diarrhea, ari, fever, breastfed)



