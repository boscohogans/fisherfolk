#Concentration curve


library(scales)
vars_ses <- hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(ses_percent = ntile(asset_index_nowashnomat,10)) %>%
           select(water_imp,water_piped,san_imp,housing_imp, ses_percent,fishing_community,country)

library(IC2)

curveConcent(vars_ses[,"water_imp"], y=vars_ses[,"ses_percent"], col="red")
curveConcent(vars_ses[,"water_piped"], y=vars_ses[,"ses_percent"], col="blue", add=TRUE)
curveConcent(vars_ses[,"san_imp"], y=vars_ses[,"ses_percent"], col="green", add=TRUE)
curveConcent(vars_ses[,"housing_imp"], y=vars_ses[,"ses_percent"], col="orange", add=TRUE)
title(main="Overall access to household facilities")
legend("topleft", legend = c("Improved water source", "Piped water source", "Improved sanitation", "Improved housing"), 
       col=c("red", "blue", "green", "orange"), lty=rep(1, 4))


library(reldist)
ginicities <- aggregate(housing_imp ~ fishing_community,
                        data = vars_ses,
                        FUN = "gini")

fish_ses <- vars_ses %>%
  #arrange(asset_index_nowashnomat) %>%
  #mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
  filter(fishing_community==1) %>%
  select(water_imp,water_piped,san_imp,housing_imp, ses_percent, country)

non_fish_ses <- vars_ses %>%
  #arrange(asset_index_nowashnomat) %>%
  #mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
  filter(fishing_community!=1) %>%
  select(water_imp,water_piped,san_imp,housing_imp, ses_percent, country)


ineq_fun <- function(x,y, column) {
  curveConcent(x[,column], x[,"ses_percent"], col="red")
  curveConcent(y[,column], y[,"ses_percent"], col="red",lty=2, add=TRUE)
  title(main=paste(column))
  legend("topleft", legend = c("Fishing community", "Non-Fishing community"),col=c("red", "red"), lty=c(1,2))
}

list <- c("water_imp", "water_piped", "san_imp", "housing_imp")

#Improved water source
ineq_fun(fish_ses, non_fish_ses, "water_imp")
#Piped water source
ineq_fun(fish_ses, non_fish_ses, "water_piped")
#Improved sanitation
ineq_fun(fish_ses, non_fish_ses, "san_imp")
#Improved housing
ineq_fun(fish_ses, non_fish_ses, "housing_imp")



##There seems to be a large degree of heterogeneity between the countries
##Plot individual curves for each country 
fish_ses_country <- fish_ses %>% group_by(country) %>% nest()

non_fish_ses_country <- non_fish_ses %>% group_by(country) %>% nest()

ineq_fun(fish_ses_country$data[1], non_fish_ses_country$data[1], "water_imp")




