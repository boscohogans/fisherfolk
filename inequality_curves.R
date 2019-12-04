#Concentration curve


hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(cumulative=cumsum(water_imp)/sum(water_imp),
         ses_decile=ntile(asset_index_nowashnomat,100))%>%
  group_by(ses_decile) %>%
  select(ses_decile, cumulative, country, fishing_community) %>%
  slice(n()) %>%
  ggplot(aes(x=ses_decile, y=cumulative)) +
  geom_line() 



library(scales)
vars_ses <- hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(ses_percent = ntile(asset_index_nowashnomat,1000)) %>%
           select(water_imp,water_piped,san_imp,housing_imp, asset_index_nowashnomat,fishing_community,country)

library(IC2)

curveConcent(vars_ses[,"water_imp"], y=vars_ses[,"asset_index_nowashnomat"], col="red")
curveConcent(vars_ses[,"water_piped"], y=vars_ses[,"asset_index_nowashnomat"], col="blue", add=TRUE)
curveConcent(vars_ses[,"san_imp"], y=vars_ses[,"asset_index_nowashnomat"], col="green", add=TRUE)
curveConcent(vars_ses[,"housing_imp"], y=vars_ses[,"asset_index_nowashnomat"], col="orange", add=TRUE)
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
  select(water_imp,water_piped,san_imp,housing_imp, asset_index_nowashnomat, country)

non_fish_ses <- vars_ses %>%
  #arrange(asset_index_nowashnomat) %>%
  #mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
  filter(is.na(fishing_community)) %>%
  select(water_imp,water_piped,san_imp,housing_imp, asset_index_nowashnomat, country)


ineq_fun <- function(x,y, column) {
  curveConcent(x[,column], x[,"asset_index_nowashnomat"], col="red")
  curveConcent(y[,column], y[,"asset_index_nowashnomat"], col="red",lty=2, add=TRUE)
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
decompSGini(vars_ses[,"water_imp"], z=vars_ses[,"country"], param=4)

decompSGini(x=vars_ses[,"water_imp"], z=vars_ses[,"country"], param=4)

summary(vars_ses[,"water_imp"])



curveConcent(vars_ses[,"water_imp"], y=vars_ses[,"asset_index_nowashnomat"], col="red")


##Plot individual curves for each country 
fish_ses_country <- fish_ses %>% group_by(country) %>% nest()

non_fish_ses_country <- non_fish_ses %>% group_by(country) %>% nest()

ineq_fun(fish_ses_country$data[1], non_fish_ses_country$data[1], "water_imp")


download.file("https://wfs.gc.cuny.edu/njohnson/www/BrankoData/LM_WPID_web.dta", 
              mode = "wb", destfile = "LM_WPID_web.dta")

wpid <- read.dta("LM_WPID_web.dta")

###Trying another method
#http://freerangestats.info/blog/2017/08/19/quantiles-gini


hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(cumulative=cumsum(water_imp)/sum(water_imp),
         ses_decile=ntile(asset_index_nowashnomat,100))%>%
  group_by(ses_decile) %>%
  select(ses_decile, cumulative, country, fishing_community) %>%
  slice(n()) %>%
  ggplot(aes(x=ses_decile, y=cumulative)) +
  geom_line() 






conc <- function(df, var) {
  var <- enquo(var)
  name <- enquo(name)
  name <- quo_name(name 
                   )
  df %>%
    arrange(asset_index_nowashnomat) %>%
    mutate(cumulative=cumsum(var)/sum(var),
           ses_decile=ntile(asset_index_nowashnomat,100))%>%
    group_by(ses_decile) %>%
    select(ses_decile, cumulative, country, fishing_community) %>%
    slice(n()) %>%
    ggplot(aes(x=ses_decile, y=cumulative)) +
    geom_line() 
}

test <- samp %>% conc(water_imp)




