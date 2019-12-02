#Concentration curve

##Water imp and ses 

library(scales)
water_imp_ses <- hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
           select(water_imp,water_piped,san_imp,housing_imp, ses_percent)

library(IC2)

ineq_fun1 <- function(x,column) {
  curveConcent(x[,column], x[,"ses_percent"], col="red")
  curveConcent(x[,column], x[,"ses_percent"], col="red",lty=2, add=TRUE)
  curveConcent(x[,column], x[,"ses_percent"], col="red",lty=2, add=TRUE)
  curveConcent(x[,column], x[,"ses_percent"], col="red",lty=2, add=TRUE)
  curveConcent(x[,column], x[,"ses_percent"], col="red",lty=2, add=TRUE)
  title(main=paste(column))
  legend("topleft", legend = c("Fishing community", "Non-Fishing community"),col=c("red", "red"), lty=c(1,2))
}



curveConcent(water_imp_ses[,"water_imp"], y=water_imp_ses[,"ses_percent"], col="red")
curveConcent(water_imp_ses[,"water_piped"], y=water_imp_ses[,"ses_percent"], col="blue", add=TRUE)
curveConcent(water_imp_ses[,"san_imp"], y=water_imp_ses[,"ses_percent"], col="green", add=TRUE)
curveConcent(water_imp_ses[,"housing_imp"], y=water_imp_ses[,"ses_percent"], col="orange", add=TRUE)
title(main="Overall access to household facilities")
legend("topleft", legend = c("Improved water source", "Piped water source", "Improved sanitation", "Improved housing"), 
       col=c("red", "blue", "green", "orange"), lty=rep(1, 4))


fish_ses <- hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
  #mutate(ses_scale=rescale(asset_index_nowashnomat, to=c(0,1))) %>%
  filter(fish_factor==1) %>%
  select(water_imp,water_piped,san_imp,housing_imp, ses_percent)

non_fish_ses <- hr_fish_gps %>%
  arrange(asset_index_nowashnomat) %>%
  mutate(ses_percent = ntile(asset_index_nowashnomat,100)) %>%
  filter(fish_factor!=1) %>%
  select(water_imp,water_piped,san_imp,housing_imp, ses_percent)


ineq_fun2 <- function(x,y, column) {
  curveConcent(x[,column], x[,"ses_percent"], col="red")
  curveConcent(y[,column], y=y[,"ses_percent"], col="red",lty=2, add=TRUE)
  title(main=paste(column))
  legend("topleft", legend = c("Fishing community", "Non-Fishing community"),col=c("red", "red"), lty=c(1,2))
}

list <- c("water_imp", "water_piped", "san_imp", "housing_imp")

ineq_fun2(fish_ses, non_fish_ses, "water_imp")
ineq_fun2(fish_ses, non_fish_ses, "water_piped")
ineq_fun2(fish_ses, non_fish_ses, "san_imp")
ineq_fun2(fish_ses, non_fish_ses, "housing_imp")



#Improved water source
p1 <- recordPlot()
curveConcent(fish_ses[,"water_imp"], y=fish_ses[,"ses_percent"], col="red")
curveConcent(non_fish_ses[,"water_imp"], y=non_fish_ses[,"ses_percent"], col="red",lty=2, add=TRUE)
title(main="Access to improved water source") 
legend("topleft", legend = c("Fishing community", "Non-Fishing community"),col=c("red", "red"), lty=c(1,2))
replayPlot(p1)

#Piped water source
curveConcent(fish_ses[,"water_piped"], y=fish_ses[,"ses_percent"], col="red")
curveConcent(non_fish_ses[,"water_piped"], y=non_fish_ses[,"ses_percent"], col="red",lty=2, add=TRUE)
title(main="Access to piped water")
legend("topleft", legend = c("Fishing community", "Non-Fishing community"), col=c("red", "red"), lty=c(1,2))

#Improved sanitation
curveConcent(fish_ses[,"san_imp"], y=fish_ses[,"ses_percent"], col="red")
curveConcent(non_fish_ses[,"san_imp"], y=non_fish_ses[,"ses_percent"], col="red",lty=2, add=TRUE)
title(main="Access to improved sanitation")
legend("topleft", legend = c("Fishing community", "Non-Fishing community"), col=c("red", "red"), lty=c(1,2))

#Improved housing
curveConcent(fish_ses[,"housing_imp"], y=fish_ses[,"ses_percent"], col="red")
curveConcent(non_fish_ses[,"housing_imp"], y=non_fish_ses[,"ses_percent"], col="red",lty=2, add=TRUE)
title(main="Access to improved housing")
legend("topleft", legend = c("Fishing community", "Non-Fishing community"),  col=c("red", "red"), lty=c(1,2))


##There seems to be a large degree of heterogeneity between the countries
##Plot individual curves for each country 







