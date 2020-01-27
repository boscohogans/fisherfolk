#Generating a wealth index using PCA
#https://rpubs.com/Sternonyos/526030

#Original variable names
orig_names <- c("hv206", "hv207", "hv208", "hv243a", "hv221", "hv209", "sh110l",
                "sh110i", "sh110j", "sh110n", "sh110v", "hv243b", "sh118g", "hv210", "hv211", "hv243c", "hv212", "hv243d", "hv244",
                "cattle", "hv246d", "hv246e", "hv246f", "hv246c")
#New names
pca_variables <- c("electricity", "radio", "television", "mobile", "landline", "fridge", "dvd", 
               "washer", "cooker", "computer", "internet", "watch", "tractor", "bicycle", "motorcycle", "cart", "car", "boat", "land",
               "cattle_cat", "goat_cat", "sheep_cat", "chicken_cat", "horse_cat")
#Id variables
id_var <- c("hv000", "hv001", "hv002")
#new list with all variables + ID
pca_variables_id <- append(pca_variables, id_var)

#Define functions
f1 <- function(x) {
  ifelse(grepl("yes",tolower(x)), 1,0)
}

#Seperate function for livestock
f2 <- function(x) {
  ifelse(grepl("none", x), 0, 
         ifelse(x %in% NA,0,
                ifelse(grepl("unknown", x),0,1)))
}
#new livestock categories
livestock <- c("cattle_cat", "goat_cat", "sheep_cat", "chicken_cat", "horse_cat")
#seperate assets
assets <- setdiff(new_names, livestock)

pca_data <- hr_fish_gps %>%
  #Coalesce values into one cattle variable
  mutate(cattle=coalesce(hv246a, hv246g, sh122a, sh122b, sh122c)) %>% 
  #Change  names to new names
  setnames(., orig_names, pca_variables) %>% 
  #Apply function 1 to assets
  mutate_at(assets, f1) %>%
  #Apply function 2 to livestock
  mutate_at(livestock, f2) %>% 
  #Select variables for PCA
  select(pca_variables_id) 

#Need to loop through each data frame with custom list of pca_colnames
pca_results <- final_pca %>% 
  #Split by survey
  split(.$hv000) %>%
  #Keep columns that don't sum to zero
  map(~.[,which(colSums(.[1:24])!=0)]) %>% 
  #Run PCA analysis
  map(~psych::principal(.,
    rotate = "varimax",
    nfactors = 3,
    covar = T,
    scores = TRUE))

#Extract scores from pca_results
index <- lapply(pca_results, `[`,'scores')
#Convert scores to data frame
index_df = do.call("rbind", lapply(index, "[[", 1))
index_df_1 <- index_df[,1]
#Label for quintiles
nlab<-c(1,2,3,4,5)
#Add column to raw data
pca_data3 <- mutate(hr_fish_gps,
                 rawscore=index_df_1,
                 quintile=as.factor(cut(rawscore,breaks=5,labels=nlab)))

test <- hr_fish_gps %>% 
  left_join(., pca_data3, by=id_var)

library(corrplot)

m <- cor(test$hv271.x, test$asset_index_nowashnomat.x)
corrplot(m, method = "circle")

qplot(hv271.x,asset_index_nowashnomat.x,
      data=test,
      geom=c("point", "smooth"),
      method="lm")


newdata<-mutate(df2,
                raw_score=index,
                quintile=as.factor(cut(index,breaks=5,labels=nlab)))

library(ggplot2)
ggplot(newdata, aes(survey)) + geom_bar(aes(fill = quintile), position = "fill",width = 0.4) +
  xlab("Survey") +
  ylab("Percentage") +
  ggtitle("Wealth Breakdown")


