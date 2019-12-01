

#https://rpubs.com/Sternonyos/526030

#Original variable names
orig_names <- c("hv206", "hv207", "hv208", "hv243a", "hv221", "hv209", "sh110l",
                "sh110i", "sh110j", "sh110n", "sh110v", "hv243b", "sh118g", "hv210", "hv211", "hv243c", "hv212", "hv243d", "hv244")
#New names
new_names <- c("electricity", "radio", "television", "mobile", "landline", "fridge", "dvd", 
               "washer", "cooker", "computer", "internet", "watch", "tractor", "bicycle", "motorcycle", "cart", "car", "boat", "land")

setnames(hr_fish_gps, orig_names, new_names)

f1 <- function(x) {
  tolower(x)
  ifelse(grepl("yes",x),1,0)
}
pca_data <- hr_fish_gps %>%
  mutate_at(new_names, f1) %>%
  mutate(cattle=coalesce(hv246a, hv246g, sh122a, sh122b, sh122c))

##Animal assets
#Tanzania 5 has a lot of missing information for animal variables

# orig_livestock <- c("cattle", "hv246d", "hv246e", "hv246f", "hv246c")
# new_livestock <- c("cattle_cat", "goat_cat", "sheep_cat","chicken_cat", "horse_cat")
# 
# f2 <- function(x, y) {
#  y <- ifelse(grepl("none", x), 0, 
#          ifelse(x %in% NA,0,
#                 ifelse(grepl("unknown", x),0,1)))
# }
# 
# 
# test <- pca_data %>%
#   mutate_at(orig_livestock, f2)
#   
# test <- lapply(X = orig_livestock, FUN=f2, y=new_livestock)

setDT(pca_data)
pca_data[, cattle_cat := ifelse(grepl("none", cattle), 0, 
                                ifelse(cattle %in% NA,0,
                                ifelse(grepl("unknown", cattle),0,1)))]

pca_data[, goat_cat := ifelse(grepl("none", hv246d), 0,
                              ifelse(hv246d %in% NA,0,
                                     ifelse(grepl("unknown", hv246d),0,1)))]

pca_data[, sheep_cat := ifelse(grepl("none", hv246e), 0,
                               ifelse(hv246e %in% NA,0,
                                      ifelse(grepl("unknown", hv246e),0,1)))]


pca_data[, chicken_cat := ifelse(grepl("none", hv246f), 0,
                                 ifelse(hv246f %in% NA,0,
                                        ifelse(grepl("unknown", hv246f),0,1)))]

pca_data[, horse_cat := ifelse(grepl("none", hv246c), 0,
                                 ifelse(hv246c %in% NA,0,
                                        ifelse(grepl("unknown", hv246c),0,1)))]


library(kableExtra)

livestock_cat <- c("cattle_cat", "goat_cat", "sheep_cat", "chicken_cat", "horse_cat")

pca_variables <- c(new_names, livestock_cat)

head(pca_data,n=20)%>%
  kable("html") %>%
  kable_styling(font_size=9) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

final_pca <- data.frame(pca_data)
library(psych)
prn<-psych::principal(final_pca[pca_variables], rotate="varimax", nfactors=3,covar=T, scores=TRUE)
index <- prn$scores[,1]

nlab<-c(1,2,3,4,5)
newdata<-mutate(final_pca,quintile=as.factor(cut(index,breaks=5,labels=nlab)))

library(ggplot2)
ggplot(newdata, aes(country)) + geom_bar(aes(fill = quintile), position = "fill",width = 0.4) +
  xlab("County") +
  ylab("Percentage") +
  ggtitle("Wealth Breakdown")


