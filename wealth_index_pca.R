



# *New wealth index without WASH INDICATORS 
# *First construct with WASH indicators 
# *Then remove WASH indicators
# gen weight=hv005/100000
# 
# *Assets owned by a household or a member of the household
# local vars1 "hv206 hv207 hv208 hv243a hv221 hv209 sh110l sh110i sh110j sh110n sh110v hv243b sh118g hv210 hv211 hv243c hv212 hv243d hv244"
# local vars2 "electricity radio television mobile landline fridge dvd washer cooker computer internet watch tractor bicycle motorcycle cart car boat land"
# local n: word count `vars1'
# forvalues i = 1/`n' {
#   local v1 : word `i' of `vars1'
#   local v2 : word `i' of `vars2'
#   gen `v2'=`v1'
#   recode `v2' (9=0)
#   recode `v2' (.=0)
#   quietly tab `v1' `v2'
#   }
#   
#   //Animal assets
#   egen cattle = rowfirst(hv246a hv246g sh122a sh122b sh122c)
#   //Tanzania 5 has no information on cattle

orig_names <- c("hv206", "hv207", "hv208", "hv243a", "hv221", "hv209", "sh110l",
                "sh110i", "sh110j", "sh110n", "sh110v", "hv243b", "sh118g", "hv210", "hv211", "hv243c", "hv212", "hv243d", "hv244")

new_names <- c("electricity", "radio", "television", "mobile", "landline", "fridge", "dvd", 
               "washer", "cooker", "computer", "internet", "watch", "tractor", "bicycle", "motorcycle", "cart", "car", "boat", "land")

setnames(hr_fish_gps, orig_names, new_names)

f1 <- function(x) {
  ifelse(grepl("yes",x),1,0)
}

test <- hr_fish_gps %>%
  select(new_names) %>%
  mutate_all(tolower) %>%
  mutate_all(f1)


  #mutate(electricity=tolower(electricity)) %>%
  mutate(electricity = ifelse(grepl("yes", electricity),1,0))
  


sapply(new_names,  function(x) {
  hr_fish_gps[[, x] <- ifelse(grepl("yes", hr_fish_gps[, x]), 1,0)]
})

hr_fish_gps$electricity <- tolower(hr_fish_gps$electricity)
hr_fish_gps[, electricity := ifelse(grepl("yes",electricity), 1, 0)]

for (i in length(colnames){
  print(i)
})

hr_fish_gps$electricity <- ifelse(grepl("yes", hr_fish_gps$electricity),1,0)

temp_fn <- function(input) ifelse(test = hr_fish_gps[["x"]],
                                  yes = "input",
                                  no = NA)
test <- hr_fish_gps %>%
  mutate_at(.vars = vars(new_names),
            .funs = funs(ifelse("yes", ., NA)))


hr_fish_gps %>% mutate_at(vars(new_names), funs(ifelse(grepl("yes", ., NA)))


test <- hr_fish_gps %>%
  mutate_at(.vars = vars(new_names),
            .funs= ())


test <- hr_fish_gps %>%
  lapply(new_names, function(x){
  hr_fish_gps[[x]] <- tolower(hr_fish_gps[[x]])
  hr_fish_gps[x] <- ifelse(grepl("yes",hr_fish_gps[x]),1,0)
})


test <- lapply(new_names, function(x){
  hr_fish_gps[, x:= ifelse(grepl("yes", x),1,0)]
})









