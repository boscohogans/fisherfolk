
hr_intercept <- "(1|clusters)"

library(afex)
formula <-
  as.formula(paste("less_than_5", "~", fixed , "+", hr_intercept))
print(formula)
less5_model <-
  glmer(formula,
        data = hr_select_scale,
        family = 'binomial')

test <- glmer(less_than_5 ~ fish_factor + asset_index_nowashnomat +
        (1 | clusters),
      data= hr_select_scale,
      family="binomial",
      nAGQ = 0 )

aa <- allFit(less5_model, verbose=TRUE)


lapply(setNames(hr_outcomes, hr_outcomes), function(var) {
  fixed <- paste0(unlist(all_hr_vars), collapse= " + ")
  formula <-
    as.formula(paste(var, "~", fixed , "+", hr_intercept))
  print(formula)
  mod <-
    glmer(formula,
          data = hr_select_scale
          ,family = 'binomial'
          ,control = glmerControl(optimizer ="Nelder_Mead"))
  #,control=glmerControl(optimizer="bobyqa"))
})

glmerwrap <-
  function(formula) { 
    cl <- origCall <- match.call()
    cl[[1L]] <- as.name("glmer") # replace 'lmerwrap' with 'glmer'
    # replace "re" with "|" in the formula:
    f <- as.formula(do.call("substitute", list(formula, list(re = as.name("|")))))
    environment(f) <- environment(formula)
    cl$formula <- f
    x <- eval.parent(cl) # evaluate modified call
    # store original call and formula in the result:
    x@call  <- origCall
    attr(x@frame, "formula") <- formula
    x
  }
formals(glmerwrap) <- formals(lme4::glmer)


options(na.action = na.fail)
(fm <- glmerwrap(cbind(diarrhea) ~ fish_factor +  num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac +
                   re(1, clusters) +  re(1, hh_num_str), family = binomial, data = kr_analysis_dataset))
options(na.action = na.omit)
x <- dredge(fm, fixed="fish_factor")


options(na.action = na.fail)
  cc <- kr_analysis_dataset %>% tidyr::drop_na(diarrhea)
  #mod <- glmer(formula, cc, family = 'binomial'
   #            ,control=glmerControl(optimizer="bobyqa"))
  (fm <- glmerwrap(cbind(diarrhea) ~ fish_factor + + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac +
                     re(1, clusters) +  re(1, hh_num_str), family = binomial, data = cc))
 x<- dredge(fm, fixed = "fish_factor")
options(na.action = na.omit)


options(na.action = na.fail)
cc <- kr_analysis_dataset %>% tidyr::drop_na(diarrhea)
#mod <- glmer(formula, cc, family = 'binomial'
#            ,control=glmerControl(optimizer="bobyqa"))
(fm2 <- glmer(diarrhea ~ fish_factor + + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac +
                   (1 | clusters) +  (1 |hh_num_str), family = binomial, data = cc))
x2<- dredge(fm, fixed = "fish_factor")
options(na.action = na.omit)


library(afex)
aa <- allFit(fm2, verbose=TRUE)





options(na.action = na.fail)
kr_models <- lapply(1:4, function(i) {
  kr_fixed <- paste0(unlist(all_kr_vars), collapse= " + ")
  formula <-
    as.formula(paste(kr_outcomes[i], "~", kr_fixed, "+", kr_levels))
  print(formula)
  cc <- kr_analysis_dataset %>% tidyr::drop_na(kr_outcomes[i])
  #mod <- glmer(formula, cc, family = 'binomial'
   #            ,control=glmerControl(optimizer="bobyqa"))
  mod <- glmerwrap(cbind())
  #dredge(mod, fixed = "fish_factor")
})
options(na.action = na.omit)


kr_vars <- ("fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac")

kr_analysis_dataset$num_hh_cat <- cut(kr_analysis_dataset$num_hh_members , 
                      breaks = c(-Inf,3,5,7,Inf) ,
                      labels = c("1-3","3-5","5-7","7+"),
                      right = FALSE)

#checking lgostic regression
model <- glm(diarrhea ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac, data= cc)
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)


mydata <- cc %>%
  dplyr::select_if(is.numeric)

predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(model, which = 4, id.n = 3)
model.data <- broom::augment(model) %>% 
  mutate(index = 1:n())

model.data %>% top_n(3, .cooksd)






diarrhea_2 <- glmer(diarrhea ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac + 
                      (1|clusters), data=kr_analysis_dataset, family='binomial',control=glmerControl(optimizer="bobyqa"))
diarrhea_3 <- glmer(diarrhea ~ fish_factor + num_hh_members + num_childrenunder5 + quintile_nowashnomat_fac + medu_fac + 
                      (1|clusters) + (1|hh_num_str), data=kr_analysis_dataset, family='binomial',control=glmerControl(optimizer="bobyqa"))

library(performance)
performance(diarrhea_1)
performance(diarrhea_2)
performance(diarrhea_3)





