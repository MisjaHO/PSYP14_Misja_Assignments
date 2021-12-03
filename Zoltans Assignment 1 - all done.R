# 2.2 Custom functions
# We will use these custom functions to get bootstrapped confidence intervals.
# function to obtain regression coefficients source:
# https://www.statmethods.net/advstats/bootstrapping.html
bs_to_boot <- function(model, data, indices) {
  d <- data[indices, ] # allows boot to select sample
  fit <- lm(formula(model), data = d)
  return(coef(fit))
}
# function to obtain adjusted R^2 source:
# https://www.statmethods.net/advstats/bootstrapping.html
# (partially modified)
adjR2_to_boot <- function(model, data, indices) {
  d <- data[indices, ] # allows boot to select sample
  fit <- lm(formula(model), data = d)
  return(summary(fit)$adj.r.squared)
}
# Computing the booststrap BCa (bias-corrected and
# accelerated) bootstrap confidence intervals by Elfron
# (1987) This is useful if there is bias or skew in the
# residuals.
confint.boot <- function(model, data = NULL, R = 1000) {
  if (is.null(data)) {
    data = eval(parse(text = as.character(model$call[3])))
  }
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)),
                                              ncol = 2))
  row.names(boot.ci_output_table) = names(coef(model))
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")
  2
  results.boot = results <- boot(data = data, statistic = bs_to_boot,
                                 R = 1000, model = model)
  for (i in 1:length(coef(model))) {
    boot.ci_output_table[i, ] = unlist(unlist(boot.ci(results.boot,
                                                      type = "bca", index = i))[c("bca4", "bca5")])
  }
  return(boot.ci_output_table)
}
# Computing the booststrapped confidence interval for a
# linear model using wild bottstrapping as descibed by Wu
# (1986) <doi:10.1214/aos/1176350142> requires the lmboot
# pakcage
wild.boot.confint <- function(model, data = NULL, B = 1000) {
  if (is.null(data)) {
    data = eval(parse(text = as.character(model$call[3])))
  }
  wild_boot_estimates = wild.boot(formula(model), data = data,
                                  B = B)
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,
                                                                     probs = c(0.025, 0.975))))
  return(result)
}

###Packages to load
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code

###Load and look at sample. Watch out for weird stuff. 

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)

summary(data_sample_1)

describe(data_sample_1)

###Correct the values which are not according to scale

corrected_data_sample_1 <- data_sample_1

corrected_data_sample_1 <- data_sample_1 %>%
  mutate(STAI_trait = replace(STAI_trait, STAI_trait == 4.2, NA))

corrected_data_sample_1 <- corrected_data_sample_1 %>%
  mutate(pain = replace(pain, pain == 55, NA))

corrected_data_sample_1 <- corrected_data_sample_1 %>% 
  filter(complete.cases(.))

####Gör om sex och ID till factor 
corrected_data_sample_1 = corrected_data_sample_1 %>%
  mutate(sex = factor(sex))

corrected_data_sample_1 = corrected_data_sample_1 %>%
  mutate(ID = factor(ID))

###Plot and see if everything seems ok for model1! 

corrected_data_sample_1 %>%
  select(pain) %>%
  ggplot() +
  aes(x = pain) +
  geom_bar()

corrected_data_sample_1 %>%
  select(sex) %>%
  ggplot() +
  aes(x = sex) +
  geom_bar()

corrected_data_sample_1 %>%
  select(age) %>%
  ggplot() +
  aes(x = age) +
  geom_bar()

corrected_data_sample_1 %>%
  ggplot() +
  aes(x = pain, fill = sex) +
  geom_bar(position = "dodge")

###Plots för predictors i model1   
corrected_data_sample_1 %>%
  ggplot() + 
  aes(x = age, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

corrected_data_sample_1 %>%
  select(sex, pain) %>%
  ggplot() +
  aes(x = sex, y = pain) +
  geom_point()

###Om en vill se plots bredvid varandra behöver de sparas ner.
grid.arrange(plot1, plot2, nrow = 1)

###Bygg model1  
model1 = lm(pain ~ age + sex, data = corrected_data_sample_1)

###Check model1 
#Cook's distance
model1 %>%
  plot(which = 4)

###See if the Cook's distance outliers makes sense and should be kept or not.
corrected_data_sample_1 %>%
  slice(c(8, 23, 46))

#Plots för alla predictors i model2
plot_STAI_trait = corrected_data_sample_1 %>%
  select(pain, STAI_trait) %>%
  ggplot() +
  aes(x = STAI_trait, y = pain) +
  geom_point() +
  geom_smooth(method = "lm")

plot_pain_cat = corrected_data_sample_1 %>%
  select(pain, pain_cat) %>%
  ggplot() +
  aes(x = pain_cat, y = pain) +
  geom_point() +
  geom_smooth(method = "lm")

plot_cortisol_serum = corrected_data_sample_1 %>%
  select(pain, cortisol_serum) %>%
  ggplot() +
  aes(x = cortisol_serum, y = pain) +
  geom_point() +
  geom_smooth(method = "lm")

plot_cortisol_saliva = corrected_data_sample_1 %>%
  select(pain, cortisol_saliva) %>%
  ggplot() +
  aes(x = cortisol_saliva, y = pain) +
  geom_point() +
  geom_smooth(method = "lm")   

plot_mindfulness = corrected_data_sample_1 %>%
  select(pain, mindfulness) %>%
  ggplot() +
  aes(x = mindfulness, y = pain) +
  geom_point() +
  geom_smooth(method = "lm")

###Check the variables for coding errors

###Bygg model2 
model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = corrected_data_sample_1)

###Check model for outliers using graphs (also getting the case number)

corrected_data_sample_1 %>%
  mutate(rownum = row.names(corrected_data_sample_1)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_label() +
  geom_smooth(method = "lm")

corrected_data_sample_1 %>%
  mutate(rownum = row.names(corrected_data_sample_1)) %>%
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) +
  geom_label() +
  geom_smooth(method = "lm")

corrected_data_sample_1 %>%
  mutate(rownum = row.names(corrected_data_sample_1)) %>%
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) +
  geom_label() +
  geom_smooth(method = "lm")

corrected_data_sample_1 %>%
  mutate(rownum = row.names(corrected_data_sample_1)) %>%
  ggplot() + aes(x = cortisol_saliva, y = pain, label = rownum) +
  geom_label() +
  geom_smooth(method = "lm")

corrected_data_sample_1 %>%
  mutate(rownum = row.names(corrected_data_sample_1)) %>%
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) +
  geom_label() +
  geom_smooth(method = "lm")

####Check model2

###Cook's distance
model2 %>%
  plot(which = 4)

#See if the Cook's distance outliers makes sense and should be kept or not.
corrected_data_sample_1 %>%
  slice(c(46, 73, 85))

#Outliers can be kept.

###normality (of the residuals)
model2 %>%
  plot(which = 2)

residuals_model2 = enframe(residuals(model2))

residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

# Skew -0.15 and kurtosis -0.03 = no violation of the assumption of normality. 

###linearity (of the relationship) 
model2 %>%
  residualPlots()

#Assumption of linearity is not violated, no p-value is significant and the lines are roughly flat. 


###homogeneity of variance (also called homoscedasticity) 
model2 %>% 
  plot(which = 3)

model2 %>%
  ncvTest()

model2 %>%
  bptest()
  
#Assumption of homoscedasticity is not violated and therefore no action is required.ncvTest = 0.87367.


###Multicollinearity ("uncorrelated predictors" in Navarro's words)
model2 %>%
  vif()
#The VIF-test showed multicollinearity between cortisol_saliva (5.07) and cortisol_serum (4.787100) (data multicollinearity)
#This is not weird because the two predictors are just two types of ways to evaluate the same thing (cortisol)

###Build new model2 with only one of the cortisol measures, keep cortisol_serum because of the higher correlation to stress
model2_serum = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = corrected_data_sample_1)

####Re-run all the tests
###Cook's distance
model2_serum %>%
  plot(which = 4)

#See if the Cook's distance outliers makes sense and should be kept or not.
corrected_data_sample_1 %>%
  slice(c(46, 64, 85))

#Outliers can be kept.

###normality (of the residuals)
model2_serum %>%
  plot(which = 2)

residuals_model2_serum = enframe(residuals(model2_serum))

residuals_model2_serum %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2_serum))

# Skew -0.18 and kurtosis 0.02 = no violation of the assumption of normality. 

###linearity (of the relationship) 
model2_serum %>%
  residualPlots()

#Assumption of linearity is not violated, no p-value is significant and the lines are roughly flat. 

###homogeneity of variance (also called homoscedasticity) 
model2_serum %>% 
  plot(which = 3)

model2_serum %>%
  ncvTest()

model2_serum %>%
  bptest()

#Assumption of homoscedasticity is not violated and therefore no action is required.ncvTest = 0.79918.

###Multicollinearity ("uncorrelated predictors" in Navarro's words)
model2_serum %>%
  vif()

#The VIF-test showed no multicollinearity between variables 

###Alas all the tests came back without any problems and the new only_serum_model2 can therefor be used as the comparing model. 






###model1 tests
###Cook's distance
model1 %>%
  plot(which = 4)

#See if the Cook's distance outliers makes sense and should be kept or not.
corrected_data_sample_1 %>%
  slice(c(8, 23, 46))

#Outliers can be kept.

###normality (of the residuals)
model1 %>%
  plot(which = 2)

residuals_model1 = enframe(residuals(model1))

residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model1))

# Skew 0.1 and kurtosis -0.02 = no violation of the assumption of normality. 

###linearity (of the relationship) 
model1 %>%
  residualPlots()

#Assumption of linearity is not violated, no p-value is significant and the lines are roughly flat. 

###homogeneity of variance (also called homoscedasticity) 
model1 %>% 
  plot(which = 3)

model1 %>%
  ncvTest()

model1 %>%
  bptest()

#Assumption of homoscedasticity is not violated and therefore no action is required.ncvTest = 0.10334.

###Multicollinearity ("uncorrelated predictors" in Navarro's words)
model1 %>%
  vif()

#The VIF-test showed no multicollinearity between variables, but the value between age and sex was exactly the same (1.004713). 

#####Compare models (model1 - model2_serum)
summary(model1)

summary(model2_serum)

###How much variance is explained by the different models (model1 and model2_serum)  
summary(model1)$adj.r.squared

#r.squared value for model1 = 0.07355033

summary(model2_serum)$adj.r.squared

#r.squared value for model2_serum = 0.504055

#The variance explained has increased drastically between model1 and model2_serum

###compare model fit 
AIC(model1)

#model fit model1 = 574.1267

AIC(model2_serum)

#model fit model2_serum = 479.2624

###compare residual error 
anova(model1, model2_serum)

#model2_serum has a better model fit and was also highly significant in the anova-test, therefore model2_serum is the better model of the two. 