#########Uppgift 2

###Dessa ska användas i backward regression: age, sex, STAI, pain catastrophizing, mindfulness, serum cortisol, weight, IQ, household income

Backward_regression_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + household_income + IQ, data = corrected_data_sample_1)

####Check Backward_regression_model 

###Cook's distance
Backward_regression_model %>%
  plot(which = 4)

#See if the Cook's distance outliers makes sense and should be kept or not.
corrected_data_sample_1 %>%
  slice(c(46, 84, 85))

#Outliers can be kept.

###normality (of the residuals)
Backward_regression_model %>%
  plot(which = 2)

residuals_Backward_regression_model = enframe(residuals(Backward_regression_model))

residuals_Backward_regression_model %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(Backward_regression_model))

# Skew -0.17 and kurtosis -0.08 = no violation of the assumption of normality. 

###linearity (of the relationship) 
Backward_regression_model %>%
  residualPlots()

#Assumption of linearity is not violated, no p-value is significant and the lines are roughly flat. 


###homogeneity of variance (also called homoscedasticity) 
Backward_regression_model %>% 
  plot(which = 3)

Backward_regression_model %>%
  ncvTest()

Backward_regression_model %>%
  bptest()

#Assumption of homoscedasticity is not violated and therefore no action is required.ncvTest = 0.86793.


###Multicollinearity ("uncorrelated predictors" in Navarro's words)
Backward_regression_model %>%
  vif()
#The VIF-test showed no multicollinearity and therefore the model passed all the tests and is ready for backward regression.

summary(Backward_regression_model)

step(Backward_regression_model, direction = "backward")

Best_model = lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = corrected_data_sample_1)

model2_serum = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = corrected_data_sample_1)

###compare model fit 
AIC(Best_model)

#model fit "Best_model" = 476.3015

AIC(model2_serum)

#model fit model2_serum = 479.2624

###compare residual error 
anova(Best_model, model2_serum)

#Best_model has a little bit better model fit 

###Import data sample to use both models on
data_sample_2 = read.csv("https://tinyurl.com/87v6emky")

view(data_sample_2)

summary(data_sample_2)

describe(data_sample_2)

###Make sex and ID into factors in data_sample_2 
data_sample_2 = data_sample_2 %>%
  mutate(sex = factor(sex))

data_sample_2 = data_sample_2 %>%
  mutate(ID = factor(ID))

### Predict values using the different models and the new data_sample_2 

predict(Best_model, newdata = data_sample_2)

###Orginal values in data_sample_2
###1        2       3         4       5         6         7       8       9       10
###5        2       6         4       7         6         4       3       5       4

###Values predicted by Best_model
###1        2        3        4        5        6        7        8       
###5.571869 4.506871 4.941226 4.919784 6.863674 6.329305 4.654078 3.748784

predict(model2_serum, newdata = data_sample_2)

###Values predicted by model2_serum
###1        2        3        4        5        6        7        8       
###5.550155 4.434391 5.013655 5.005085 6.814066 6.437391 4.873057 3.859598