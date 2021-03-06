
# Data tweaks -------------------------------------------------------------
full_data <- readRDS("data/full_data.rds")

full_data$race <- factor(full_data$race,levels=c("White","Black","Hispanic","Other"))
full_data$black <- ifelse(full_data$race=="Black",1,0)
full_data$west <- ifelse(full_data$region=="West",1,0)
full_data$examiner_id_factor <- relevel(factor(full_data$examiner_id,levels=as.character(c(3007,c(3001:3012)[-7]))),ref="3007")


# Variable selection ------------------------------------------------------
# Use LASSO to penalize the use of variables with less important contributions

library(glmnet)
library(glmnetUtils)

health_lasso <- cv.glmnet(health_status2 ~
                            race+ gender + age + region + time_of_day +
                            scale(systolic_bp) + scale(diastolic_bp) +
                            locomotion + paralysis + impairment + infection +
                            scale(bmi) + scale(weight) + scale(height)+ 
                            scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                            examiner_id_factor,
                          data=full_data,
                          family= binomial(link = "logit"))

# Test with observation weights
health_lasso_weighted <- cv.glmnet(health_status2 ~
                                    race+ gender + age + region + time_of_day +
                                    scale(systolic_bp) + scale(diastolic_bp) +
                                    locomotion + paralysis + impairment + infection +
                                    scale(bmi) + scale(weight) + scale(height)+ 
                                    scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                    examiner_id_factor,
                                  weights = WTPFHX6,
                                  data=full_data,
                                  family= binomial(link = "logit"))

# Test with weights as a predictor instead
health_lasso_weightaspredictor <- cv.glmnet(health_status2 ~
                                     scale(WTPFHX6) +
                                     race+ gender + age + region + time_of_day +
                                     scale(systolic_bp) + scale(diastolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     scale(bmi) + scale(weight) + scale(height)+ 
                                     scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     examiner_id_factor,
                                   data=full_data,
                                   family= binomial(link = "logit"))

coef(health_lasso,s="lambda.1se")
coef(health_lasso_weighted,s="lambda.1se")
coef(health_lasso_weightaspredictor,s="lambda.1se")

# Use selected variables, check with and without examiner ID --------------
# Even though the LASSO procedure suggests that we keep it
# Use variables from the UNWEIGHTED version

# Reductions based on LASSO:
# Use BLACK instead of RACE
# Use WEST instead of REGION

# Base model - using the selected variables but NO examiner effect
health_model <- glm(health_status2 ~
                      black + gender + age + west +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin),
                    data=full_data,
                    family = binomial(link = "logit"))

# Base model limited to those that have examiner IDs
health_model_lim <- glm(health_status2 ~
                      black + gender + age + west +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin),
                    data=subset(full_data,!is.na(examiner_id_factor)),
                    family = binomial(link = "logit"))

# Base model WITH examiner
health_model_examiner <- glm(health_status2 ~
                      black + gender + age + west +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                      examiner_id_factor,
                    data=full_data,
                    family = binomial(link = "logit"))

# Double-check whether weights would be useful as predictors
health_model_examiner_weightaspredictor <- glm(health_status2 ~
                                                 scale(WTPFHX6) +
                                                 black + gender + age + west +
                                                 scale(systolic_bp) + scale(diastolic_bp) +
                                                 locomotion + paralysis + impairment + infection +
                                                 scale(bmi) + scale(height) +
                                                 scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                                 examiner_id_factor,
                                               data=full_data,
                                               family = binomial(link = "logit"))

AIC(health_model,health_model_lim,health_model_examiner,health_model_examiner_weightaspredictor)
BIC(health_model,health_model_lim,health_model_examiner,health_model_examiner_weightaspredictor)
# The weight doesn't make much difference as a linear predictor
# Note that warning is tripped by the health_model (health_model_lim and all others have same n)


# Interactions ------------------------------------------------------------
health_model_examiner_intgender <- update(health_model_examiner,.~.+examiner_id_factor:gender)
health_model_examiner_intblack <- update(health_model_examiner,.~.+examiner_id_factor:black)
health_model_examiner_intblackgender <- update(health_model_examiner,.~.+examiner_id_factor:gender:black)

AIC(health_model_examiner,health_model_examiner_intgender,health_model_examiner_intblack,health_model_examiner_intblackgender)
BIC(health_model_examiner,health_model_examiner_intgender,health_model_examiner_intblack,health_model_examiner_intblackgender)
# Gender interaction seems to be making the most difference

# Likelihood ratio tests
(health_model_lim$deviance - health_model_examiner$deviance)>qchisq(.95,11)
# This is greater, so we conclude that we should keep the examiner IDs

(health_model_examiner$deviance - health_model_examiner_intgender$deviance)>qchisq(.95,11)
# This is greater, so we conclude that we should keep the gender interaction

(health_model_examiner_intgender$deviance - health_model_examiner_intblackgender$deviance) > qchisq(.95,12)
# This is greater, but really not by much

summary(health_model_examiner_intgender)


# Diagnostics for health_model_examiner_intgender  -------------------------------------------------------------

library(tidyverse)

outcomes <- bind_cols(model.frame(health_model_examiner_intgender), 
                      predicted=ifelse(predict(health_model_examiner_intgender,type="response")>.5,1,0))

outcome_table <- outcomes %>% count(health_status2,predicted) %>% rename(Actual = health_status2, Predicted = predicted)

# Accuracy
outcome_table %>% group_by(Actual == Predicted) %>% summarize(n=sum(n)) %>% mutate(n = n/sum(n))

# Sensitivity
outcome_table %>% group_by(Actual) %>% summarize(Predicted= Predicted,n = n/sum(n)) %>% 
  bind_cols(desc = c("True negative rate", "False positive rate","False negative rate","True positive rate"))

# Pseudo R2
# Not great, but not terrible either
# http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
1-health_model_examiner_intgender$deviance/health_model_examiner$null.deviance

# Alternative formulation from Taylor's class
# Very similar results
1-exp((health_model_examiner_intgender$deviance-health_model_examiner_intgender$null.deviance)/dim(model.frame(health_model_examiner_intgender))[1])

# Consider R2 of no examiner
1-health_model_lim$deviance/health_model_lim$null.deviance