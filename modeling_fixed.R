full_data <- readRDS("data/full_data.rds")

full_data$race <- factor(full_data$race,levels=c("White","Black","Hispanic","Other"))
full_data$black <- ifelse(full_data$race=="Black",1,0)

full_data$examiner_id_factor <- relevel(factor(full_data$examiner_id,levels=as.character(c(3007,c(3001:3012)[-7]))),ref="3007")


# Variable selection ------------------------------------------------------

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



# Use selected variables, check with and without examiner ID --------------
# Even though the LASSO procedure suggests that we keep it
# Use variables from the UNWEIGHTED version

health_model <- glm(health_status2 ~
                      race + gender + age + region +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin),
                    data=full_data,
                    family = binomial(link = "logit"))

# Version limited to those that have examiner IDs
health_model_lim <- glm(health_status2 ~
                      race + gender + age + region +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin),
                    data=subset(full_data,!is.na(examiner_id_factor)),
                    family = binomial(link = "logit"))

health_model_examiner <- glm(health_status2 ~
                      race + gender + age + region +
                      scale(systolic_bp) + scale(diastolic_bp) +
                      locomotion + paralysis + impairment + infection +
                      scale(bmi) + scale(height) +
                      scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                      examiner_id_factor,
                    data=full_data,
                    family = binomial(link = "logit"))

health_model_examiner_gender <- update(health_model_examiner,.~.+examiner_id_factor:gender)
health_model_examiner_black <- update(health_model_examiner,.~.+examiner_id_factor:black)
health_model_examiner_blackgender <- update(health_model_examiner,.~.+examiner_id_factor:gender:black)

AIC(health_model,health_model_lim,health_model_examiner)
# Version with only limited to avoid the warning
AIC(health_model_lim,health_model_examiner)

AIC(health_model_examiner,health_model_examiner_gender,health_model_examiner_black,health_model_examiner_blackgender)

# LRT
(health_model_lim$deviance - health_model_examiner$deviance)>qchisq(.95,11)
# This is greater, so we conclude that we should keep the examiner IDs

(health_model_examiner$deviance - health_model_examiner_gender$deviance)>qchisq(.95,11)
# This is greater, so we conclude that we should keep the examiner IDs

summary(health_model_examiner_gender)


# Diagnostics -------------------------------------------------------------

outcomes <- model.frame(health_model_examiner)
outcomes$predicted<-ifelse(predict(health_model_examiner,type="response")>.5,1,0)

# Accuracy
mean(outcomes$health_status2 == outcomes$predicted)

# But what we really care about is accurately identifying those who were ranked below excellent
with(subset(outcomes,health_status2!= 1),{
  mean(health_status2 == predicted)
})

with(subset(outcomes,health_status2 == 1),{
  mean(health_status2 == predicted)
})

# Actually somewhat better at predicting true negatives than true positives

# Pseudo R2
# Not great, but not terrible either
# http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
1-health_model_examiner$deviance/health_model_examiner$null.deviance

# Alternative formulation from Taylor's class
# Very similar results
1-exp((health_model_examiner$deviance-health_model_examiner$null.deviance)/dim(model.frame(health_model_examiner))[1])

# Consider R2 of no examiner
1-health_model_lim$deviance/health_model_lim$null.deviance

# A significant improvement!

# Try ROC