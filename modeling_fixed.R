# library(lme4)

full_data <- readRDS("data/full_data.rds")

full_data$race <- factor(full_data$race,levels=c("White","Black","Hispanic","Other"))

full_data$examiner_id_factor <- factor(full_data$examiner_id,levels=as.character(c(3007,c(3001:3012)[-7])))


# Binary health status, fixed effects on interviewer ----------------------

health_status_interviewer <- glm(health_status2 ~
                                   # Demographics
                                   race + gender + age + region + time_of_day + scale(WTPFHX6) +
                                   scale(systolic_bp) +
                                   locomotion + paralysis + impairment + infection +
                                   # scale(diastolic_bp) +
                                   scale(bmi) +
                                   # Lab variables
                                   # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                   # Examiner (physician) ID
                                   examiner_id_factor,
                                 # weights = scaled_weight,
                                 data = full_data,
                                 family = binomial(link = "logit"))

summary(health_status_interviewer)

# Regularization techniques
library(glmnet)
library(glmnetUtils)
health_status_interviewer_lasso<-glmnet(health_status2 ~
      # Demographics
      race + gender + age + region + time_of_day + scale(WTPFHX6) +
      scale(systolic_bp) +
      locomotion + paralysis + impairment + infection +
      scale(diastolic_bp) +
      scale(bmi) +
      # Lab variables
      scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
      # Examiner (physician) ID
        examiner_id_factor,
    # weights = scaled_weight,
    data = full_data,
    family = binomial(link = "logit"))

health_opt <- cv.glmnet(health_status2 ~
                          # Demographics
                          race + gender + age + region + time_of_day + scale(WTPFHX6) +
                          scale(systolic_bp) +
                          locomotion + paralysis + impairment + infection +
                          scale(diastolic_bp) +
                          scale(bmi) +
                          # Lab variables
                          scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                          # Examiner (physician) ID
                          examiner_id_factor,
                        # weights = scaled_weight,
                        data = full_data,
                        family = binomial(link = "logit"))

coef(health_opt,s="lambda.1se")
coef(health_opt,s="lambda.min")


# Machine learning --------------------------------------------------------