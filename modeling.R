library(lme4)

full_data <- readRDS("data/full_data.rds")

full_data$race <- factor(full_data$race,levels=c("White","Black","Hispanic","Other"))



# Binary health status models ---------------------------------------------

health_status_interviewer_weighted <- glmer(health_status2 ~
                                     # Demographics
                                     race + gender + age + region + time_of_day + scale(WTPFHX6) +
                                     scale(systolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) +
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data,
                                   family = binomial(link = "logit"))


# This one is hit-or-miss on convergence
health_status_interviewer <- glmer(health_status2 ~
                                     # Demographics
                                     race + gender + age + region + time_of_day +
                                     scale(systolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) +
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data,
                                   family = binomial(link = "logit"))

health_status_interviewer_notime <- glmer(health_status2 ~
                                     # Demographics
                                     race + gender + age + region + 
                                       # time_of_day +
                                     scale(systolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) +
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data,
                                   family = binomial(link = "logit"))

health_status_interviewerno3002 <- glmer(health_status2 ~
                                     # Demographics
                                     race + gender + age + region + time_of_day +
                                     scale(systolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) +
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = subset(full_data,examiner_id!=3002),
                                   family = binomial(link = "logit"))

health_status_interviewer_rand <- glmer(health_status2 ~
                                     # Demographics
                                     race + age + region + time_of_day +
                                     scale(systolic_bp) +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) +
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1+gender|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data,
                                   family = binomial(link = "logit"))


# Remove "other" race
health_status_interviewer_noother <- glmer(health_status2 ~ 
                                     # Demographics
                                     race + gender + age + region + time_of_day +
                                     scale(systolic_bp) + 
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) + 
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data %>% filter(race!="Other"),
                                   family = binomial(link = "logit"))

# This model uses a hypertension indicator rather than scaled systolic bp
health_status_interviewer_hypertension <- glmer(health_status2 ~ 
                                     # Demographics
                                     race + gender + age + region + time_of_day +
                                     hypertension +
                                     locomotion + paralysis + impairment + infection +
                                     # scale(diastolic_bp) + 
                                     scale(bmi) +
                                     # Lab variables
                                     # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                     # Examiner (physician) ID
                                     (1|examiner_id),
                                   # weights = scaled_weight,
                                   data = full_data,
                                   family = binomial(link = "logit"))

referral_interviewer <- glmer(referral_level2 ~
                                # Demographics
                                race + gender + age + region + time_of_day +
                                scale(systolic_bp) +
                                locomotion + paralysis + impairment + infection +
                                # scale(diastolic_bp) +
                                scale(bmi) +
                                # Lab variables
                                # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                # Examiner (physician) ID
                                (1|examiner_id),
                              # weights = scaled_weight,
                              data = full_data,
                              family = binomial(link = "logit"))

referral_interviewer_race <- glmer(referral_level2 ~
                                # Demographics
                                 gender + age + region + time_of_day +
                                scale(systolic_bp) +
                                locomotion + paralysis + impairment + infection +
                                # scale(diastolic_bp) +
                                scale(bmi) +
                                # Lab variables
                                # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                # Examiner (physician) ID
                                (1+white|examiner_id),
                              # weights = scaled_weight,
                              data = full_data,
                              family = binomial(link = "logit"))

referral_interviewer_health <- glmer(referral_level2 ~
                                # Demographics
                                race + gender + age + region + time_of_day +
                                scale(systolic_bp) +
                                locomotion + paralysis + impairment + infection +
                                # scale(diastolic_bp) +
                                scale(bmi) +
                                health_status+
                                # Lab variables
                                # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                # Examiner (physician) ID
                                (1|examiner_id),
                              # weights = scaled_weight,
                              data = full_data,
                              family = binomial(link = "logit"))


# Cumulative logit (ordinal) health status models  ----------------------------------------------

library(ordinal)
ordinal_model<-clmm(as.factor(health_status) ~
       # Demographics
       race + gender + age + region + time_of_day +
       scale(systolic_bp) + 
       locomotion + paralysis + impairment + infection +
       # scale(diastolic_bp) + 
       scale(bmi) +
       # Lab variables
       # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
       # Examiner (physician) ID
       (1|examiner_id),
     # weights = scaled_weight,
     data = full_data)

ordinal_model2<-clmm2(as.factor(health_status) ~
                      # Demographics
                      race + gender + age + region + time_of_day +
                      scale(systolic_bp) + 
                      locomotion + paralysis + impairment + infection +
                      # scale(diastolic_bp) + 
                      scale(bmi),
                      random = factor(examiner_id),
                    # weights = scaled_weight,
                    data = full_data)


# This model uses hypertension rather than scaled systolic bp
ordinal_model_hypertension<-clmm(as.factor(health_status) ~
                      # Demographics
                      race + gender + age + region + time_of_day +
                      hypertension +
                      locomotion + paralysis + impairment + infection +
                      # scale(diastolic_bp) + 
                      scale(bmi) +
                      # Lab variables
                      # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                      # Examiner (physician) ID
                      (1|examiner_id),
                    # weights = scaled_weight,
                    data = full_data)

# Remove "other" race to match the glmer model
ordinal_model_match<-clmm(as.factor(health_status) ~
                      # Demographics
                      race + gender + age + region + time_of_day +
                      scale(systolic_bp) + 
                      locomotion + paralysis + impairment + infection +
                      # scale(diastolic_bp) + 
                      scale(bmi) +
                      # Lab variables
                      # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                      # Examiner (physician) ID
                      (1|examiner_id),
                    # weights = scaled_weight,
                    data = full_data %>% filter(race != "Other"))


# Linear models -----------------------------------------------------------

health_status_linear <- lmer(health_status ~
                                # Demographics
                                race + gender + age + region + time_of_day +
                                scale(systolic_bp) +
                                locomotion + paralysis + impairment + infection +
                                # scale(diastolic_bp) +
                                scale(bmi) +
                                # Lab variables
                                # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                                # Examiner (physician) ID
                                (1|examiner_id),
                              # weights = scaled_weight,
                              data = full_data,
                             REML=T)

health_status_linear_rand <- lmer(health_status ~
                               # Demographics
                               race + gender + age + region + time_of_day +
                               scale(systolic_bp) +
                               locomotion + paralysis + impairment + infection +
                               # scale(diastolic_bp) +
                               scale(bmi) +
                               # Lab variables
                               # scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                               # Examiner (physician) ID
                               (1+gender+white|examiner_id),
                             # weights = scaled_weight,
                             data = full_data,
                             REML=T)


# Lasso -------------------------------------------------------------------


