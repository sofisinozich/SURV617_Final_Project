library(lme4)

health_status_model_uw <- glmer(health_status2 ~ 
                             # Demographics
                             race + gender + age + region + time_of_day + race:gender +
                             # Exam variables
                             locomotion + paralysis + scale(systolic_bp) + scale(diastolic_bp) +
                             # chest + heart + systolic_murmur + diastolic_murmur +
                             dermatitis + impairment + infection + scale(bmi) +
                             # Lab variables
                             scale(rbc_count) + scale(wbc_count) + scale(plt_count) + scale(hemoglobin) +
                             # Examiner (physician) ID
                             (1+race|examiner_id),
                           # weights = scaled_weight,
                           data = full_data,
                           family = binomial(link = "logit"))
