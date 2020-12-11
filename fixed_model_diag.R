library(tidyverse)
library(broom)

probabilities <- predict(health_model_examiner_intgender, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

 
mydata <- full_data %>%
  drop_na(black, gender, age, west,systolic_bp, diastolic_bp, locomotion, paralysis, impairment, infection,
            bmi, height,wbc_count, plt_count, hemoglobin,examiner_id_factor, health_status2) %>%
  dplyr::select(age, systolic_bp, bmi, height, wbc_count, plt_count, hemoglobin) 
predictors <- colnames(mydata)

mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

pred <- c("Age", "BMI", "Systolic BP", "Height", "WBC Count", "Platelet Count", "Hemoglobin Count")
names(pred) <- c("age", "bmi", "systolic_bp", "height", "wbc_count", "plt_count", "hemoglobin")
pred_val <- c("Predictor Value")
names(pred_val) <- c("predictor.value")

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y", labeller = labeller(predictors = pred)) + 
  labs(y = "Predictor Value", x = "Logit")
# I honestly can't tell if these could be considered roughly linear.

plot(health_status_interviewer, which = 4, id.n = 3)

model.data <- augment(health_status_interviewer) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = health_status2), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)
# Note: We seem to have 24 influential data points that could be skewing the data.

car::vif(health_model_examiner_intblackgender)
#Note: We don't seem to have a problematic amount of multicollinearity.


