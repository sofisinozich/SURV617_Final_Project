library(tidyverse)
library(broom)

probabilities <- predict(health_status_interviewer, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

 
mydata <- full_data %>%
  drop_na(race, gender, age, region, time_of_day, WTPFHX6, systolic_bp, locomotion, paralysis, impairment, infection, bmi, 
          examiner_id_factor, health_status2) %>%
  dplyr::select(age, WTPFHX6, systolic_bp, bmi) 
predictors <- colnames(mydata)

mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
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

car::vif(health_status_interviewer)
#Note: We don't seem to have a problematic amount of multicollinearity.


