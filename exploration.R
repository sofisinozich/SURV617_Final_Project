# Exploratory analysis of the NHANES III exam data ------------------------

exam_data <- readRDS("data/exam.rds")

library(tidyverse)

# Distribution of scores given by examiner
exam_data %>% select(PEPTECH,PEP13A) %>%
  filter(PEPTECH <88888 & PEP13A < 8 ) %>% 
  ggplot +
  geom_bar(aes(x = PEP13A)) +
  facet_wrap(PEPTECH ~.)

exam_data %>% select(PEPTECH,PEPLEVEL) %>%
  filter(PEPTECH <88888 & PEPLEVEL < 8 ) %>% 
  ggplot +
  geom_bar(aes(x = PEPLEVEL)) +
  facet_wrap(PEPTECH ~.)

# Examinations by phase
exam_data %>% count(SDPPHASE,PEPTECH)


# Using recodes -----------------------------------------------------------

exam_data %>% ggplot + geom_boxplot(aes(group=examiner_id, y=health_status))


exam_data %>% select(examiner_id,health_status) %>% mutate(health_status = sqrt(health_status)) %>% 
  ggplot + geom_bar(aes(x = health_status)) + facet_wrap(examiner_id ~.)


exam_data %>% ggplot + geom_boxplot(aes(group = health_status, x = health_status, y = waist_to_hip))
