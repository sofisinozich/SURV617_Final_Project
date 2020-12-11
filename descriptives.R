# Descriptive statistics

library(ggplot)
library(tidyverse)
library(gridExtra)
full_data <- readRDS("data/full_data.rds")

full_data$race <- factor(full_data$race,levels=c("White","Black","Hispanic","Other"))
full_data$black <- ifelse(full_data$race=="Black",1,0)
full_data$west <- ifelse(full_data$region=="West",1,0)
full_data$examiner_id_factor <- relevel(factor(full_data$examiner_id,levels=as.character(c(3007,c(3001:3012)[-7]))),ref="3007")

# use this if you want the data that was actually used to fit the model
used_data<-model.frame(health_model_examiner_intgender) 

# Health status
phealth <- full_data %>% filter(!is.na(health_status)) %>% 
  count(health_status) %>% 
  ggplot + geom_bar(aes(x = health_status,y=n),stat="identity")+ 
  scale_x_continuous(breaks=1:5,labels = c("Excellent","Very good","Good","Fair","Poor")) +
  xlab("Health status (five-point)") +
  ylab("Count") +
  geom_label(aes(x = health_status,y=n,label=n))

phealth2 <- full_data %>% filter(!is.na(health_status)) %>% 
  count(health_status2) %>% 
  ggplot + geom_bar(aes(x = health_status2,y=n),stat="identity") + 
  scale_x_continuous(breaks = 0:1,labels = c("Very good or worse", "Excellent")) +
  xlab("Health status (dichotomized)") +
  ylab("Count") +
  geom_label(aes(x = health_status2,y=n,label=n))

grid.arrange(phealth,phealth2,ncol=2,top ="Health Status among NHANES III Adults") %>% ggsave(file="charts/health_status_counts.png",width=7,height=3)


# Caseloads

pexaminers <- full_data %>% filter(!is.na(examiner_id_factor)) %>% 
  count(examiner_id_factor) %>% 
  ggplot + geom_bar(aes(x = examiner_id_factor,y=n), stat="identity") +
  ylab("Count") +
  geom_label(aes(x =examiner_id_factor, y=n, label=n)) +
  xlab("Physician ID") +
  ggtitle("Adult Caseloads for Examining Physicians")
pexaminers %>% ggsave(file="charts/examiner_distribution.png",width=7,height=3)
