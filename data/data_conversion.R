# Data, SAS code, and documentation are available on the CDC website: 
# https://wwwn.cdc.gov/nchs/nhanes/nhanes3/DataFiles.aspx

# Convert SAS dat to workable format --------------------------------------
library(SAScii)
# Use the edited version of the SAS instructions, which correct
# for some missing character flags (identified by SAScii)

# This takes a very long time but it will finish
# Warnings are just NA coercions, they are OK
# 
# exam_data<-read.SAScii("data/exam.dat","data/exam_edited.sas")
# saveRDS(exam_data,"data/exam.rds")

exam_data <- readRDS("data/exam.rds")

# lab_data <- read.SAScii("data/lab.dat","data/lab_edited.sas")
# saveRDS(lab_data,"data/lab.rds")

lab_data <- readRDS("data/lab.rds")

# Only includes those where exam and lab data are available
full_data <- merge(exam_data,lab_data)
# saveRDS(full_data,"data/full_data.rds")

library(tidyverse)

full_data <- full_data %>% 
  # Demographics and outcomes
  mutate(race = case_when(DMARETHN == 1 ~ "White",
                          DMARETHN == 2 ~ "Black",
                          DMARETHN == 3 ~ "Hispanic",
                          DMARETHN == 4 & DMAETHNR == 2 ~ "Hispanic",
                          DMARETHN == 4 & DMAETHNR != 2 ~ "Other"),
         white = ifelse(race == "White",1,0),
         black = ifelse(race == "Black",1,0),
         gender = ifelse(HSSEX==1,"Male","Female"),
         age = ifelse(HSAGEU==2,HSAGEIR,NA_real_), # note topcoded at 90, consider limiting to adults
         region = case_when(DMPCREGN == 1 ~ "Northeast",
                            DMPCREGN == 2 ~ "Midwest",
                            DMPCREGN == 3 ~ "South",
                            DMPCREGN == 4 ~ "West"),
         west = ifelse(region == "West",1,0),
         time_of_day = case_when(MXPSESSR == 1 ~ "Morning",
                                 MXPSESSR == 2 ~ "Afternoon",
                                 MXPSESSR == 3 ~ "Evening"),
         day_of_week = ifelse(is.na(MXPTIDW), HXPTIDW, MXPTIDW) %>% ifelse(. == 8, NA_real_,.),
         examiner_id = ifelse(PEPTECH < 88888, PEPTECH,NA_real_),
         examiner_id_factor = relevel(factor(examiner_id,levels=as.character(c(3007,c(3001:3012)[-7]))),ref="3007"),
         health_status= ifelse(PEP13A <8, PEP13A, NA_real_),
         referral_level = ifelse(PEPLEVEL < 8, PEPLEVEL, NA_real_),
         referral_level2 = ifelse(referral_level == 3,0,1), # 1 = referred
         health_status2 = ifelse(health_status>1, 0,1), # 1 = status excellent
         health_status2evg = ifelse(health_status>2,0,1), # 1 = status vgood+
         health_statusevg = case_when(health_status == 1 ~ 1,
                                      health_status == 2 ~ 0) # 1 = status excellent
  ) %>% 
  # Health variables
  mutate(locomotion = case_when(PEP1 == 1 ~ 0,
                                PEP1 == 2 ~ 1),
         paralysis = case_when(PEP3A == 1 ~ 0,
                               PEP3A == 2 ~ 1),
         amputee_arm = case_when(PEP3B1 == 1 ~ 0,
                                 PEP3B1 == 2 ~ 1),
         amputee_leg = case_when(PEP3B2 == 1 ~ 0,
                                 PEP3B2 == 2 ~ 1),
         systolic_bp = ifelse(PEPMNK1R<888,PEPMNK1R,NA_real_),
         diastolic_bp = ifelse(PEPMNK5R<888,PEPMNK5R,NA_real_),
         chest = case_when(PEP7 == 1 ~ 0,
                           PEP7 == 2 ~ 1),
         heart = case_when(PEP8 == 1 ~ 0,
                           PEP8 == 2 ~ 1),
         systolic_murmur = ifelse(PEP8A<88,PEP8A,NA_real_),
         diastolic_murmur = ifelse(PEP8B<88,PEP8B,NA_real_),
         dermatitis = case_when(PEP9 == 1 ~ 0,
                                PEP9 == 2 ~ 1),
         impairment = case_when(PEP13B == 1 ~ 0,
                                PEP13B == 2 ~ 1),
         infection = case_when(PEP13C == 1 ~ 0,
                               PEP13C == 2 ~ 1),
         weight = ifelse(BMPWT < 888888, BMPWT,NA_real_),
         bmi = ifelse(BMPBMI < 8888, BMPBMI, NA_real_), 
         height = ifelse(BMPHT < 88888, BMPHT, NA_real_),
         waist_to_hip = ifelse(BMPWHR < 8888, BMPWHR, NA_real_),
         hypertension = ifelse(systolic_bp >= 130 | diastolic_bp >= 80,1,0)
  ) %>% 
  # Lab variables
  mutate(rbc_count = ifelse(RCP < 8888,RCP,NA_real_),
         wbc_count = ifelse(WCP < 88888,WCP,NA_real_),
         plt_count = ifelse(PLP < 88888,PLP,NA_real_),
         hemoglobin = ifelse(HGP < 88888,HGP,NA_real_)) %>% 
  # Limit sample to adults
  filter(age >= 18) %>% 
  # Limit sample to MEC-interviewees
  filter(WTPFEX6 > 0)

saveRDS(full_data,"data/full_data.rds")
