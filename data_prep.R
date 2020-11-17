# Data preparation for modeling and analysis ------------------------------

library(tidyverse)
library(magrittr)

full_data %<>%
  # Demographics and outcomes
  mutate(race = case_when(DMARETHN == 1 ~ "White",
                          DMARETHN == 2 ~ "Black",
                          DMARETHN == 3 ~ "Hispanic",
                          DMARETHN == 4 & DMAETHNR == 2 ~ "Hispanic",
                          DMARETHN == 4 & DMAETHNR != 2 ~ "Other"),
         gender = ifelse(HSSEX==1,"Male","Female"),
         age = HSAGEIR, # note topcoded at 90, consider limiting to adults
         region = case_when(DMPCREGN == 1 ~ "Northeast",
                            DMPCREGN == 2 ~ "Midwest",
                            DMPCREGN == 3 ~ "South",
                            DMPCREGN == 4 ~ "West"),
         time_of_day = ifelse(is.na(MXPSESSR), HXPSESSR, MXPSESSR) %>% ifelse(. == 8, NA_real_,.),
         day_of_week = ifelse(is.na(MXPTIDW), HXPTIDW, MXPTIDW) %>% ifelse(. == 8, NA_real_,.),
         examiner_id = ifelse(PEPTECH < 88888, PEPTECH,NA_real_),
         health_status= ifelse(PEP13A <8, PEP13A, NA_real_),
         referral_level = ifelse(PEPLEVEL < 8, PEPLEVEL, NA_real_),
         referral_level2 = ifelse(referral_level != 3,0,1), # 1 = referred
         health_status2 = ifelse(health_status>1, 0,1), # 1 = status less than excellent
         health_statusevg = case_when(health_status == 1 ~ 1,
                                      health_status == 2 ~ 0)
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
         waist_to_hip = ifelse(BMPWHR < 8888, BMPWHR, NA_real_)
         ) %>% 
  # Lab variables
  mutate(rbc_count = ifelse(RCP < 8888,RCP,NA_real_),
         wbc_count = ifelse(WCP < 88888,WCP,NA_real_),
         plt_count = ifelse(PLP < 88888,PLP,NA_real_),
         hemoglobin = ifelse(HGP < 88888,HGP,NA_real_)) %>% 
  # Limit sample to adults
  filter(age >= 18)