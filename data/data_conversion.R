# Data, SAS code, and documentation are available on the CDC website: 
# https://wwwn.cdc.gov/nchs/nhanes/nhanes3/DataFiles.aspx

# Convert SAS dat to workable format --------------------------------------
library(SAScii)
# Use the edited version of the SAS instructions, which correct
# for some missing character flags (identified by SAScii)

# This takes a very long time but it will finish
# Warnings are just NA coercions, they are OK

exam_data<-read.SAScii("data/exam.dat","data/exam_edited.sas")
saveRDS(exam_data,"data/exam.rds")

lab_data <- read.SAScii("data/lab.dat","data/lab_edited.sas")
saveRDS(lab_data,"data/lab.rds")

# Only includes those where exam and lab data are available
full_data <- merge(exam_data,lab_data)
# saveRDS(full_data,"data/full_data.rds")
