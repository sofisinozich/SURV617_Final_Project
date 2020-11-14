# Data, SAS code, and documentation are available on the CDC website: 
# https://wwwn.cdc.gov/nchs/nhanes/nhanes3/DataFiles.aspx

# Convert SAS dat to workable format --------------------------------------
library(SAScii)
# Use the edited version of the SAS instructions, which correct
# for some missing character flags (identified by SAScii)

# This takes a very long time but it will finish
# Warnings are just NA coercions, they are OK

exam_data<-read.SAScii("exam.dat","exam_edited.sas")

saveRDS(exam_data,"exam.rds")
