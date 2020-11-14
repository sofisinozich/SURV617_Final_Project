# SURV617 Final Project

This project is by [Rachael Jackson](https://github.com/rajackso) and [Sofi Sinozich](https://github.com/sofisinozich) for SURV617 Applications of Statistical Modeling.

## Background

Studies have found that physicians differ from one another in terms of the quality of examinations and physician-patient communication. Such differences are correlated with physicians traits (e.g., gender, race, experience), differences between patient case-mix (e.g., age range of patients, severity of disease of patients), and by the interaction between physician and patient traits (e.g., same vs. different gender or race). Although such differences in examination quality are to be somewhat expected, they can lead to measurement error in surveys, such as NHANES, which include physician examinations or in studies in which medical records from multiple physicians’ patients are used to determine health status. Physician effects may introduce total survey error if not accounted for, just as interviewer effects can. Thus, our study aims to determine the impact of physician effects in a national health study. 

## Data

For this analysis, we plan to use data from the third National Health and Nutrition Examination Survey (NHANES III), conducted from 1988-1994. As part of the study, 30,818 individuals were examined by physicians and a standardized regimen of physical tests were performed, results of which are available in the dataset. After the exam, the physician was asked to give an overall evaluation of the patient’s health (ranging from excellent to poor) as well as determine whether any health conditions observed were severe enough to warrant a referral for further medical attention, either immediately or in the next two weeks. Some examinations were performed at respondents’ homes, while others took place in designated examination facilities.

Twelve different physicians performed these exams during the administration of NHANES III, with caseloads ranging from 199 to 9,529 patients over the course of the study. We are using data from the NHANES III (vs. more recent iterations of NHANES) because this was the last data set in which they publicly released physician IDs.

## Research Questions

1. Are physician evaluations of overall health and the severity of health issues influenced by patient characteristics, such as race or gender?

2. For similar patients, what kind of variation is there in evaluations across different physicians?