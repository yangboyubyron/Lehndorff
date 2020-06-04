# BPA Analysis and Cleaning in R
library(dplyr)

#### Load Data ####
# Tracking of surveys that have been mailed
sent_surveys<-readxl::read_xlsx("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - residential HVAC analysis/Res DHP Customer survey/Contact Data/Survey Intake.xlsx") %>% 
  filter(!is.na(CustomerID))

# Survey data entered in Qualtrics
survey_data<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_Data.csv",stringsAsFactors = FALSE)

# Corespondence of survey data columns and survey questions
data_fields<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/Survey_Fields.csv",stringsAsFactors = FALSE)

# Survey weights (utility respondents -> utility population)
survey_weights<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - residential HVAC analysis/Res DHP Customer survey/Contact Data/survey_weights.csv",stringsAsFactors = FALSE)

# Taylor's modeling results
model_results<-read.csv("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - Residential HVAC analysis/Res DHP customer survey/BPA DHP Usage Data.csv")

#### Re-categorizing a variable to a more sensible order (like Q42) ####
## What was Question 42?
### column "Question" from data_fields where column "Question_Number" is equal to Q42
data_fields$Question[data_fields$Question_Number=="Q42"]

##What is the frequency of responses to Q42
### create a table of column Q42 from "survey_data" 
table(survey_data$Q42)

##Create a new variable where answers to Q42 are ordered more logically
###column "Q42_Recode" of survey_data IS a factor of column "Q42" of survey_data with "" being the lowest level, then "Much lower energy bill", then...
survey_data$Q42_Recode<-factor(survey_data$Q42,levels = c("","Much lower energy bill","Slightly lower energy bill","About the same","Slightly higher energy bill","Much higher energy bill"))
table(survey_data$Q42_Recode) #responses to Q42 are now ordered correctly


#### create binary variable from survey responses ####
##Did respondent say that their home is a primary residence (Q3)?
###Frequency of Q3 responses
table(survey_data$Q3)

###Column "Q3_Recode" of survey_data IS when column Q3 of survey_data is equal to "Primary residence"
survey_data$Q3_Recode<-survey_data$Q3=="Primary residence"
table(survey_data$Q3_Recode)

#### Recode/re-group survey responses ####
##Were respondent more comfortable, less comfortable or as comfortable in the winter after installing their DHP (Q22)
###Frequency of Q22 responses
table(survey_data$Q22)

##Recode to more/less/same/NA (define a new column first)
###(new) column "Q22_Recode" of survey_data IS NA (empty)
survey_data$Q22_Recode<-NA

###"Q22_Recode" WHERE (Q22 is equal to "1 – Much less comfortable" OR WHERE Q22 is equal to 2 – Somewhat less comfortable) IS "Less comfortable"
survey_data$Q22_Recode[survey_data$Q22=="1 – Much less comfortable since installing the ductless heat pump"|survey_data$Q22=="2 – Somewhat less comfortable since installing the ductless heat pump"]<-"Less comfortable"

###...
survey_data$Q22_Recode[survey_data$Q22=="1 – Much less comfortable since installing the ductless heat pump"|survey_data$Q22=="2 – Somewhat less comfortable since installing the ductless heat pump"]<-"Less comfortable"
survey_data$Q22_Recode[survey_data$Q22=="4 – Somewhat more comfortable since installing the ductless heat pump"|survey_data$Q22=="5 – Much more comfortable since installing the ductless heat pump"]<-"More comfortable"
survey_data$Q22_Recode[survey_data$Q22=="3 – About the same comfort level since installing the ductless heat pump"]<-"Same comfort"
survey_data$Q22_Recode[survey_data$Q22==""]<-"Did not answer question"

### frequency of recoded responses
table(survey_data$Q22_Recode)

#### crosstabs (for counts only) ####
#What did people do with their old heating systmes v. when their home was built
table(survey_data$Q52,survey_data$Q30)

#### Merging datasets ####
#How did vbdd saving vary by winter comfort change and presence of floor insulation
##combine relevant datasets
###model_survey IS join model_results on to survey data where "Q59_1_TEXT" equal "CustomerID"
model_survey<-left_join(survey_data,model_results,by=c("Q59_1_TEXT"="CustomerID"))

##Summarise data after restricting to relevant cases
###survey_summary IS model_suvery, then
###filter TO cases where Q22_Recode does not equal "Did not answer question, then
###filter to cases where Q57_2 does not equal blank, then
###group the data by the responses to Q22_Recode and Q57_2, then
###summarise (one line per group) the data such that n equals the count of cases, mean_vbdd equals the mean of vbdd for cases...
survey_summary<-model_survey %>% 
  filter(Q22_Recode!="Did not answer question") %>% 
  filter(Q57_2!="") %>% 
  group_by(Q22_Recode,Q57_2) %>% 
  summarise(n=n(),mean_vbdd=mean(vbdd_savings),sd_vbdd=sd(vbdd_savings),median_vbdd=median(vbdd_savings))


#### Additional useful items ####
#Traditional cross tab for selected var
cross_tab<-survey_summary %>% 
  select(Q22_Recode,Q57_2,mean_vbdd) %>% 
  reshape2::dcast(Q22_Recode~Q57_2,value.var = "mean_vbdd")

#More complicated summary -- adds the percent that each response made up of Q22_Recode group
survey_summary2<-model_survey %>% 
  filter(Q22_Recode!="Did not answer question") %>% 
  filter(Q57_2!="") %>% 
  group_by(Q22_Recode,Q57_2) %>% 
  summarise(n=n(),mean_vbdd=mean(vbdd_savings),sd_vbdd=sd(vbdd_savings),median_vbdd=median(vbdd_savings)) %>% 
  group_by(Q22_Recode) %>% 
  mutate(pct_response=n/sum(n))

# simple scatter plot
plot(model_survey$vbdd_savings,model_survey$Pre_daily_kwh)
boxplot(model_survey$vbdd_savings~model_survey$Q22_Recode)

#quick summary stats
summary(model_survey$vbdd_savings)
