# Data wrangling for ComEd Survey
library(dplyr)
library(xlsx)

Contact<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/ComEd sample files with PPI/Evergreen Update 043019.txt",stringsAsFactors = FALSE)
DNC<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/Master_DNC_List_for_ComEd_07JUN18 120618.csv",stringsAsFactors=FALSE) #Not sure what this file is.
frame.pull<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull 0404.csv",stringsAsFactors = FALSE)
frame.rand<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Frame With Random 0404.csv",stringsAsFactors = FALSE)
load("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Population All Fields.RData")
quotas<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Sample Quotas.csv",stringsAsFactors = FALSE)

# Master file - an internal master file that keeps all the data we have for our own use. Usage information included at some level, including overall usage and the winter-shoulder and summer-shoulder ratios.
master<-X

# Survey file - file for the survey implementer that only includes a project customer ID, the sampling bins the customer falls into, and the contact information.
survey.frame<-frame.pull %>% left_join(Contact %>% 
    select(-BUDGET.BILL,-Message.Category,-Channel),
  by="ID") %>% mutate(DNC.ACC=ACCOUNT%in%DNC$account|ACCOUNT%in%DNC$Account,DNC.Phone=gsub("-","",Phone.Number)%in%DNC$telnum)

## DNC drop results
mean(survey.frame$DNC.ACC)
mean(survey.frame$DNC.Phone)
mean(survey.frame$Can.Contact!="Yes")
mean(survey.frame$TCPA.Date=="")
mean(survey.frame$Status!="Active")

survey.out<-survey.frame %>% 
  filter(Can.Contact=="Yes"&TCPA.Date!=""&Status=="Active"&DNC.ACC==FALSE&DNC.ACC==FALSE) %>% 
  select(-ws_ratio,-avg.annual,-ACCOUNT,-Status,-Can.Contact,-TCPA.Date)

### survey summary
survey.out.sum<-survey.out %>% 
  group_by(Sample.Group) %>% 
  summarise(survey.n=n()) %>% left_join(quotas,by=c("Sample.Group"="Segment")) %>% 
  mutate(ratio=survey.n/Quota) %>% arrange(ratio)

# write.csv(survey.out,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/ComEd sample files with PPI/Survey Frame 0506.csv",row.names=FALSE)

check.dnc<-survey.out %>% filter(ACCOUNT%in%DNC$Account)
surv.dnc<-DNC %>% filter(Account%in%check.dnc$ACCOUNT)
