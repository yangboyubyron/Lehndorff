# Data wrangling for ComEd Survey
library(dplyr)
library(xlsx)

Contact<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/ComEd sample files with PPI/Evergreen Update 043019.txt",stringsAsFactors = FALSE)
DNC<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/Master_DNC_List_for_ComEd_07JUN18 120618.csv",stringsAsFactors=FALSE)
frame.pull<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull 0507.csv",stringsAsFactors = FALSE)
frame.rand<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Frame With Random 0404.csv",stringsAsFactors = FALSE)
load("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Population All Fields.RData")
quotas<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Sample Quotas.csv",stringsAsFactors = FALSE)

# Master file - an internal master file that keeps all the data we have for our own use. Usage information included at some level, including overall usage and the winter-shoulder and summer-shoulder ratios.
## already accomplished at end of Frame Summary
## add in response results and true IE status as it becomes available

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
  select(-CENSUS_TRACT_CODE,-ws_ratio,-avg.annual,-ACCOUNT,-Status,-Can.Contact,-TCPA.Date,-DNC.ACC,-DNC.Phone,
    -M01,-M02,-M03,-M04,-M05,-M06,-M07,-M08,-M09,-M10,-M11,-M12)
mean(survey.out$eMail!="")

survey.pii<-survey.frame %>% 
  filter(Can.Contact=="Yes"&TCPA.Date!=""&Status=="Active"&DNC.ACC==FALSE&DNC.ACC==FALSE) %>% 
  select(-Status,-Can.Contact,-DNC.ACC,-DNC.Phone)

### survey summary
survey.out.sum<-survey.out %>% 
  group_by(Sample.Group) %>% 
  summarise(survey.n=n()) %>% left_join(quotas,by=c("Sample.Group"="Segment")) %>% 
  mutate(ratio=survey.n/Quota) %>% arrange(ratio)

# write.csv(survey.out,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/ComEd sample files with PPI/Survey Frame External 0507.csv",row.names=FALSE)
# write.csv(survey.pii,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 2/ComEd sample files with PPI/Survey Frame INTERNAL 0507.csv",row.names=FALSE)

# Survey weights after survey completion
strata.counts<-frame.rand %>% group_by(Sample.Group) %>% summarise(pop.n=n())  

survey.results<-readxl::read_excel("/volumes/Projects/466002 - ComEd Needs Assessment/Task 2 - Survey/Data/ComEd Final Data Combined.xlsx",sheet = "ComEd Final Data Oct 3")
n_distinct(survey.results$ID)==nrow(survey.results)
table(survey.results$ID%in%frame.rand$ID)
table(survey.results$Sample.Group%in%strata.counts$Sample.Group)

survey.weights<-survey.results %>% select(ID,Sample.Group) %>% 
  group_by(Sample.Group) %>% mutate(surv.count=n()) %>% 
  left_join(strata.counts) %>% mutate(weight=pop.n/surv.count) %>% 
  select(ID,Sample.Group,weight)
table(survey.weights$ID%in%survey.results$ID)
# write.csv(survey.weights,"/volumes/Projects/466002 - ComEd Needs Assessment/Task 2 - Survey/Data/Survey Weights.csv",row.names = FALSE)

