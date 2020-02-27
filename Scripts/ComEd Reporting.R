# ComEd Reporting
load("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/usage LI and rate code.RData")

ted.data<-read.csv("~/desktop/ComEd/Table for Hans.csv",stringsAsFactors = FALSE)

ted.fields<-w_rate %>% ungroup() %>% 
  select(ID,ws_ratio,LOWINCOME,
    vlow_inc_p,#
    high_pov_p,#
    med_inc_p,#
    Median.Income..Household.Based._p,#
    Net.Worth_p#
    )

for.ted<-left_join(ted.data,ted.fields,by="ID")
# write.csv(for.ted,"~/desktop/ComEd/Table for Ted.csv",row.names = FALSE)

TT.fields<-w_rate %>% ungroup() %>% 
  select(ID,avg)
for.TT<-left_join(ted.data %>% select(ID),TT.fields,by="ID") %>% mutate(Annual=avg*12)
# write.csv(for.TT,"/volumes/Projects/466002 - ComEd Needs Assessment/Task 3 - Interviews/Data/All_survey_usage.csv",row.names = FALSE)
