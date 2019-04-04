# ComEd Sample Frame Summary
library(dplyr)
library(xlsx)

load("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/usage LI and rate code.RData")

# select customers with usage in samplable range
frame<-w_rate %>% 
  filter((avg*12>11000)&avg*12<=34000)

# redefine heating technology
frame$e_heat[frame$e_heat=="Gas Heat"]<-"Non-Elec Heat"

#average annual kWh
frame$avg.annual<-frame$avg*12

# usage strata
frame$usage_level<-"NOT DEFINED"
frame$usage_level[frame$avg.annual>=11000&frame$avg.annual<14000]<-"Stratum 1"
frame$usage_level[frame$avg.annual>=14000&frame$avg.annual<=34000]<-"Stratum 2"
table(frame$usage_level)

# IE definition n should equal 38,910
frame$LI_level<-ifelse(frame$LI_score>.8,"IE","non-IE")
table(frame$LI_level)

frame$Sample.Group<-paste(frame$LI_level,frame$SFMF,frame$e_heat,frame$usage_level)
table(frame$Sample.Group)

# select IE high users
frame.IE<-frame %>% filter(LI_level=="IE")
table(frame.IE$Sample.Group)

# add quotas
quotas<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Sample Quotas.csv",stringsAsFactors = FALSE)

frame.quota<-frame.IE %>% left_join(quotas,by=c("Sample.Group"="Segment"))

# assign random rank
set.seed(97244)
frame.rand<-frame.quota %>% ungroup() %>% 
  group_by(Sample.Group) %>% 
  mutate(rand.rank=rank(runif(n())))

# pull sample
frame.pull<-frame.rand %>% filter(rand.rank<=15*Quota) %>% select(ID,CENSUS_TRACT_CODE,LI_score,SFMF,ws_ratio,e_heat,avg.annual,usage_level,Sample.Group)
table(frame.pull$Sample.Group)
# write.csv(frame.pull,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull 0404.csv",row.names = FALSE)

# Pull summary
pull.summary.table<-frame.pull %>% group_by(SFMF,Heating=e_heat) %>% 
  summarise(Stratum.1=sum(usage_level=="Stratum 1")/15,Stratum.2=sum(usage_level=="Stratum 2")/15) %>% 
  arrange(desc(SFMF))
# write.xlsx(pull.summary.table %>% data.frame(),"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull Summary.xlsx",sheetName = "Allocation Confirmation",row.names = FALSE)

pull.summary.full<-frame.pull %>% group_by(SFMF,Heating=e_heat,usage_level) %>% 
  summarise(count.in.pull=n(),allocation=count.in.pull/15,mean.li=mean(LI_score),mean.ws=mean(ws_ratio),mean.usage=mean(avg.annual)) %>% 
  arrange(desc(SFMF))
# write.xlsx(pull.summary.full %>% data.frame(),"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull Summary.xlsx",sheetName = "Sample Summary",append = TRUE,row.names = FALSE)

# counts should match previous sample frame summary
frame.summary<-frame %>% 
  group_by(IE=LI_score>.8,SFMF,Heating=e_heat) %>% 
  summarise(
    c_9.5_11=sum(avg.annual>=9500&avg.annual<11000),
    c_11_12.5=sum(avg.annual>=11000&avg.annual<12500),
    c_12.5_14=sum(avg.annual>=12500&avg.annual<14000),
    c_g14=sum(avg.annual>=14000&avg.annual<=34000)) %>% 
  arrange(IE,desc(SFMF),Heating)

# write.csv(frame.summary,"~/desktop/ComEd Frame Summary TEMP.csv")

customers<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Residential Base 101618.txt",stringsAsFactors = FALSE)

# Confirm that customer ID is accurate and meaningful
# cust.check<-customers %>% 
#   select(ID,LOWINCOME,CENSUS_TRACT_CODE,Tariff.Rate.Typ,PRIZM.Code) %>% 
#   filter(ID%in%w_rate$ID) %>% arrange(ID)
#   
# data.check<-w_rate %>% ungroup() %>% select(ID,LOWINCOME,CENSUS_TRACT_CODE,Tariff.Rate.Typ,PRIZM.Code) %>% arrange(ID)
# 
# identical(cust.check$ID,data.check$ID)
# identical(cust.check$LOWINCOME,data.check$LOWINCOME)
# identical(cust.check$PRIZM.Code,data.check$PRIZM.Code)
# identical(cust.check$Tariff.Rate.Typ,data.check$Tariff.Rate.Typ)
# 
# rm(cust.check,data.check)

