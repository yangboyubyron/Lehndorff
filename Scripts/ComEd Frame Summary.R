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
# write.csv(frame.rand,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Frame With Random 0404.csv")

# pull sample
frame.pull<-frame.rand %>% filter(rand.rank<=18*Quota) %>% 
  select(ID,CENSUS_TRACT_CODE,LI_score,SFMF,ws_ratio,e_heat,avg.annual,usage_level,Sample.Group,
    M01,M02,M03,M04,M05,M06,M07,M08,M09,M10,M11,M12)
table(frame.pull$Sample.Group)
# write.csv(frame.pull,"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull 0507.csv",row.names = FALSE)

# Pull summary
pull.summary.table<-frame.pull %>% group_by(SFMF,Heating=e_heat) %>% 
  summarise(Stratum.1=sum(usage_level=="Stratum 1")/18,Stratum.2=sum(usage_level=="Stratum 2")/18) %>% 
  arrange(desc(SFMF))
# write.xlsx(pull.summary.table %>% data.frame(),"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull Summary 0507.xlsx",sheetName = "Allocation Confirmation",row.names = FALSE)

pull.summary.full<-frame.pull %>% group_by(SFMF,Heating=e_heat,usage_level) %>% 
  summarise(count.in.pull=n(),allocation=count.in.pull/18,mean.li=mean(LI_score),mean.ws=mean(ws_ratio),mean.usage=mean(avg.annual)) %>% 
  arrange(desc(SFMF))
# write.xlsx(pull.summary.full %>% data.frame(),"/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Sample Pull Summary 0507.xlsx",sheetName = "Sample Summary",append = TRUE,row.names = FALSE)

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

# analysis for 04/26 Report
# usage strata
w_rate$usage_level<-"Not High Use"
w_rate$usage_level[w_rate$avg*12>=11000&w_rate$avg*12<14000]<-"Stratum 1"
w_rate$usage_level[w_rate$avg*12>=14000&w_rate$avg*12<=34000]<-"Stratum 2"
table(w_rate$usage_level)

table.2<-w_rate %>% group_by(IE=LI_score>.8,SFMF) %>% 
  summarise(n=n(),Median=median(avg*12),p.80=quantile(avg*12,probs = .8),p.90=quantile(avg*12,probs = .9)) %>% 
  arrange(-IE,desc(SFMF))

quantile(w_rate$avg*12,probs = c(.05,.5,.8,.9,.95))

# write.csv(table.2,"~/desktop/tab2.csv",row.names = FALSE)

probs<-c(seq(0,.9,.1),.995)

all<-data.frame("Quants"=probs,"All"=quantile(w_rate$avg*12,probs,names = FALSE))
lowest<-data.frame("Quants"=probs,"Lowest"=quantile(subset(w_rate,between(LI_score,0,.19999))$avg*12,probs,names = FALSE))
vlow<-data.frame("Quants"=probs,"Very Low"=quantile(subset(w_rate,between(LI_score,.2,.39999))$avg*12,probs,names = FALSE))
low<-data.frame("Quants"=probs,"Low"=quantile(subset(w_rate,between(LI_score,.4,.59999))$avg*12,probs,names = FALSE))
medium<-data.frame("Quants"=probs,"Medium"=quantile(subset(w_rate,between(LI_score,.6,.79999))$avg*12,probs,names = FALSE))
high<-data.frame("Quants"=probs,"High"=quantile(subset(w_rate,between(LI_score,.8,1))$avg*12,probs,names = FALSE))

usage.out<-left_join(all,lowest) %>% 
  left_join(vlow) %>% left_join(low) %>% 
  left_join(medium) %>% left_join(high)

# write.csv(usage.out,"~/desktop/usage_quant.csv",row.names = FALSE)

# Table 3 WS ratio
w_rate$li_group[w_rate$LI_score>0]<-"Low"
w_rate$li_group[w_rate$LI_score>=.6]<-"Medium"
w_rate$li_group[w_rate$LI_score>=.8]<-"High"
table(w_rate$li_group,exclude = NULL)

w_rate$ws_kWh<-w_rate$W_DJF-w_rate$Shoulder

all.3<-w_rate %>% group_by(Type="All",Group="All") %>% 
  # mutate(grp.ws=median(ws_ratio),ws_kWh=(ws_ratio-grp.ws)*W_DJF) %>%
  summarise(mean.ws=median(ws_ratio),p_1.4=mean(ws_ratio>1.4),med.use=median(ws_kWh[ws_ratio>1.4]))
HT.3<-w_rate %>% group_by(Type="SFMF",Group=SFMF) %>% 
  # mutate(grp.ws=median(ws_ratio),ws_kWh=(ws_ratio-grp.ws)*W_DJF) %>% 
  summarise(mean.ws=median(ws_ratio),p_1.4=mean(ws_ratio>1.4),med.use=median(ws_kWh[ws_ratio>1.4])) %>% arrange(desc(Group))
use.3<-w_rate %>% group_by(Type="Use",Group=usage_level) %>% 
  # mutate(grp.ws=median(ws_ratio),ws_kWh=(ws_ratio-grp.ws)*W_DJF) %>% 
  summarise(mean.ws=median(ws_ratio),p_1.4=mean(ws_ratio>1.4),med.use=median(ws_kWh[ws_ratio>1.4]))
LI.3<-w_rate %>% group_by(Type="LI",Group=li_group) %>% 
  # mutate(grp.ws=median(ws_ratio),ws_kWh=(ws_ratio-grp.ws)*W_DJF) %>%
  summarise(mean.ws=median(ws_ratio),p_1.4=mean(ws_ratio>1.4),med.use=median(ws_kWh[ws_ratio>1.4]))

table.3<-bind_rows(all.3,HT.3,use.3,LI.3)
# write.csv(table.3,"~/desktop/table3.csv",row.names = FALSE)

# Table 4 SS ratio
w_rate$ss_kWh<-w_rate$S_JA-w_rate$Shoulder

all.4<-w_rate %>% group_by(Type="All",Group="All") %>% 
  # mutate(grp.ss=median(ss_ratio),ss_kWh=(ss_ratio-grp.ss)*S_JA) %>% 
  summarise(mean.ss=median(ss_ratio),p_1.4=mean(ss_ratio>1.4),med.use=median(ss_kWh[ss_ratio>1.4]))
HT.4<-w_rate %>% group_by(Type="SFMF",Group=SFMF) %>% 
  # mutate(grp.ss=median(ss_ratio),ss_kWh=(ss_ratio-grp.ss)*S_JA) %>% 
  summarise(mean.ss=median(ss_ratio),p_1.4=mean(ss_ratio>1.4),med.use=median(ss_kWh[ss_ratio>1.4])) %>% arrange(desc(Group))
use.4<-w_rate %>% group_by(Type="Use",Group=usage_level) %>% 
  # mutate(grp.ss=median(ss_ratio),ss_kWh=(ss_ratio-grp.ss)*S_JA) %>% 
  summarise(mean.ss=median(ss_ratio),p_1.4=mean(ss_ratio>1.4),med.use=median(ss_kWh[ss_ratio>1.4]))
LI.4<-w_rate %>% group_by(Type="LI",Group=li_group) %>% 
  # mutate(grp.ss=median(ss_ratio),ss_kWh=(ss_ratio-grp.ss)*S_JA) %>% 
  summarise(mean.ss=median(ss_ratio),p_1.4=mean(ss_ratio>1.4),med.use=median(ss_kWh[ss_ratio>1.4]))

table.4<-bind_rows(all.4,HT.4,use.4,LI.4)
# write.csv(table.4,"~/desktop/table4.csv",row.names = FALSE)

w_rate$in.sample1<-w_rate$ID%in%frame.pull$ID
# save(w_rate,file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/Population All Fields.RData")
