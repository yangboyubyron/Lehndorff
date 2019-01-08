# ComEd Needs Propensity
library(dplyr)
library(pROC)
library(xlsx)
library(lubridate)
options(scipen = 999)

# Load customer/usage data
# customers<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Residential Base 101618.txt",stringsAsFactors = FALSE)
# use_2016<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2016 to 2017 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# use_2017<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2017 to 2018 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# save(list = c("customers","use_2016","use_2017"),file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

load(file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

# tracking/census
census<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/Latest matrix - estimated ie density by geography.xlsx",sheetName = "Sheet1")
PRIZM<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/PRIZM Premier Master Demographic Spreadsheet External 2017 HL.xlsx",sheetName = "Essentials") %>% mutate(code_merge=as.numeric(Full_Code))

# within LI, high users - avg monthly, minimum monthly, cooling in summer, heat with elec, summer/winter sholder ratio
# Usage aggregation
use_2016$month<-month(as.Date(paste(as.character(use_2016$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))
use_2017$month<-month(as.Date(paste(as.character(use_2017$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))

use_agg<-bind_rows(use_2016,use_2017) %>%
  group_by(ID,month) %>%
  summarise(avg=mean(BILLING_USAGE_QTY,na.rm = TRUE)) %>%
  group_by(ID) %>% 
  summarise(
    M01=max(avg[month==1],-Inf),
    M02=max(avg[month==2],-Inf),
    M03=max(avg[month==3],-Inf),
    M04=max(avg[month==4],-Inf),
    M05=max(avg[month==5],-Inf),
    M06=max(avg[month==6],-Inf),
    M07=max(avg[month==7],-Inf),
    M08=max(avg[month==8],-Inf),
    M09=max(avg[month==9],-Inf),
    M10=max(avg[month==10],-Inf),
    M11=max(avg[month==11],-Inf),
    M12=max(avg[month==12],-Inf))

use_agg[use_agg==-Inf]<-NA
table(is.na(use_agg))

use_agg$min<-apply(use_agg %>% select(-ID),1,FUN=min,na.rm=TRUE)
use_agg$avg<-apply(use_agg %>% select(-ID,-min),1,FUN=mean)
use_agg$S_JJA<-apply(use_agg %>% select(M06,M07,M08),1,FUN=mean)
use_agg$W_DJF<-apply(use_agg %>% select(M12,M01,M02),1,FUN=mean)

# metrics
use_agg$sm_ratio<-use_agg$S_JJA/use_agg$min
use_agg$wm_ratio<-use_agg$W_DJF/use_agg$min
use_agg$sa_ratio<-use_agg$S_JJA/use_agg$avg
use_agg$wa_ratio<-use_agg$W_DJF/use_agg$avg
use_agg$am_ratio<-use_agg$avg/use_agg$min
use_agg$sw_ratio<-use_agg$S_JJA/use_agg$W_DJF
summary(use_agg)

usable<-use_agg %>% filter((min>0|avg>0)&!is.na(S_JJA)&!is.na(W_DJF))
quantile(usable$sa_ratio,probs = c(.1,.25,.5,.75,.9),na.rm = TRUE)

usable$avg_group<-"NOT DEFINED"
usable$avg_group[is.na(usable$avg)]<-"Unknown"
usable$avg_group[usable$avg>=1000&usable$avg_group=="NOT DEFINED"]<-"High"
usable$avg_group[usable$avg<1000&usable$avg>=500&usable$avg_group=="NOT DEFINED"]<-"Medium"
usable$avg_group[usable$avg<500&usable$avg_group=="NOT DEFINED"]<-"Low"
table(usable$avg_group)

usable$summer_group<-"NOT DEFINED"
usable$summer_group[is.na(usable$sa_ratio)]<-"Unknown"
usable$summer_group[usable$sa_ratio>1.1&usable$summer_group=="NOT DEFINED"]<-"Up"
usable$summer_group[usable$sa_ratio<=1.1&usable$sa_ratio>.9&usable$summer_group=="NOT DEFINED"]<-"Flat"
usable$summer_group[usable$sa_ratio<=.9&usable$summer_group=="NOT DEFINED"]<-"Down"
table(usable$summer_group)

usable$winter_group<-"NOT DEFINED"
usable$winter_group[is.na(usable$wa_ratio)]<-"Unknown"
usable$winter_group[usable$wa_ratio>1.1&usable$winter_group=="NOT DEFINED"]<-"Up"
usable$winter_group[usable$wa_ratio<=1.1&usable$wa_ratio>.9&usable$winter_group=="NOT DEFINED"]<-"Flat"
usable$winter_group[usable$wa_ratio<=.9&usable$winter_group=="NOT DEFINED"]<-"Down"
table(usable$winter_group)

test<-usable %>% group_by(avg_group,summer_group,winter_group) %>% summarise(n=n(),sw=mean(sw_ratio),sd_sw=sd(sw_ratio),avg=mean(avg),summer=mean(S_JJA),winter=mean(W_DJF),group=unique(paste(avg_group,summer_group,winter_group))) %>% arrange(-n)
test2<-usable %>% group_by(avg_group,summer_group,winter_group) %>% mutate(rand=rank(runif(n())),group=paste(avg_group,summer_group,winter_group))

for(i in unique(test$group)){
  print(i)
  plot_dat<-test2 %>% ungroup %>% filter(group==i&rand<6) %>% select(ID,contains("M",ignore.case = FALSE),avg) %>% reshape2::melt(.,id.vars="ID")
  plot<-ggplot(plot_dat %>% filter(variable!="avg"))+
    geom_hline(data = plot_dat %>% filter(variable=="avg"),aes(yintercept=value))+
    geom_line(aes(x=as.Date(paste("2018",as.numeric(substr(variable,2,3)),"01",sep="-")),y=value),color="blue")+
    labs(x="Month",y="kWh",title=paste(i,test$n[test$group==i]))+
    facet_grid(ID~.,scales = "free")
  ggsave(plot,filename = paste0("~/desktop/ComEd Plots/",i,".jpg"),width = 7, height = 5)
}

usable$avg_group
agg_2016<-use_2016 %>% 
  group_by(ID) %>% 
  summarise(
    total_use_2016=sum(BILLING_USAGE_QTY),
    summer_use_2016=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201706|MM_ADJ_BILLING_YEARMO==201707|MM_ADJ_BILLING_YEARMO==201708]),
    winter_use_2016=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201612|MM_ADJ_BILLING_YEARMO==201701|MM_ADJ_BILLING_YEARMO==201702]))

agg_2017<-use_2017 %>% 
  group_by(ID) %>% 
  summarise(
    total_use_2017=sum(BILLING_USAGE_QTY),
    summer_use_2017=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201806|MM_ADJ_BILLING_YEARMO==201807|MM_ADJ_BILLING_YEARMO==201808]),
    winter_use_2017=sum(BILLING_USAGE_QTY[MM_ADJ_BILLING_YEARMO==201712|MM_ADJ_BILLING_YEARMO==201801|MM_ADJ_BILLING_YEARMO==201802]))

modeling_data<-left_join(customers,agg_2016,by="ID") %>% left_join(agg_2017,"ID") %>% left_join(census,"ID") %>% left_join(PRIZM,by=c("PRIZM.Code"="code_merge"))
table(is.na(modeling_data$Full_Code))

# prizm + census for likelood of LI scale of 1-9 -> surveys. 
