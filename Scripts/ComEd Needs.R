# ComEd Needs Propensity
library(dplyr)
library(pROC)
library(xlsx)
library(lubridate)
library(ggplot2)
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
use_agg$S_JA<-apply(use_agg %>% select(M07,M08),1,FUN=mean)
use_agg$W_DJF<-apply(use_agg %>% select(M12,M01,M02),1,FUN=mean)
use_agg$Shoulder<-apply(use_agg %>% select(M03,M04,M05,M10,M11),1,FUN=mean)

# metrics
# use_agg$sm_ratio<-use_agg$S_JJA/use_agg$min
# use_agg$wm_ratio<-use_agg$W_DJF/use_agg$min
# use_agg$sa_ratio<-use_agg$S_JJA/use_agg$avg
# use_agg$wa_ratio<-use_agg$W_DJF/use_agg$avg
# use_agg$am_ratio<-use_agg$avg/use_agg$min
use_agg$ss_ratio<-use_agg$S_JA/use_agg$Shoulder
use_agg$ws_ratio<-use_agg$W_DJF/use_agg$Shoulder
use_agg$sw_ratio<-use_agg$S_JA/use_agg$W_DJF
summary(use_agg)

usable<-use_agg %>% filter((min>0|Shoulder>0)&!is.na(avg)&!is.na(S_JA)&!is.na(W_DJF))
quantile(usable$ws_ratio,probs = c(.1,.25,.5,.75,.9),na.rm = TRUE)

hist(usable$ss_ratio[usable$ss_ratio<quantile(usable$ss_ratio,probs = .99)],breaks = 100)

usable$summer_group<-"NOT DEFINED"
usable$summer_group[usable$ss_ratio>=1.4&usable$summer_group=="NOT DEFINED"]<-"Up"
usable$summer_group[usable$ss_ratio<1.4&usable$ss_ratio>=.9&usable$summer_group=="NOT DEFINED"]<-"Flat"
usable$summer_group[usable$ss_ratio<.9&usable$summer_group=="NOT DEFINED"]<-"Down"
table(usable$summer_group)/nrow(usable)

# https://www.eia.gov/consumption/residential/data/2015/hc/php/hc6.7.php

hist(usable$ws_ratio[usable$ws_ratio<quantile(usable$ws_ratio,probs = .99)],breaks = 100)

usable$winter_group<-"NOT DEFINED"
usable$winter_group[usable$ws_ratio>=1.4&usable$winter_group=="NOT DEFINED"]<-"Up"
usable$winter_group[usable$ws_ratio<1.4&usable$ws_ratio>=.9&usable$winter_group=="NOT DEFINED"]<-"Flat"
usable$winter_group[usable$ws_ratio<.9&usable$winter_group=="NOT DEFINED"]<-"Down"
table(usable$winter_group)/nrow(usable)

# usage groups
hist(usable$S_JA[usable$S_JA<quantile(usable$S_JA,probs = .99)],breaks=100)
quantile(usable$S_JA,probs = seq(0,1,.05))
usable$s_group<-ifelse(usable$S_JA>=1500,TRUE,FALSE)
table(usable$s_group)

hist(usable$W_DJF[usable$W_DJF<quantile(usable$W_DJF,probs = .99)],breaks=100)
quantile(usable$W_DJF,probs = seq(0,1,.05))
usable$w_group<-ifelse(usable$W_DJF>=1300,TRUE,FALSE)
table(usable$w_group)

hist(usable$Shoulder[usable$Shoulder<quantile(usable$Shoulder,probs = .99)],breaks=100)
quantile(usable$Shoulder,probs = seq(0,1,.05))
usable$sh_group<-ifelse(usable$Shoulder>=1000,TRUE,FALSE)
table(usable$sh_group)

hist(usable$avg[usable$avg<quantile(usable$avg,probs = .99)],breaks=100)
quantile(usable$avg,probs = seq(0,1,.05))
usable$avg_group<-ifelse(usable$avg>=1100,TRUE,FALSE)
table(usable$avg_group)

use_cross<-usable %>% group_by(s_group,w_group,sh_group,avg_group) %>% summarise(n=n(),m_s=mean(S_JA),m_w=mean(W_DJF),m_sh=mean(Shoulder),m_avg=mean(avg))

usable$high_use<-apply(usable %>% select(s_group,w_group,sh_group,avg_group),1,FUN=max)
table(usable$high_use)

test<-usable %>% 
  group_by(high_use,summer_group,winter_group) %>% 
  summarise(n=n(),sw=mean(sw_ratio),sd_sw=sd(sw_ratio),Shoulder=mean(Shoulder),summer=mean(S_JA),winter=mean(W_DJF),group=unique(paste(high_use,summer_group,winter_group))) %>%
  group_by(high_use) %>% 
  mutate(p_use=n/sum(n)) %>% 
  arrange(-n)

test2<-usable %>% group_by(high_use,summer_group,winter_group) %>% mutate(rand=rank(runif(n())),group=paste(high_use,summer_group,winter_group))

usable$e_cool<-"NOT DEFINED"
usable$e_cool[usable$summer_group=="Up"]<-"CAC"
usable$e_cool[usable$summer_group=="Flat"]<-"Window Unit"
usable$e_cool[usable$summer_group=="Down"]<-"None"
table(usable$e_cool)

usable$e_heat<-ifelse(usable$winter_group=="Up","Elec Heat","Gas Heat")

test<-usable %>% 
  group_by(high_use,e_cool,e_heat) %>% 
  summarise(n=n(),sw=mean(sw_ratio),sd_sw=sd(sw_ratio),avg=mean(avg),Shoulder=mean(Shoulder),summer=mean(S_JA),winter=mean(W_DJF),group=unique(paste(high_use,e_cool,e_heat))) %>%
  group_by(high_use) %>% 
  mutate(p_use=n/sum(n)) %>% 
  arrange(-n)

test2<-usable %>% group_by(high_use,e_cool,e_heat) %>% mutate(rand=rank(runif(n())),group=paste(high_use,e_cool,e_heat))

for(i in unique(test$group)){
  print(i)
  plot_dat<-test2 %>% ungroup %>% filter(group==i&rand<9) %>% select(ID,contains("M",ignore.case = FALSE),Shoulder) %>% reshape2::melt(.,id.vars="ID")
  plot<-ggplot(plot_dat %>% filter(variable!="Shoulder"))+
    geom_hline(data = plot_dat %>% filter(variable=="Shoulder"),aes(yintercept=value),color="black")+
    geom_hline(data = plot_dat %>% filter(variable=="Shoulder"),aes(yintercept=value*1.4),color="green",alpha=.5)+
    geom_line(aes(x=as.Date(paste("2018",as.numeric(substr(variable,2,3)),"01",sep="-")),y=value),color="blue")+
    geom_point(aes(x=as.Date(paste("2018",as.numeric(substr(variable,2,3)),"01",sep="-")),y=value),color="black")+
    geom_vline(xintercept=c(as.numeric(as.Date("2018-02-15")),as.numeric(as.Date("2018-06-15")),as.numeric(as.Date("2018-08-15")),as.numeric(as.Date("2018-11-15"))),alpha=.5,color="red")+
    labs(x="Month",y="kWh",title=paste(i,test$n[test$group==i]))+
    facet_grid(ID~.,scales = "free")
  ggsave(plot,filename = paste0("~/desktop/ComEd Plots/",i,".jpg"),width = 7, height = 5)
}

modeling_data<-left_join(customers,agg_2016,by="ID") %>% left_join(agg_2017,"ID") %>% left_join(census,"ID") %>% left_join(PRIZM,by=c("PRIZM.Code"="code_merge"))
table(is.na(modeling_data$Full_Code))

# prizm + census for likelood of LI scale of 1-9 -> surveys. 

test<-usable %>% select(ID,contains("M",ignore.case = FALSE)) %>% reshape2::melt(.,id.vars="ID") %>% 
  # group_by(ID) %>% mutate(rank=rank(-as.numeric(value)))
  group_by(ID) %>% mutate(rank=value/max(value))

test2<-test %>% group_by(variable) %>% summarise(n=n(),med_rank=median(rank),mean_rank=mean(rank),sd_rank=sd(rank))
