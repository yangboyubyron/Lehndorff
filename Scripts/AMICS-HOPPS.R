library(lubridate)
library(ggplot2)
library(dplyr)

day_bins<-readr::read_csv("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/hopps_daybins.csv")
track <- read.csv("/Volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Sample Data for Ted/Sample_Tracking.csv",stringsAsFactors = FALSE)
# correspond sample and said
track$sample_id<-0
track$sample_id[track$said==1]<-1
track$sample_id[track$said==2]<-2
track$sample_id[track$said==3]<-3
track$sample_id[track$said==4]<-4
track$sample_id[track$said==5]<-5
track$sample_id[track$said==7]<-6
track$sample_id[track$said==8]<-7
track$sample_id[track$said==434127]<-8
track$sample_id[track$said==13497992]<-9
track$sample_id[track$said==32748371]<-10
table(track$sample_id)

HVAC<-read.csv("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/SumHVAC_60min_2.csv",stringsAsFactors = FALSE)
HVAC$date<-date(HVAC$Date.Time)
HVAC$hour<-hour(HVAC$Date.Time)
# HVAC$hour[as.POSIXct(HVAC$Date.Time)<="2016-11-06 01:00:00"]<-HVAC$hour[as.POSIXct(HVAC$Date.Time)<="2016-11-06 01:00:00"]-1
HVAC$hour<-HVAC$hour-1
HVAC$date[HVAC$hour==-1]<-HVAC$date[HVAC$hour==-1]-1
HVAC$hour[HVAC$hour==-1]<-23

# add day bins to HVAC data
HVAC_station<-left_join(
  HVAC %>% filter(Date.Time!="2016-11-06 01:00:00"),
  track %>% select(sample_id,stationid,stationid_alt),
  by=c("Site"="sample_id"))

table(is.na(HVAC_station$Site),is.na(HVAC_station$stationid)&is.na(HVAC_station$stationid_alt))

HVAC_bins<-left_join(
  HVAC_station,
  day_bins %>% select(stationid,date,cdd_bin,day_bin),
  by=c("stationid","date"))

table(is.na(HVAC_bins$cdd_bin))

HVAC_site<-subset(HVAC_bins,Site==2)

load("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id2.Rdata")

Site_agg<-HVAC_site %>% filter(date<=max(pred_pre$date)) %>% group_by(cdd_bin,hour=hour) %>% summarise(mean_kWh=mean(sumHVACWh/1000/4))

range(Site_agg$cdd_bin)==range(pred_pre$cdd_bin)

HVAC_range<-Site_agg %>%
  group_by(hour) %>%
  summarise(
    high_hvac=mean(mean_kWh[cdd_bin==max(cdd_bin)]),
    low_hvac=mean(mean_kWh[cdd_bin==min(cdd_bin)]),
    hvac_range=high_hvac-low_hvac,
    hvac_pct=hvac_range/low_hvac)

AMICS_range<-pred_pre %>%
  group_by(hour=as.numeric(hour)) %>%
  summarise(
    high_amics=mean(fit[cdd_bin==max(cdd_bin)]),
    low_amics=mean(fit[cdd_bin==min(cdd_bin)]),
    amics_range=high_amics-low_amics,
    amics_pct_range=amics_range/low_amics)

ggplot(left_join(HVAC_range,AMICS_range,by="hour"))+
  geom_line(aes(x=hour,y=hvac_range),color="blue")+
  geom_line(aes(x=hour,y=amics_range),color="red")




ggplot(amics_lm)+
  geom_point(aes(x=as.numeric(hour),y=fit,color=day_bin,group=1))+
  facet_grid(.~substr(day_bin,1,1))



low<-amics_lm %>% filter(substr(day_bin,1,1)<=0) %>% group_by(hour=as.numeric(hour)) %>% summarise(mean_fit=mean(fit))
high<-amics_lm %>% filter(substr(day_bin,1,1)==4) %>% group_by(hour=as.numeric(hour)) %>% summarise(mean_fit=mean(fit))

ggplot()+
  geom_point(data=low,aes(x=hour,y=mean_fit),color="blue")+
  geom_point(data = high,aes(x=hour,y=mean_fit),color="red")

ggplot(left_join(high,low,by="hour"))+
  geom_line(aes(x=hour,y=mean_fit.x-mean_fit.y))

test<-subset(HVAC,Site=="3")

cdd<-dta_pre %>% group_by(date=as.Date(date)) %>% summarise(cdd=unique(cdd))

test$date<-date(test$Date.Time)
test$hour<-hour(test$Date.Time)

testcdd<-left_join(test,cdd,by="date")

testagg<-testcdd %>% group_by(cdd,hour) %>% summarise(mean_kWh=mean(sumHVACWh/1000))

#
ggplot(testagg %>% filter(cdd<=6))+
  geom_point(aes(x=hour,y=mean_kWh,color=as.factor(cdd)))+
  labs(color="cdd")

low_hvac<-subset(testagg,cdd==0)
high_hvac<-subset(testagg,cdd==4|cdd==4) %>% ungroup() %>% group_by(cdd=5,hour) %>% summarise(mean_kWh=mean(mean_kWh))

ggplot()+
  geom_point(data = low_hvac,aes(x=hour,y=mean_kWh),color="blue")+
  geom_point(data = high_hvac,aes(x=hour,y=mean_kWh),color="red")


ggplot()+
  geom_line(data=left_join(low_hvac,high_hvac,by="hour"),aes(x=hour,y=(mean_kWh.y-mean_kWh.x)/mean_kWh.x*100),color="blue")+
  geom_line(data=left_join(low,high,by="hour"),aes(x=hour,y=(mean_fit.y-mean_fit.x)/mean_fit.x*100),color="red")+
  labs(y="% kWh",title="% diff cdd4 cdd0")

ggplot()+
  geom_line(data=left_join(low_hvac,high_hvac,by="hour"),aes(x=hour,y=(mean_kWh.y-mean_kWh.x)),color="blue")+
  geom_line(data=left_join(low,high,by="hour"),aes(x=hour,y=(mean_fit.y-mean_fit.x)),color="red")+
  labs(y="kWh",title="kWh diff cdd4 cdd0")


