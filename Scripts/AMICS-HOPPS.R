library(lubridate)
library(ggplot2)
library(dplyr)
library(caTools)

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
  day_bins %>% select(stationid,date,cdd_bin,weekend,day_bin),
  by=c("stationid","date"))

table(is.na(HVAC_bins$cdd_bin))

for (i in 1:7){
print(i)
  
load(paste0("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id",i,".Rdata"))

# HVAC_site<-subset(HVAC_bins,Site==8)
HVAC_site<-subset(HVAC_bins,Site==unique(pred_pre$said))

# clarify ami/pre-period overlap
site_pre_start<-min(HVAC_site$date)
site_pre_end<-max(HVAC_site$date[HVAC_site$sumHVACWh>10&HVAC_site$date<=max(pred_pre$date)])

Site_agg<-HVAC_site %>% filter(date<=max(pred_pre$date)&date<site_pre_end) %>% group_by(cdd_bin,weekend,hour=hour) %>% summarise(mean_kWh=mean(sumHVACWh/1000/4))

range(Site_agg$cdd_bin)==range(pred_pre$cdd_bin)

full_data<-left_join(
  pred_pre %>% filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start) %>% mutate(date=as.Date(date),hour=as.numeric(hour)),
  HVAC_site %>% filter(date<site_pre_end,date>site_pre_start) %>% mutate(hvac_est=sumHVACWh/1000/4),
  by=c("date","hour")
  )

ws_bin<-full_data %>% 
  filter(cdd_bin.x==max(cdd_bin.x)|cdd_bin.x==min(cdd_bin.x)) %>% 
  group_by(cdd_bin.x,weekend.x,hour) %>% 
  summarise(mean_amics=mean(fit,na.rm = TRUE),mean_hvac=mean(hvac_est,na.rm = TRUE)) %>% 
  group_by(hour,weekend.x) %>% 
  summarise(
    amics_diff=mean_amics[cdd_bin.x==max(cdd_bin.x)]-mean_amics[cdd_bin.x==min(cdd_bin.x)],
    hvac_diff=mean_hvac[cdd_bin.x==max(cdd_bin.x)]-mean_hvac[cdd_bin.x==min(cdd_bin.x)])

ggplot(ws_bin)+
  geom_line(aes(x=hour,y=amics_diff),color="red")+
  geom_line(aes(x=hour,y=hvac_diff),color="blue")+
  geom_line(aes(x=hour,y=amics_diff-hvac_diff),color="purple",alpha=.3)+
  labs(y=paste("difference between cdd bin ",range(Site_agg$cdd_bin)[1]," and cdd bin ",range(Site_agg$cdd_bin)[2],sep = ""),x="Hour",
    title=paste("Site",unique(HVAC_site$Site),"Weekday/Weekend"))+
  facet_grid(.~weekend.x)

ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/1a. Weather Sensitivity (bin) by Hour.jpg"))

ws<-full_data %>% 
  group_by(weekend.x) %>% 
  mutate(high_cdd=cdd>=max(cdd)-2,low_cdd=cdd<=min(cdd)+2) %>% 
  filter(high_cdd|low_cdd) %>% 
  group_by(high_cdd,weekend.x,hour) %>% 
  summarise(mean_amics=mean(fit,na.rm = TRUE),mean_hvac=mean(hvac_est,na.rm = TRUE)) %>%
  group_by(hour,weekend.x) %>% 
  summarise(
    amics_diff=mean_amics[high_cdd]-mean_amics[!high_cdd],
    hvac_diff=mean_hvac[high_cdd]-mean_hvac[!high_cdd])

ggplot(ws)+
  geom_line(aes(x=hour,y=amics_diff),color="red")+
  geom_line(aes(x=hour,y=hvac_diff),color="blue")+
  geom_line(aes(x=hour,y=amics_diff-hvac_diff),color="purple",alpha=.3)+
  labs(y=paste("difference between two highest cdd and two lowest cdd"),x="Hour",
    title=paste("Site",unique(HVAC_site$Site),"Weekday/Weekend"))+
  facet_grid(.~weekend.x)


ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/1b. Weather Sensitivity (cdd) by Hour.jpg"))


ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit),color="red")+
  geom_line(aes(x=readdate,y=hvac_est),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit-hvac_est),color="purple")+
  coord_cartesian(xlim = c(as.POSIXct("2016-07-05"),as.POSIXct("2016-07-12")))+
  labs(title="actuals (one week), imputed non-HVAC",y="kW",x="Date")

ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/2. Hourly Comp Snapshot.jpg"))

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=runmean(fit,k=4*24*1,align = "right",endrule = "NA")),color="red")+
  geom_line(aes(x=readdate,y=runmean(hvac_est,k=4*24*1,align = "right",endrule = "NA")),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=runmean((fit-hvac_est),k=4*24*1,align = "right",endrule = "NA")),color="purple")+
  labs(title="one-day MA, imputed non-HVAC",y="kW",x="Date")

ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/3. Daily moving average.jpg"))

# calculate one-week MA
full_data$fit_av<-runmean(full_data$fit,k=4*24*7,align = "right",endrule = "NA")
full_data$fit_hvac<-runmean(full_data$hvac_est,k=4*24*7,align = "right",endrule = "NA")
full_data$fit_diff<-runmean(full_data$fit-full_data$hvac_est,k=4*24*7,align = "right",endrule = "NA")

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit_av),color="red")+
  geom_line(aes(x=readdate,y=fit_hvac),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit_diff),color="purple")+
  labs(title="one-week MA, imputed non-HVAC",y="kW",x="Date")

ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/4. Weekly moving average.jpg"))


# ggplot(full_data)+
#   geom_line(aes(x=readdate,y=fit_av/max(full_data$fit_av,na.rm = TRUE)),color="blue")+
#   geom_line(aes(x=readdate,y=fit_hvac/max(full_data$fit_hvac,na.rm = TRUE)),color="red")+
#   geom_line(alpha=.3,aes(x=readdate,y=fit_diff/max(full_data$fit_diff,na.rm = TRUE)),color="purple")+
#   labs(title="one-week MA % of max")

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit_av-lag(fit_av)),color="red")+
  geom_line(aes(x=readdate,y=fit_hvac-lag(fit_hvac)),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit_diff-lag(fit_diff)),color="purple")+
  coord_cartesian(xlim = c(as.POSIXct("2016-07-05"),as.POSIXct("2016-07-12")))+
  labs(title="one-week MA rate of change",y="kW",x="Date")

ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/5. Rate of Change Weekly moving average.jpg"))


ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit-lag(fit)),color="red")+
  geom_line(aes(x=readdate,y=hvac_est-lag(hvac_est)),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=(fit-hvac_est)-lag(fit-hvac_est)),color="purple")+
  coord_cartesian(xlim = c(as.POSIXct("2016-07-05"),as.POSIXct("2016-07-12")))+
  labs(title="actuals rate of change",y="kW",x="Date")
  
ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/6. Rate of Change actuals.jpg"))

}



cor(full_data$fit,full_data$fit-full_data$hvac_est)
cor(full_data$fit,full_data$hvac_est)
cor(full_data$fit_av,full_data$fit_diff,use = "complete.obs")
cor(full_data$fit_av,full_data$fit_hvac,use = "complete.obs")
table(full_data$hvac_est>full_data$fit)


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


