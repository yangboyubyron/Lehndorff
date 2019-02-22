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

error_metrics<-function(data,actual,predicted,Site=i,plot=FALSE){
 error_out<- data %>% select(hour,Actual=actual,Predicted=predicted) %>% 
    mutate(avg=mean(Actual, na.rm=TRUE)) %>%
    summarize(
      comp=paste(actual,predicted,sep="/"),
      Site=Site,
      Avg_Actual=mean(Actual),
      Avg_Predicted=mean(Predicted),
      TSS=sum((Actual-avg)^2),
      ESS=sum((Predicted-avg)^2),
      R_sq=ESS/TSS, n=n(),
      SSE=sum((Predicted-Actual)^2),
      RMSE=sqrt(SSE/(n-23)),
      NMBE=sum(Predicted-avg)/sum(avg),
      CV_RMSE=RMSE/mean(Actual))
 
 if(plot==TRUE){
   plot_dat<-data %>% select(hour,Actual=actual,Predicted=predicted) %>% 
     group_by(hour) %>% 
     summarise(mean_act=mean(Actual),sd_act=sd(Actual),mean_pred=mean(Predicted),sd_pred=sd(Predicted))
   
   error_out<-ggplot(plot_dat)+
     geom_ribbon(aes(x=hour,ymin=mean_act-sd_act,ymax=mean_act+sd_act),fill="blue",alpha=.2)+
     geom_ribbon(aes(x=hour,ymin=mean_pred-sd_pred,ymax=mean_pred+sd_pred),fill="red",alpha=.2)+
     geom_line(aes(x=hour,y=mean_act),color="blue")+
     geom_line(aes(x=hour,y=mean_pred),color="red")+
     labs(x="Hour",y="WS kWh",title=paste("Site:",Site,actual,predicted))
     

 }
 
  return(error_out)
  }

full_metout<-NULL
full_compout<-NULL
data_stats<-NULL

for (i in 1:7){
print(i)
  
load(paste0("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id",i,".Rdata"))
load(paste0("~/desktop/AMI HVAC Write Up/AMI HVAC Outputs/amics_ttow_id",i,".Rdata"))

pred_pre_hvac$date2<-as.Date(pred_pre_hvac$date)
pred_pre_hvac$hour2<-as.numeric(pred_pre_hvac$hour)-1
pred_pre_hvac$date2[pred_pre_hvac$hour2==-1]<-pred_pre_hvac$date2[pred_pre_hvac$hour2==-1]-1
pred_pre_hvac$hour2[pred_pre_hvac$hour2==-1]<-23

# HVAC_site<-HVAC_bins %>% filter(Site==i) %>% select(date,hour,sumHVACWh)

sel_id<-unique(pred_pre$said)
id_plot<-i

# clarify ami/pre-period overlap
# site_pre_start<-min(HVAC_site$date)
# site_pre_end<-max(HVAC_site$date[HVAC_site$sumHVACWh>10&HVAC_site$date<=max(pred_pre$date)])

full_data<-left_join(
  pred_pre %>% 
    mutate(date=as.Date(date),hour=as.numeric(hour)) %>% 
    # group_by(date,hour) %>% summarise(kwh=sum(kwh),fit=sum(fit)) %>% 
    filter(),
  pred_pre_hvac %>% 
    mutate(date=as.Date(date2),hour=as.numeric(hour2),hvac_est=kwh/4,fit.hvac=fit/4) %>%
    # mutate(date=as.Date(date2),hour=as.numeric(hour2),hvac_est=kwh,fit.hvac=fit) %>% 
    select(date,hour,hvac_est,fit.hvac),
    # select(date,hour,day_bin,weekend,hvac_est,fit.hvac),
  by=c("date","hour")) %>% 
  filter()

baseline<-full_data %>% filter(day_bin=="001"|day_bin=="000") %>% group_by(weekend,hour) %>% 
  summarise(n_base=n(),n_hvac=sum(!is.na(hvac_est)),base_amics=mean(fit,na.rm = TRUE),base_actual=mean(kwh,na.rm = TRUE),base_hvac=mean(hvac_est,na.rm = TRUE),base_hvmod=mean(fit.hvac,na.rm = TRUE))

full_ws<-full_data %>% 
  # filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start) %>%
  filter(!is.na(hvac_est)&!is.na(fit.hvac)) %>%
  left_join(baseline,by=c("weekend","hour")) %>% ungroup() %>% 
  mutate(ws_amics=fit-base_amics,ws_actual=kwh-base_actual,ws_hvac=hvac_est-base_hvac,ws_hvmod=fit.hvac-base_hvmod,ws_non_hvac=ws_actual-ws_hvac)

ws_agg2<-full_ws %>% group_by(Site=i,hour) %>% 
  # summarise(n=n(),ws_amics=mean(ws_amics,na.rm = FALSE),ws_actual=mean(ws_actual,na.rm = FALSE),ws_hvac=mean(ws_hvac,na.rm = FALSE),ws_hvmod=mean(ws_hvmod,na.rm = FALSE),ws_non_hvac=mean(ws_non_hvac,na.rm = FALSE))
  summarise(n=n(),fit=mean(fit),kwh=mean(kwh),ws_amics=mean(ws_amics,na.rm = TRUE),ws_actual=mean(ws_actual,na.rm = TRUE),ws_hvac=mean(ws_hvac,na.rm = TRUE),ws_hvmod=mean(ws_hvmod,na.rm = TRUE),ws_non_hvac=mean(ws_non_hvac,na.rm = TRUE))

# ws_plot<-ggplot(ws_agg2 %>% ungroup() %>% select(-n,-Site,-fit,-kwh) %>% reshape2::melt(id.vars=c("hour")))+
ws_plot<-ggplot(ws_agg2 %>% ungroup() %>% select(hour,ws_amics,ws_hvac) %>% reshape2::melt(id.vars=c("hour")))+
  geom_line(aes(x=hour,y=value,color=variable))+
  geom_hline(color="green",aes(yintercept=mean(value[variable=="ws_non_hvac"])))+
  # facet_grid(.~weekend)+
  labs(color="Data",y="Average Hourly Weather-Sensitive Change (kWh)",x="Hour")+
  scale_color_manual(
    breaks=c("ws_actual","ws_hvac","ws_amics","ws_hvmod","ws_non_hvac"),
    labels=c("WS Actual","WS HVAC","WS AMICS","WS Mod. HVAC","WS non-HVAC"),
    values=c("red","blue","gray50","orange","green"))

# ggsave(plot = ws_plot,file=paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/plot_",i,".jpg"))

comp_data<-full_ws %>% 
  group_by(hour) %>% 
  summarise(ws_hvac=mean(ws_hvac),ws_non_hvac=mean(ws_non_hvac),
    non_ws_hvac=mean(base_hvac),non_non=mean(base_actual-base_hvac),
    ws_amics=mean(ws_amics),base_amics=mean(base_amics),
    base=mean(base_actual),actual=mean(kwh),total=ws_hvac+ws_non_hvac+non_ws_hvac+non_non)

comp_plot<-ggplot(comp_data %>% select(hour,ws_hvac,ws_non_hvac,non_ws_hvac,non_non) %>% reshape2::melt(id.vars="hour"))+
  geom_bar(aes(x=hour,y=value,fill=variable),position = "stack",stat="identity")+
  scale_fill_manual(
    breaks=c("ws_hvac","ws_non_hvac","non_ws_hvac","non_non"),
    labels=c("WS HVAC","WS non-HVAC","Baseline HVAC","Baseline non-HVAC"),
    values=c("blue","red","gray50","black")
  )+
  labs(y="kWh",x="Hour",fill="Component")

# ggsave(plot = comp_plot,file=paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/compplot_",i,".jpg"))


ws_amics.ws_hvac<-error_metrics(full_ws,"ws_hvac","ws_amics")

ws_amics.ws_actual<-error_metrics(full_ws,"ws_actual","ws_amics")

ws_actual.ws_hvac<-error_metrics(full_ws,"ws_hvac","ws_actual")

ws_hvac.ws_hvmod<-error_metrics(full_ws,"ws_hvac","ws_hvmod")

actual.amics<-error_metrics(full_ws,"kwh","fit")

hvac.fit_hvac<-error_metrics(full_ws,"hvac_est","fit.hvac")

hvac_actual.ws_amics<-error_metrics(full_ws,"hvac_est","ws_amics")

met_out<-bind_rows(actual.amics,ws_amics.ws_hvac,ws_amics.ws_actual,ws_actual.ws_hvac,ws_hvac.ws_hvmod,hvac_actual.ws_amics)
comp_out<-full_ws %>% summarise(
  Site=i,
  `Baseline non-HVAC`=mean(base_actual),
  `Baseline HVAC`=mean(base_hvac),
  `WS HVAC`=mean(ws_hvac),
  `WS non-HVAC`=mean(ws_non_hvac),
  `WS Actual`=mean(ws_actual),
  `WS AMICS`=mean(ws_amics),
  Error=`WS HVAC`-`WS AMICS`)

stats_out<-full_ws %>% 
  summarise(Site=i,
    min.date=min(date),
    max.date=max(date),
    total.days=n_distinct(date),
    base.days=n_distinct(date[(day_bin=="001"|day_bin=="000")]))

full_metout<-bind_rows(full_metout,met_out)
full_compout<-bind_rows(full_compout,comp_out)
data_stats<-bind_rows(data_stats,stats_out)
# write.csv(met_out,paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/error_metrics_",i,".csv"))

}

# write.csv(full_metout,"~/desktop/AMI HVAC Write Up/Full Error Metric.csv",row.names = FALSE)
# write.csv(full_compout,"~/desktop/AMI HVAC Write Up/Full Components.csv",row.names = FALSE)
# write.csv(data_stats,"~/desktop/AMI HVAC Write Up/Data Stats.csv",row.names = FALSE)

error_metrics(full_ws,"ws_hvac","ws_amics",plot=TRUE)
