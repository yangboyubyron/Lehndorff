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

error_metrics<-function(data,actual,predicted,Site=i){
 error_out<- data %>% select(hour,Actual=actual,Predicted=predicted) %>% 
    mutate(avg=mean(Actual, na.rm=TRUE)) %>%
    summarize(
      comp=paste(actual,predicted,sep="/"),
      Site=Site,
      TSS=sum((Actual-avg)^2),
      ESS=sum((Predicted-avg)^2),
      R_sq=ESS/TSS, n=n(),
      SSE=sum((Predicted-Actual)^2),
      RMSE=sqrt(SSE/(n-23)),
      CV_RMSE=RMSE/mean(Actual),
      NMBE=sum(Predicted-avg)/sum(avg))
  
  # error_dat<-data %>% 
  #   select(Site,hour,actual=actual,predicted=predicted) %>% 
  #   group_by(Site,hour) %>% 
  #   summarise(
  #     hour_actual=mean(actual),
  #     hour_predicted=mean(predicted),
  #     sse=sum((predicted-actual)^2, na.rm=TRUE), 
  #     n_obs = n())
  # 
  # error_out<-error_dat %>% 
  #   summarize(comp=paste(actual,predicted,sep="/"),
  #     act=sum(hour_actual), 
  #     pred=sum(hour_predicted),
  #     diff=sum(hour_predicted-hour_actual)/sum(hour_predicted),
  #     rmse=sqrt(sum(sse)/sum(n_obs)), 
  #     cv_rmse=sqrt(sum(sse)/sum(n_obs))/mean(hour_actual))
return(error_out)
  }

full_metout<-NULL

for (i in 1:7){
print(i)
  
load(paste0("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/Outputs/amics_ttow_id",i,".Rdata"))
load(paste0("~/desktop/AMI HVAC Write Up/AMI HVAC Outputs/amics_ttow_id",i,".Rdata"))

pred_pre_hvac$date<-as.Date(pred_pre_hvac$date)
pred_pre_hvac$hour<-as.numeric(pred_pre_hvac$hour)-1
pred_pre_hvac$date[pred_pre_hvac$hour==-1]<-pred_pre_hvac$date[pred_pre_hvac$hour==-1]-1
pred_pre_hvac$hour[pred_pre_hvac$hour==-1]<-23

HVAC_site<-HVAC_bins %>% filter(Site==unique(pred_pre$said)) %>% select(date,hour,sumHVACWh)

sel_id<-unique(pred_pre$said)
id_plot<-i

# clarify ami/pre-period overlap
site_pre_start<-min(HVAC_site$date)
site_pre_end<-max(HVAC_site$date[HVAC_site$sumHVACWh>10&HVAC_site$date<=max(pred_pre$date)])

full_data<-left_join(
  pred_pre %>% 
    # filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start) %>% 
    mutate(date=as.Date(date),hour=as.numeric(hour)),
  HVAC_site %>% filter(date<site_pre_end,date>site_pre_start) %>% mutate(hvac_est=sumHVACWh/1000/4),
  by=c("date","hour")
  ) %>% 
  left_join(pred_pre_hvac %>% mutate(date=as.Date(date),hour=as.numeric(hour),fit.hvac=fit/4) %>% select(date,hour,fit.hvac),by=c("date","hour")) %>% 
  # filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start)
  # filter(!is.na(hvac_est)&!is.na(fit.hvac))
  filter()

baseline<-full_data %>% filter(day_bin=="001"|day_bin=="000") %>% group_by(weekend,hour) %>% 
  summarise(n_base=n(),n_hvac=sum(!is.na(hvac_est)),base_amics=mean(fit,na.rm = TRUE),base_actual=mean(kwh,na.rm = TRUE),base_hvac=mean(hvac_est,na.rm = TRUE),base_hvmod=mean(fit.hvac,na.rm = TRUE))

full_ws<-full_data %>% 
  # filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start) %>%
  filter(!is.na(hvac_est)&!is.na(fit.hvac)) %>%
  left_join(baseline,by=c("weekend","hour")) %>% ungroup() %>% 
  mutate(ws_amics=fit-base_amics,ws_actual=kwh-base_actual,ws_hvac=hvac_est-base_hvac,ws_hvmod=fit.hvac-base_hvmod,ws_non_hvac=ws_actual-ws_hvac)

ws_agg<-full_ws %>% group_by(Site=i,hour,weekend) %>% 
  summarise(n=n(),ws_amics=mean(ws_amics,na.rm = FALSE),ws_actual=mean(ws_actual,na.rm = FALSE),ws_hvac=mean(ws_hvac,na.rm = FALSE),ws_hvmod=mean(ws_hvmod,na.rm = FALSE),ws_non_hvac=mean(ws_non_hvac,na.rm = FALSE))

ws_plot<-ggplot(ws_agg %>% ungroup() %>% select(-n,-Site) %>% reshape2::melt(id.vars=c("hour","weekend")))+
  geom_line(aes(x=hour,y=value,color=variable))+
  facet_grid(.~weekend)+
  labs(color="Source",y="WS Component",x="Hour/Day Type",title=paste0("Site ",unique(pred_pre$said),"/",i))+
  scale_color_manual(
    breaks=c("ws_actual","ws_hvac","ws_amics","ws_hvmod","ws_non_hvac"),
    labels=c("WS Actual","WS HVAC","WS AMICS","WS Mod. HVAC","WS non-HVAC"),
    values=c("blue","gray50","red","orange","green"))

# ggsave(plot = ws_plot,file=paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/plot_",i,".jpg"))

ws_agg2<-full_ws %>% group_by(Site=i,hour) %>% 
  # summarise(n=n(),ws_amics=mean(ws_amics,na.rm = FALSE),ws_actual=mean(ws_actual,na.rm = FALSE),ws_hvac=mean(ws_hvac,na.rm = FALSE),ws_hvmod=mean(ws_hvmod,na.rm = FALSE),ws_non_hvac=mean(ws_non_hvac,na.rm = FALSE))
  summarise(n=n(),fit=mean(fit),kwh=mean(kwh),ws_amics=mean(ws_amics,na.rm = TRUE),ws_actual=mean(ws_actual,na.rm = TRUE),ws_hvac=mean(ws_hvac,na.rm = TRUE),ws_hvmod=mean(ws_hvmod,na.rm = TRUE),ws_non_hvac=mean(ws_non_hvac,na.rm = TRUE))

# ws_plot<-ggplot(ws_agg2 %>% ungroup() %>% select(-n,-Site,-fit,-kwh) %>% reshape2::melt(id.vars=c("hour")))+
ws_plot<-ggplot(ws_agg2 %>% ungroup() %>% select(hour,ws_amics,ws_hvac) %>% reshape2::melt(id.vars=c("hour")))+
  geom_line(aes(x=hour,y=value,color=variable))+
  geom_hline(color="green",aes(yintercept=mean(value[variable=="ws_non_hvac"])))+
  # facet_grid(.~weekend)+
  labs(color="Data",y="Average Hourly Weather-Sensitive Change",x="Hour")+
  scale_color_manual(
    breaks=c("ws_actual","ws_hvac","ws_amics","ws_hvmod","ws_non_hvac"),
    labels=c("WS Actual","WS HVAC","WS AMICS","WS Mod. HVAC","WS non-HVAC"),
    values=c("red","blue","gray50","orange","green"))

# ggsave(plot = ws_plot,file=paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/plot_",i,".jpg"))

ws_amics.ws_hvac<-error_metrics(full_ws,"ws_hvac","ws_amics")

ws_amics.ws_actual<-error_metrics(full_ws,"ws_actual","ws_amics")

ws_actual.ws_hvac<-error_metrics(full_ws,"ws_hvac","ws_actual")

ws_hvac.ws_hvmod<-error_metrics(full_ws,"ws_hvac","ws_hvmod")

actual.amics<-error_metrics(full_ws,"kwh","fit")

hvac_actual.ws_amics<-error_metrics(full_ws,"hvac_est","ws_amics")

met_out<-bind_rows(actual.amics,ws_amics.ws_hvac,ws_amics.ws_actual,ws_actual.ws_hvac,ws_hvac.ws_hvmod,hvac_actual.ws_amics)

full_metout<-bind_rows(full_metout,met_out)
# write.csv(met_out,paste0("~/desktop/AMI HVAC Write Up/HOPPS-AMI 2/error_metrics_",i,".csv"))

avg_daily<-pred_pre %>%
  filter(as.Date(date)<site_pre_end,as.Date(date)>site_pre_start) %>%
  mutate(date=as.Date(date),hour=as.numeric(hour)) %>% 
  group_by(date) %>% summarise(daily=sum(kwh))

# print(mean(avg_daily$daily))
# print(error_metrics(data=pred_pre,actual = "kwh",predicted = "fit"))
# print(error_metrics(data=pred_pre_hvac,actual = "kwh",predicted = "fit"))
ws_data<-full_ws %>% 
  # filter(date=="2016-06-20") %>%
  group_by(hour) %>% 
  summarise(ws_hvac=mean(ws_hvac),ws_non_hvac=mean(ws_non_hvac),
    non_ws_hvac=mean(base_hvac),non_non=mean(base_actual-base_hvac),
    ws_amics=mean(ws_amics),base_amics=mean(base_amics),
    base=mean(base_actual),actual=mean(kwh),total=ws_hvac+ws_non_hvac+non_ws_hvac+non_non)

ggplot(ws_data %>% select(hour,ws_hvac,ws_non_hvac,non_ws_hvac,non_non) %>% reshape2::melt(id.vars="hour"))+
  geom_bar(aes(x=hour,y=value,fill=variable),position = "stack",stat="identity")+
  scale_fill_manual(
    breaks=c("ws_hvac","ws_non_hvac","non_ws_hvac","non_non"),
    labels=c("WS HVAC","WS non-HVAC","Baseline HVAC","Baseline non-HVAC"),
    values=c("blue","red","gray50","black")
  )+
  labs(y="kWh",x="Hour",fill="Component")

ggplot(ws_data %>% select(hour,ws_amics,base_amics) %>% reshape2::melt(id.vars="hour"))+
  geom_bar(aes(x=hour,y=value,fill=variable),position = "stack",stat="identity")+
  scale_fill_manual(
    breaks=c("ws_amics","base_amics"),
    labels=c("WS AMICS","Baseline AMICS"),
    values=c("blue","black")
  )+
  labs(y="kWh",x="Hour",fill="Component")
  # geom_line(data=ws_data,aes(x=hour,y=ws_amics+base))

ws_data<-full_ws %>% filter(date=="2016-06-20") %>% group_by(hour) %>% 
    # summarise(base=mean(base_actual),actual=mean(kwh))
    summarise(base=mean(base_hvac),actual=mean(hvac_est))

ws_calc<-ggplot(ws_data)+
  geom_ribbon(aes(x=hour,ymin=base,ymax=actual),fill="blue",alpha=.3)+
  # geom_ribbon(aes(x=hour,ymin=0,ymax=base),fill="black",alpha=.3)+
  geom_line(data=ws_data %>% reshape2::melt(id.vars="hour"),aes(x=hour,y=value,color=variable))+
  scale_color_manual(
    breaks=c("base","actual"),
    # labels=c("Baseline","Actual Usage"),
    labels=c("Baseline HVAC","Actual HVAC"),
    # values=c("black","blue")
    values=c("gray40","blue")
  )+
  labs(color="Data",y="kWh",x="Hours of June 20th, 2016")

ggplot(
  full_ws %>% 
    filter(date=="2016-06-20"&minute(readdate)==0) %>% 
    select(readdate,ws_hvac,ws_amics) %>% 
    reshape2::melt(id.vars="readdate"))+
  geom_line(aes(x=hour(readdate),y=value,color=variable))+
  scale_color_manual(
    breaks=c("ws_amics","ws_hvac"),
    labels=c("WS AMICS","WS HVAC"),
    values=c("blue","red")
  )+
  labs(color="Data",y="Weather-Sensitive Change (kWh)",x="Hours of June 20th")

# ggplot(full_ws)+
#   geom_line(aes(x=readdate,y=kwh),color="red")+
#   geom_line(aes(x=readdate,y=fit),color="blue")+
#   geom_line(aes(x=readdate,y=fit.hvac),color="orange")+
#   geom_line(aes(x=readdate,y=hvac_est),color="gray")

# ggplot(full_ws)+
#   # geom_line(aes(x=readdate,y=ws_actual),color="red")+
#   # geom_line(aes(x=readdate,y=ws_amics),color="blue")+
#   geom_line(aes(x=readdate,y=ws_hvmod),color="orange")+
#   geom_line(aes(x=readdate,y=ws_hvac),color="gray")+
#   labs(x=NULL)

# ggplot(full_ws %>% filter(date(readdate)>"2016-10-01"))+
#   geom_line(aes(x=readdate,y=base_actual),color="green")+
#   geom_line(aes(x=readdate,y=kwh),color="red")
#   geom_line(aes(x=readdate,y=base_actual+ws_actual),color="red")
  
# zzz<-full_ws %>% group_by(cdd_bin,hdd_bin,weekend) %>% summarise(n=n_distinct(date(readdate)),actual=mean(kwh),base=mean(base_actual),ws=mean(ws_actual))
# yyy<-full_ws %>% group_by(cdd_bin,hdd_bin,weekend) %>% summarise(n=n_distinct(date(readdate)),amics=mean(fit),base_amics=mean(base_amics),ws_amics=mean(ws_amics))
# fff<-left_join(zzz,yyy)
# fff$off<-fff$ws-fff$ws_amics
# fff$p<-fff$off/fff$ws

# AMICS2<-full_ws %>% group_by(day_bin,hour) %>% mutate(amics2=mean(kwh))
# 
# check_bin<-"000"
# ggplot(AMICS2 %>% filter(day_bin==check_bin))+
#   geom_line(aes(x=readdate,y=kwh),color="red")+
#   geom_hline(yintercept = mean(AMICS2$kwh[AMICS2$day_bin==check_bin]),color="red")+
#   geom_line(aes(x=readdate,y=fit),color="blue")+
#   geom_hline(yintercept=mean(AMICS2$fit[AMICS2$day_bin==check_bin]),color="blue")+
#   labs(title=NULL)

# print(ws_plot)
}
break
break

# write.csv(full_metout,"~/desktop/AMI HVAC Write Up/Full Error Metric.csv",row.names = FALSE)

plot_ttow <- function(x=dta_test, pre_test=TRUE, coord=c(18, 7), method="TTOW", color_labs=NULL){
  # x$weekend <- ifelse(x$weekend==0, "Weekend", "Weekday")
  dta_plot <- x %>% select(-readdate) %>% mutate(hour=substr(tow, 3, 4)) %>%
    group_by(said, hour) %>% summarize(actual=mean(kwh), predicted=mean(fit), lwr=mean(lwr), upr=mean(upr),
                                       sse=sum((fit-kwh)^2, na.rm=TRUE), n_obs = n())
  dta_plot$hour <- as.numeric(dta_plot$hour)
  pred_lab <- sprintf("Predicted (%s model %s)", method, ifelse(pre_test==TRUE, "sample pre", "est of post"))
  scale_ht <- (max(dta_plot$upr) - min(dta_plot$upr))/4
  if (is.null(color_labs)){
    color_labs <- c("Actual", pred_lab)
  }
  plot <- ggplot(data=dta_plot) +
    labs(title=sprintf("%s load shape for ID #%d", ifelse(pre_test==TRUE, "Pre-period HO", "Post-period"), id_plot), y="Energy Usage (kWh)", x="Hour of Day", color=NULL) +
    theme(legend.position="bottom") +
    geom_line(aes(x=hour, y=predicted, group=said, color=color_labs[2])) +
    geom_ribbon(aes(x=hour, ymax=upr, ymin=lwr, group=said), fill="red", alpha=0.1) +
    geom_line(aes(x=hour, y=actual, group=said, color=color_labs[1])) +
    scale_colour_manual("", breaks = color_labs, values = c("blue", "red")) +
    annotate("rect", xmin=coord[1]-5, xmax=coord[1]+5, ymin=coord[2]-scale_ht, ymax=coord[2]+scale_ht, alpha=1, fill="white")  #+
    #facet_grid(.~weekend)
  if (pre_test==TRUE){
    day <- dta_plot %>% summarize(
      act=sum(actual), pred=sum(predicted), diff=sum(predicted-actual)*100/sum(predicted),
      rmse=sqrt(sum(sse)/sum(n_obs)), cv_rmse=sqrt(sum(sse)/sum(n_obs))*24/sum(actual))
    plot +  annotate("text", label=paste(
      paste("Actual Daily kWh:",round(day$act, 2)),
      paste("Predicted Daily kWh:",round(day$pred, 2)),
      paste("% Difference: ", round(day$diff, 1), "%", sep=""),
      paste("RMSE:", round(day$rmse,2)),
      paste("CV(RMSE): ", round(day$cv_rmse,2), sep=""),
      sep="\n"),  x=coord[1], y=coord[2], color="black", size=3)
  } else {
    plot + annotate("text", label=paste(
      paste("Actual Daily kWh:",round(sum(dta_plot$actual), 2)),
      paste("Predicted Daily kWh:",round(sum(dta_plot$predicted), 2)),
      paste("Savings kWh: ", round(sum(dta_plot$predicted-dta_plot$actual), 2)),
      paste("Savings %: ", round(sum(dta_plot$predicted-dta_plot$actual)*100/sum(dta_plot$predicted), 1), "%", sep=""),
      # paste("CV(RMSE): ", round(sqrt(sum(savings$sse)/sum(savings$n_obs))/sum(savings$wt.act),2), sep=""),
      sep="\n"),  x=coord[1], y=coord[2], color="black", size=3)
  }
}

zzz<-pred_pre_hvac %>% mutate(fit=fit/4,kwh=kwh/4,lwr=lwr/4,upr=upr/4)
plot_ttow(subset(zzz, is.na(fit)==FALSE), pre_test=TRUE, coord=c(13, 2), "AMICS")

zzz<-pred_ho %>% select(readdate,kwh) %>% left_join(pred_ho_hvac %>% select(readdate,kwh),by="readdate")

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

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/1a. Weather Sensitivity (bin) by Hour.jpg"))

ws<-full_data %>%
  group_by(weekend.x) %>%
  mutate(high_cdd=cdd>=max(cdd)-2,low_cdd=cdd<=min(cdd)+2) %>%
  filter(high_cdd|low_cdd) %>%
  group_by(high_cdd,weekend.x,hour) %>%
  summarise(mean_amics=mean(fit,na.rm = TRUE),mean_hvac=mean(hvac_est,na.rm = TRUE),mean_ami=mean(kwh,na.rm = TRUE)) %>%
  group_by(hour,weekend.x) %>%
  summarise(
    amics_diff=mean_amics[high_cdd]-mean_amics[!high_cdd],
    hvac_diff=mean_hvac[high_cdd]-mean_hvac[!high_cdd],
    ami_diff=mean_ami[high_cdd]-mean_ami[!high_cdd])

ggplot(ws)+
  geom_line(aes(x=hour,y=ami_diff),color="gray50")+
  geom_line(aes(x=hour,y=amics_diff),color="red")+
  geom_line(aes(x=hour,y=hvac_diff),color="blue")+
  geom_line(aes(x=hour,y=amics_diff-hvac_diff),color="purple",alpha=.3,size=.5)+
  geom_line(aes(x=hour,y=ami_diff-hvac_diff),color="green",alpha=.3,size=.5)+
  labs(y=paste("difference between two highest cdd and two lowest cdd"),x="Hour",
    title=paste("Site",unique(HVAC_site$Site),"Weekday/Weekend"))+
  facet_grid(.~weekend.x)


# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/1b. Weather Sensitivity (cdd) by Hour.jpg"))


ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=kwh),color="black")+
  geom_line(aes(x=readdate,y=fit),color="red")+
  geom_line(aes(x=readdate,y=hvac_est),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit-hvac_est),color="purple")+
  # geom_line(alpha=.3,aes(x=readdate,y=kwh-hvac_est),color="orange")+
  coord_cartesian(xlim = c(as.POSIXct("2016-06-15"),as.POSIXct("2016-06-22")))+
  labs(title="actuals (one week), imputed non-HVAC",y="kW",x="Date")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/2. Hourly Comp Snapshot.jpg"))

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=runmean(fit,k=4*24*1,align = "right",endrule = "NA")),color="red")+
  geom_line(aes(x=readdate,y=runmean(hvac_est,k=4*24*1,align = "right",endrule = "NA")),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=runmean((fit-hvac_est),k=4*24*1,align = "right",endrule = "NA")),color="purple")+
  labs(title="one-day MA, imputed non-HVAC",y="kW",x="Date")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/3. Daily moving average.jpg"))

# calculate one-week MA
full_data$fit_av<-runmean(full_data$fit,k=4*24*7,align = "right",endrule = "NA")
full_data$fit_hvac<-runmean(full_data$hvac_est,k=4*24*7,align = "right",endrule = "NA")
full_data$fit_diff<-runmean(full_data$fit-full_data$hvac_est,k=4*24*7,align = "right",endrule = "NA")

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit_av),color="red")+
  geom_line(aes(x=readdate,y=fit_hvac),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit_diff),color="purple")+
  labs(title="one-week MA, imputed non-HVAC",y="kW",x="Date")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/4. Weekly moving average.jpg"))


# ggplot(full_data)+
#   geom_line(aes(x=readdate,y=fit_av/max(full_data$fit_av,na.rm = TRUE)),color="blue")+
#   geom_line(aes(x=readdate,y=fit_hvac/max(full_data$fit_hvac,na.rm = TRUE)),color="red")+
#   geom_line(alpha=.3,aes(x=readdate,y=fit_diff/max(full_data$fit_diff,na.rm = TRUE)),color="purple")+
#   labs(title="one-week MA % of max")

ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit_av-lag(fit_av)),color="red")+
  geom_line(aes(x=readdate,y=fit_hvac-lag(fit_hvac)),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=fit_diff-lag(fit_diff)),color="purple")+
  coord_cartesian(xlim = c(as.POSIXct("2016-06-15"),as.POSIXct("2016-06-22")))+
  labs(title="one-week MA rate of change",y="kW",x="Date")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/5. Rate of Change Weekly moving average.jpg"))


ggplot(full_data %>% filter(minute(readdate)==0))+
  geom_line(aes(x=readdate,y=fit-lag(fit)),color="red")+
  geom_line(aes(x=readdate,y=hvac_est-lag(hvac_est)),color="blue")+
  geom_line(alpha=.3,aes(x=readdate,y=(fit-hvac_est)-lag(fit-hvac_est)),color="purple")+
  coord_cartesian(xlim = c(as.POSIXct("2016-06-15"),as.POSIXct("2016-06-22")))+
  labs(title="actuals rate of change",y="kW",x="Date")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/6. Rate of Change actuals.jpg"))

cdd_hour_agg<-full_data %>%
  filter(minute(readdate)==0) %>%
  group_by(cdd_bin.x,hour,weekend.x) %>%
  summarise(avg_non_HVAC=mean(fit-hvac_est,na.rm = TRUE))

ggplot(cdd_hour_agg)+
  geom_line(aes(x=hour,y=avg_non_HVAC,color=as.factor(weekend.x)))+
  facet_grid(.~cdd_bin.x)+
  scale_color_manual(values=c("purple","navy"))+
  labs(title="Imputed non-HVAC by CDD",x="Hour",color="Weekend")

# ggsave(filename = paste0("~/desktop/HOPPS-AMI Plots/",i,"/7. Imputed non-HVAC by CDD.jpg"))

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


full_ws %>% mutate(model="amics", said=sel_id, avg=mean(ws_actual, na.rm=TRUE)) %>%
    summarize(model=first(model), said=first(said),
      TSS=sum((ws_actual-avg)^2),
      ESS=sum((ws_amics-avg)^2),
      R_sq=ESS/TSS, n=n(),
      SSE=sum((ws_amics-ws_actual)^2),
      RMSE=sqrt(SSE/(n-23)),
      CV_RMSE=RMSE/mean(ws_actual),
      NMBE=sum(ws_amics-avg)/sum(avg))

error_metrics(ws_agg2,"kwh","fit")
full_ws %>% mutate(model="amics", said=sel_id, avg=mean(kwh, na.rm=TRUE)) %>%
    summarize(model=first(model), said=first(said),
      TSS=sum((kwh-avg)^2),
      ESS=sum((fit-avg)^2),
      R_sq=ESS/TSS, n=n(),
      SSE=sum((fit-kwh)^2),
      RMSE=sqrt(SSE/(n-23)),
      CV_RMSE=RMSE/mean(kwh),
      NMBE=sum(fit-avg)/sum(avg))

full_ws_agg<-full_ws %>% group_by(hour) %>% 
  summarise(
    mean_fit=mean(fit),sd_fit=sd(fit),mean_kwh=mean(kwh),sd_kwh=sd(kwh),
    mean_wsfit=mean(ws_amics),sd_wsfit=sd(ws_amics),mean_wskwh=mean(ws_actual),sd_wskwh=sd(ws_actual))
ggplot(full_ws_agg)+
  geom_ribbon(aes(x=hour,ymin=mean_fit-sd_fit,ymax=mean_fit+sd_fit),fill="red",alpha=.2)+
  geom_ribbon(aes(x=hour,ymin=mean_kwh-sd_kwh,ymax=mean_kwh+sd_kwh),fill="blue",alpha=.2)+
  geom_line(aes(x=hour,y=mean_fit),color="red")+
  geom_line(aes(x=hour,y=mean_kwh),color="blue")

ggplot(full_ws_agg)+
  geom_ribbon(aes(x=hour,ymin=mean_wsfit-sd_wsfit,ymax=mean_wsfit+sd_wsfit),fill="red",alpha=.2)+
  geom_ribbon(aes(x=hour,ymin=mean_wskwh-sd_wskwh,ymax=mean_wskwh+sd_wskwh),fill="blue",alpha=.2)+
  geom_line(aes(x=hour,y=mean_wsfit),color="red")+
  geom_line(aes(x=hour,y=mean_wskwh),color="blue")


