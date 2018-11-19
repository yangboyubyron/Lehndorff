library(lubridate)

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


HVAC<-read.csv("/volumes/Projects/~ Closed Projects/419012 - SCE HOPPs AMI/Data/SumHVAC_60min.csv",stringsAsFactors = FALSE)

test<-subset(HVAC,Site=="3")

cdd<-dta_pre %>% group_by(date=as.Date(date)) %>% summarise(cdd=unique(cdd))

test$date<-date(test$Date.Time)
test$hour<-hour(test$Date.Time)

testcdd<-left_join(test,cdd,by="date")

testagg<-testcdd %>% group_by(cdd,hour) %>% summarise(mean_kWh=mean(sumHVACWh/1000))

low_hvac<-subset(testagg,cdd==0)
high_hvac<-subset(testagg,cdd==4|cdd==4) %>% ungroup() %>% group_by(cdd=5,hour) %>% summarise(mean_kWh=mean(mean_kWh))

ggplot()+
  geom_point(data = low_hvac,aes(x=hour,y=mean_kWh),color="blue")+
  geom_point(data = high_hvac,aes(x=hour,y=mean_kWh),color="red")


ggplot()+
  geom_line(data=left_join(low_hvac,high_hvac,by="hour"),aes(x=hour,y=mean_kWh.y-mean_kWh.x),color="blue")+
  geom_line(data=left_join(high,low,by="hour"),aes(x=hour,y=mean_fit.x-mean_fit.y),color="red")
