library(dplyr)

# creating end-use data from RBSAM data
path<-"/volumes/Projects/401013 - PG&E RBSA/Metering QC/RBSAM Data/RBSAM"
column<-"Dryer.kWh..Appliance."

fulldata<-NULL
for(i in 1:9){
  s<-proc.time()
  print(i)
  data<-read.csv(paste(path,i,".csv",sep=""),stringsAsFactors = FALSE)
  dataout<-select(data,c(siteid,time,(1:length(colnames(data)))[colnames(data)==column]))
  fulldata<-bind_rows(fulldata,dataout)
  rm(data)
  print(proc.time()-s)
}

# write.csv(paste(path,"dryers_only.csv",sep="/"))

# aggregation
library(lubridate)
s<-proc.time()
enduse<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Metering QC/RBSAM Data/RBSAM Subsets/dryers_only.csv",stringsAsFactors = FALSE)
proc.time()-s

enduse$formtime<-as.POSIXct(enduse$time,format="%d%b%y:%H:%M:%S")
enduse$week<-week(enduse$formtime)
enduse$date<-date(enduse$formtime)
enduse$hour<-hour(enduse$formtime)
enduse$year<-year(enduse$formtime)

Houragg<-enduse %>% filter(!is.na(formtime)) %>% group_by(siteid,year,week,date,hour) %>% summarise(obs=n(),kWh=sum(Dryer.kWh..Appliance.))
Dayagg<-Houragg %>% group_by(siteid,year,week,date) %>% summarise(obs=sum(obs),kWh=sum(kWh))
Weekagg<-Dayagg %>% group_by(siteid,year,week) %>% summarise(obs=sum(obs),kWh=sum(kWh))
Yearagg<-Weekagg %>% group_by(siteid,year) %>% summarise(obs=sum(obs),kWh=sum(kWh))

summary(Houragg$kWh)
summary(Dayagg$kWh)
summary(Yearagg$kWh)
