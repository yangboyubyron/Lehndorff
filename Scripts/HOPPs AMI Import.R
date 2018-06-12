library(evergreen)
library(dplyr)
library(xlsx)
library(ggplot2)
library(reshape2)

# read submetering data
data.folder<-"/volumes/Projects/401027 - AMI Phase II/Data - CONFIDENTIAL/SCE Data/Data Delivery 010218/DR SCE AMI Phase II Field Data/Pre-Processed Data/"
subfolders<-list.files(path=data.folder)
for (i in 1:length(subfolders)){
  read.csv.folder(folder = paste(data.folder,subfolders[i],"/Pre-Processed Data/",sep = ""))
}

# read whole building data
wholebuild<-read.csv("/volumes/Projects/401027 - AMI Phase II/Data - CONFIDENTIAL/SCE Data/Data Delivery 010218/adhq1041/adhq1041_2_intrvl_201604.csv")
wholebuild$loc<-gsub("RFF|OFS|_| ","",wholebuild$Site_Description)
wholebuild$formdate<-as.character(strptime(wholebuild$Interval_Start_Dttm,format = "%d%b%Y:%H:%M:%S"))

MinutesTWO<-c(apropos("2.min"),apropos("2min"))
MinutesFIF<-apropos("15")
MinutesSIXT<-apropos("60")

# check for expected number of datasets (9 60 minute, 9 15 minute and 9 2 minute; one for each of the 9 sites)
if(length(MinutesTWO)==length(subfolders)&length(MinutesFIF)==length(subfolders)&length(MinutesSIXT)==length(subfolders)){"proceed"}else(warning("error: unexpected number of datasets"))

# columns to rows for each equipment-time, excluding temperature.
Data60<-NULL
Data15<-NULL
Data02<-NULL
for (j in c(MinutesSIXT,MinutesFIF,MinutesTWO)){
  if(j%in%MinutesSIXT){interval<-60}
  if(j%in%MinutesFIF){interval<-15}
  if(j%in%MinutesTWO){interval<-2}
  processing<-get(j)
  
  # change location names to conform to whole building names
  processing$loc<-NA
  if(grepl("santaana",j,ignore.case = TRUE)){processing$loc<-"SantaAna"}
  if(grepl("lakeforest.1",j,ignore.case = TRUE)){processing$loc<-"LakeForest(1)"}
  if(grepl("lakeforest2",j,ignore.case = TRUE)){processing$loc<-"LakeForest(2)"}
  if(grepl("signalhill",j,ignore.case = TRUE)){processing$loc<-"SignalHill"}
  if(grepl("irvine",j,ignore.case = TRUE)){processing$loc<-"Irvine"}
  if(grepl("huntington",j,ignore.case = TRUE)){processing$loc<-"Huntington"}
  if(grepl("torrance",j,ignore.case = TRUE)){processing$loc<-"Torrance"}
  if(grepl("placentia",j,ignore.case = TRUE)){processing$loc<-"Placentia"}
  if(grepl("covina",j,ignore.case = TRUE)){processing$loc<-"Covina"}
  processing$site<-j
  
  # Don't include temperature columns
  outcolumns<-colnames(processing)[colnames(processing)!="Date.Time"&colnames(processing)!="site"&colnames(processing)!="loc"&!grepl("temp",colnames(processing),ignore.case = TRUE)]
  siteout<-NULL
  for (k in outcolumns){
    siteproc<-select(processing,one_of("Date.Time","site","loc",k))
    
    # locate and simplfy equipment type
    if(grepl("AC|HR|LR|Unit",k)){
      siteproc$equip<-gsub("_","",gsub(".","",substr(k,regexpr("AC|HR|LR|Unit",k),regexpr("Ch",k)-1),fixed=TRUE))
    }else{
      siteproc$equip<-gsub("_","",gsub(".","",k,fixed=TRUE))
    }
    colnames(siteproc)<-c("Date.Time","site","loc","value","equip")
    siteout<-bind_rows(siteout,siteproc)
  }
  siteout$Date.Time[grepl("-",siteout$Date.Time)]<-as.character(strptime(siteout$Date.Time[grepl("-",siteout$Date.Time)],format = "%Y-%m-%d %H:%M:%S"))
  siteout$Date.Time[grepl("/",siteout$Date.Time)]<-as.character(strptime(siteout$Date.Time[grepl("/",siteout$Date.Time)],format = "%m/%d/%Y %H:%M"))
  if(interval==60){Data60<-bind_rows(Data60,siteout)}
  if(interval==15){Data15<-bind_rows(Data15,siteout)}
  if(interval==2){Data02<-bind_rows(Data02,siteout)}
}

# check whole building data continuity, pre/post
continuity<-wholebuild%>%group_by(loc,Site_no,date=as.Date(formdate))%>%summarise(n=n(),correctobs=(n==96),KWH=sum(Usage))
discontinuity<-continuity%>%filter(correctobs==FALSE)
View(discontinuity)

# distinguish pre/post using post start date (NA for two sites)
prepostdates<-read.xlsx("/volumes/Projects/401027 - AMI Phase II/Data - CONFIDENTIAL/SCE Data/Data Delivery 010218/DR SCE AMI Phase II Field Data/Results Analysis/1 - 4-27-2017_CQI Dashboard_Final.xlsx",sheetIndex = 1)%>%filter(!is.na(Site..))
prepostdates$prepostdate<-as.Date(as.numeric(prepostdates$Post.Period.Start),origin = "1899-12-30")
prepostdates$loc<-gsub("RFF|OFS|_| ","",prepostdates$Site.Description)

# count of pre/post obs by site for whole building
WBprepost<-left_join(continuity,select(prepostdates,c(loc,prepostdate)),by="loc")%>%group_by(loc,Site_no)%>%summarise(preobs=sum(n[as.Date(date)<as.Date(prepostdate)]),ADpreKWH=mean(KWH[as.Date(date)<as.Date(prepostdate)]),postobs=sum(n[as.Date(date)>=as.Date(prepostdate)]),total=sum(n))
View(WBprepost)

# check for overlap of submeter data and whole building data (at date level)
Data60range<-Data60%>%group_by(loc)%>%summarise(min60date=min(as.Date(Date.Time)),max60date=max(as.Date(Date.Time)),n60=length(unique(Date.Time)))
Data15range<-Data15%>%group_by(loc)%>%summarise(min15date=min(as.Date(Date.Time)),max15date=max(as.Date(Date.Time)),n15=length(unique(Date.Time)))
Data02range<-Data02%>%group_by(loc)%>%summarise(min02date=min(as.Date(Date.Time)),max02date=max(as.Date(Date.Time)),n2=length(unique(Date.Time)))

library(plyr)
AllIntRange<-join_all(list(Data60range,Data15range,Data02range),by="loc",type="left")
detach("package:plyr", unload=TRUE)

DataAllrange<-left_join(wholebuild%>%group_by(loc)%>%summarise(mindateWB=min(as.Date(formdate)),maxdateWB=max(as.Date(formdate))),
  AllIntRange%>%group_by(loc)%>%mutate(mindate=pmin(min60date,min15date,min02date),maxdate=pmax(max60date,max15date,max02date)),by="loc")%>%
  mutate(within=(as.Date(mindateWB)<as.Date(mindate)&as.Date(maxdateWB)>as.Date(maxdate)))

# number of sites where whole building date range contains entire submetering date range
table(DataAllrange$within)

# data breakdown
selecttrack<-select(prepostdates,c(loc,Baseline..Start.Date,Baseline.End.Date,Renno..Start,Renov..End,Post.Period.Start,Post.Period.End))
selecttrack$Renno..Start<-as.Date(as.numeric(selecttrack$Renno..Start),origin = "1899-12-30")
selecttrack$Renov..End<-as.Date(as.numeric(selecttrack$Renov..End),origin = "1899-12-30")
selecttrack$Post.Period.Start<-as.Date(as.numeric(selecttrack$Post.Period.Start),origin = "1899-12-30")
breakdown<-left_join(selecttrack,left_join(WBprepost,select(DataAllrange,c(loc,mindateWB,maxdateWB,min60date,max60date,n60)),by="loc"),by="loc")

# write.csv(breakdown,"~/desktop/AMIbreakdown.csv")

# aggregate (daily) submeter data and compare to whole building
Data60sum<-Data60%>%filter(equip!="TotalSystem")%>%group_by(loc,Date.Time=as.Date(Date.Time))%>%summarise(HVACtotal=sum(value))
Data15sum<-Data15%>%filter(equip!="TotalSystem")%>%group_by(loc,Date.Time=as.Date(Date.Time))%>%summarise(HVACtotal=sum(value)/4)
Data02sum<-Data02%>%filter(equip!="TotalSystem")%>%group_by(loc,Date.Time=as.Date(Date.Time))%>%summarise(HVACtotal=sum(value)/30)

Datasubsum<-left_join(Data60sum,left_join(Data15sum,Data02sum,by=c("loc","Date.Time"),suffix=c(".15",".02")),by=c("loc","Date.Time"))%>%
  group_by(loc,Date.Time=as.Date(Date.Time))%>%mutate(maxKWH=pmax(HVACtotal,HVACtotal.15,HVACtotal.02,na.rm=TRUE))

DataAllsum<-left_join(wholebuild%>%group_by(loc,Date.Time=as.Date(formdate))%>%summarise(HVACtotalWB=sum(Usage)*1000),Datasubsum,by=c("loc","Date.Time"))

# plot daily average HVAC, whole building and imputed non-HVAC
for(site in unique(DataAllsum$loc)){
  plotdata<-subset(DataAllsum,loc==site)
  plotdata$maxKWH[plotdata$maxKWH<1]<-NA
  plotdata$diff<-plotdata$HVACtotalWB-plotdata$maxKWH
  forplot<-melt(select(plotdata %>% ungroup(),c(Date.Time,`Whole Building`=HVACtotalWB,HVAC=maxKWH,`Imputed non-HVAC`=diff)),id = "Date.Time")
  plot<-ggplot(forplot)+
    geom_point(aes(x=as.Date(Date.Time),y=value,color=variable,alpha=variable))+
    scale_color_manual(values = c("red","blue","purple"))+
    scale_alpha_manual(values = c(1,1,.3))+
    labs(x="Date",y="kWh",color="Energy Type")+
    guides(alpha=FALSE)+
    ggtitle(paste("Energy Usage at",site,sep = " "))
  # ggsave(paste("~/desktop/HOPPs/Graphs/",site,".jpg",sep = ""),plot=plot,device = "jpeg")
  print(plot)
}

# calculate average pre EER
EER<-read.csv("~/desktop/2 - Results Analysis - 170430_Final.csv")
EERagg<-EER%>%group_by(Site.Name,Site..)%>%summarise(aEERin=weighted.mean(Test.In.System.Field.EER,w=Tons),aEERout=weighted.mean(Test.Out.System.Field.EER,w=Tons),exantesav=sum(Projected.Annual.Savings.Wh/1000))

# export data
Data60out<-Data60%>%filter(equip!="TotalSystem")%>%group_by(Site=loc,Date.Time)%>%summarise(sumHVACWh=sum(value))
Data60out$Site[Data60out$Site=="Covina"]<-3
Data60out$Site[Data60out$Site=="Huntington"]<-1
Data60out$Site[Data60out$Site=="Irvine"]<-6
Data60out$Site[Data60out$Site=="LakeForest(1)"]<-8
Data60out$Site[Data60out$Site=="LakeForets(2)"]<-9
Data60out$Site[Data60out$Site=="Placentia"]<-2
Data60out$Site[Data60out$Site=="SantaAna"]<-4
Data60out$Site[Data60out$Site=="SignalHill"]<-7
Data60out$Site[Data60out$Site=="Torrance"]<-5

# Data15out
# Data02out

# write.csv(Data60out,"/volumes/Projects/419XXX - SCE HOPPs AMI/Data/SumHVAC_60min.csv",row.names = FALSE)
