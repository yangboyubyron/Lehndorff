# AK
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(caTools)
library(forecast)

add_forcast<-function(data,groups,var_to_cast){
  output<-NULL
  for(i in groups){
    data_sub<-data %>% 
      filter(Group==i) 
    
    start_month<-last(data_sub$FiscalMonth)
    start_year<-last(data_sub$FiscalYear)

    data_for_cast<-data_sub%>% select(var_to_cast)
    
    six_month<-as.vector(forecast(auto.arima(data_for_cast[,1]),h=6)$mean)
    
   FiscalYear<-rep(start_year,6)
   FiscalMonth<-(start_month+1):(start_month+6)
   Group<-rep(i,6)
   
   group_out<-as.data.frame(cbind(as.data.frame(FiscalYear),FiscalMonth,Group,six_month))
   output<-bind_rows(output,group_out)
  }

  full_out<-bind_rows(data,output)
}

enrollment<-read.csv("/volumes/Projects/Alaska DHSS/SpendingPerMemberReporting/FY2017_Enrollment.csv",stringsAsFactors = FALSE)
enrollment$merge<-paste(enrollment$BSysID,enrollment$FiscalYear,enrollment$FiscalMonth,sep="-")

spending<-read.csv("/volumes/Projects/Alaska DHSS/SpendingPerMemberReporting/FY2017_Spending.csv",stringsAsFactors = FALSE)
spending$merge<-paste(spending$BSysID,spending$FiscalYear,spending$FiscalMonth,sep="-")

enroll_spending<-left_join(enrollment,spending %>% select(-BSysID,-FiscalYear,-FiscalMonth),by="merge")
enroll_spending$CalendarDate<-years(enroll_spending$FiscalYear)+months(enroll_spending$FiscalMonth+6)

enroll_spending$Group<-"Not Defined"
enroll_spending$Group[enroll_spending$Age<20]<-"Children"
enroll_spending$Group[enroll_spending$Age>=20&enroll_spending$Age<65&enroll_spending$ElgClass=="88"]<-"Expansion"
enroll_spending$Group[enroll_spending$Age>=20&enroll_spending$Age<65&enroll_spending$ElgClass!="88"&enroll_spending$ElgClass!="56"&enroll_spending$ElgClass!="54"&enroll_spending$ElgClass!="52"&enroll_spending$ElgClass!="50"]<-"Other Non-Disabled Adults"
enroll_spending$Group[enroll_spending$Age>=65|(enroll_spending$Age>=20&enroll_spending$Age<65&(enroll_spending$ElgClass=="56"|enroll_spending$ElgClass=="54"|enroll_spending$ElgClass=="52"|enroll_spending$ElgClass=="50"))]<-"Senior or Disabled"

table(enroll_spending$ElgClass[enroll_spending$Group=="Not Defined"],is.na(enroll_spending$Age[enroll_spending$Group=="Not Defined"]))

NotDefined<-subset(enroll_spending,Group=="Not Defined")

AllSpendbyGroup<-enroll_spending %>% 
  filter(Group!="Not Defined") %>%
  group_by(FiscalYear,FiscalMonth,Group) %>% 
  summarise(
    CountElig=n_distinct(BSysID[!is.na(Spend01)]),
    SpendPerElig_01=sum(Spend01,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend01)]),
    SpendPerElig_02=sum(Spend02,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend02)]),
    SpendPerElig_03=sum(Spend03,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend03)]),
    SpendPerElig_04=sum(Spend04,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend04)]),
    SpendPerElig_05=sum(Spend05,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend05)]),
    SpendPerElig_06=sum(Spend06,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend06)]),
    SpendPerElig_07=sum(Spend07,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend07)]),
    SpendPerElig_08=sum(Spend08,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend08)]),
    SpendPerElig_09=sum(Spend09,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend09)]),
    SpendPerElig_10=sum(Spend10,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend10)]),
    SpendPerElig_11=sum(Spend11,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend11)]),
    SpendPerElig_12=sum(Spend12,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend12)]),
    SpendPerElig_13=sum(Spend13,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend13)]),
    SpendPerElig_14=sum(Spend14,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend14)]),
    SpendPerElig_15=sum(Spend15,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend15)]),
    SpendPerElig_16=sum(Spend16,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend16)]),
    SpendPerElig_17=sum(Spend17,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend17)]),
    SpendPerElig_18=sum(Spend18,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend18)]),
    SpendPerElig_19=sum(Spend19,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend19)]),
    SpendPerElig_20=sum(Spend20,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend20)]),
    StatePerElig_01=sum(StateSpend01,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend01)]),
    StatePerElig_02=sum(StateSpend02,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend02)]),
    StatePerElig_03=sum(StateSpend03,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend03)]),
    StatePerElig_04=sum(StateSpend04,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend04)]),
    StatePerElig_05=sum(StateSpend05,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend05)]),
    StatePerElig_06=sum(StateSpend06,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend06)]),
    StatePerElig_07=sum(StateSpend07,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend07)]),
    StatePerElig_08=sum(StateSpend08,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend08)]),
    StatePerElig_09=sum(StateSpend09,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend09)]),
    StatePerElig_10=sum(StateSpend10,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend10)]),
    StatePerElig_11=sum(StateSpend11,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend11)]),
    StatePerElig_12=sum(StateSpend12,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend12)]),
    StatePerElig_13=sum(StateSpend13,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend13)]),
    StatePerElig_14=sum(StateSpend14,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend14)]),
    StatePerElig_15=sum(StateSpend15,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend15)]),
    StatePerElig_16=sum(StateSpend16,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend16)]),
    StatePerElig_17=sum(StateSpend17,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend17)]),
    StatePerElig_18=sum(StateSpend18,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend18)]),
    StatePerElig_19=sum(StateSpend19,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend19)]),
    StatePerElig_20=sum(StateSpend20,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend20)])
    ) %>% 
  group_by(Group) %>% 
  mutate(Date=factor(paste(FiscalYear,FiscalMonth,sep="-"),levels = paste(FiscalYear,FiscalMonth,sep="-"))) %>% 
  ungroup()

AllSpendbyAll<-enroll_spending %>% 
  filter(Group!="Not Defined") %>%
  group_by(FiscalYear,FiscalMonth,Group="All Enrollees") %>% 
  summarise(
    CountElig=n_distinct(BSysID[!is.na(Spend01)]),
    SpendPerElig_01=sum(Spend01,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend01)]),
    SpendPerElig_02=sum(Spend02,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend02)]),
    SpendPerElig_03=sum(Spend03,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend03)]),
    SpendPerElig_04=sum(Spend04,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend04)]),
    SpendPerElig_05=sum(Spend05,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend05)]),
    SpendPerElig_06=sum(Spend06,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend06)]),
    SpendPerElig_07=sum(Spend07,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend07)]),
    SpendPerElig_08=sum(Spend08,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend08)]),
    SpendPerElig_09=sum(Spend09,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend09)]),
    SpendPerElig_10=sum(Spend10,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend10)]),
    SpendPerElig_11=sum(Spend11,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend11)]),
    SpendPerElig_12=sum(Spend12,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend12)]),
    SpendPerElig_13=sum(Spend13,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend13)]),
    SpendPerElig_14=sum(Spend14,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend14)]),
    SpendPerElig_15=sum(Spend15,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend15)]),
    SpendPerElig_16=sum(Spend16,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend16)]),
    SpendPerElig_17=sum(Spend17,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend17)]),
    SpendPerElig_18=sum(Spend18,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend18)]),
    SpendPerElig_19=sum(Spend19,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend19)]),
    SpendPerElig_20=sum(Spend20,na.rm = TRUE)/n_distinct(BSysID[!is.na(Spend20)]),
    StatePerElig_01=sum(StateSpend01,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend01)]),
    StatePerElig_02=sum(StateSpend02,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend02)]),
    StatePerElig_03=sum(StateSpend03,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend03)]),
    StatePerElig_04=sum(StateSpend04,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend04)]),
    StatePerElig_05=sum(StateSpend05,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend05)]),
    StatePerElig_06=sum(StateSpend06,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend06)]),
    StatePerElig_07=sum(StateSpend07,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend07)]),
    StatePerElig_08=sum(StateSpend08,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend08)]),
    StatePerElig_09=sum(StateSpend09,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend09)]),
    StatePerElig_10=sum(StateSpend10,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend10)]),
    StatePerElig_11=sum(StateSpend11,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend11)]),
    StatePerElig_12=sum(StateSpend12,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend12)]),
    StatePerElig_13=sum(StateSpend13,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend13)]),
    StatePerElig_14=sum(StateSpend14,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend14)]),
    StatePerElig_15=sum(StateSpend15,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend15)]),
    StatePerElig_16=sum(StateSpend16,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend16)]),
    StatePerElig_17=sum(StateSpend17,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend17)]),
    StatePerElig_18=sum(StateSpend18,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend18)]),
    StatePerElig_19=sum(StateSpend19,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend19)]),
    StatePerElig_20=sum(StateSpend20,na.rm = TRUE)/n_distinct(BSysID[!is.na(StateSpend20)])
    ) %>% 
  group_by(Group) %>% 
  mutate(Date=factor(paste(FiscalYear,FiscalMonth,sep="-"),levels = paste(FiscalYear,FiscalMonth,sep="-"))) %>% 
  ungroup()

FullData<-bind_rows(AllSpendbyGroup,AllSpendbyAll)%>%
  select(FiscalYear,FiscalMonth,Group,contains("02")) %>% 
  group_by(Group) %>% 
  mutate(
    Date=factor(paste(FiscalYear,FiscalMonth,sep="-"),levels = paste(FiscalYear,FiscalMonth,sep="-")),
    three_month=runmean(SpendPerElig_02,3,align = "right",endrule = "NA"),
    six_month=runmean(SpendPerElig_02,6,align = "right",endrule = "NA"),
    CalendarYear=ifelse(FiscalMonth<=6,FiscalYear,FiscalYear+1),
    order=1:n()) %>%
  ungroup()

ForPlot<-add_forcast(FullData,groups = unique(FullData$Group),var_to_cast = "six_month")
# ForPlot$Date<-factor(paste(ForPlot$FiscalYear,ForPlot$FiscalMonth,sep="-"),levels = unique(paste(ForPlot$FiscalYear,ForPlot$FiscalMonth,sep="-")))
ForPlot$CalendarMonth<-ForPlot$FiscalMonth+6
ForPlot$CalendarMonth[ForPlot$CalendarMonth>=13]<-ForPlot$CalendarMonth[ForPlot$CalendarMonth>=13]-12
ForPlot$CalendarYear[is.na(ForPlot$CalendarYear)]<-2018


ForPlot$CalDate<-as.Date(paste(ForPlot$CalendarYear,ForPlot$CalendarMonth,"1",sep="-"))

cols<-c("Actual"="#666666","3-Month MA"="red","6-Month MA"="blue")
shape<-c("Actual"=17,"Forecasted 6-Month MA"=24)

ggplot(ForPlot)+
  geom_line(aes(x=CalDate,y=SpendPerElig_02,group=1,color="Actual"),linetype=3)+
  geom_point(aes(x=CalDate,y=SpendPerElig_02,group=1,shape="Actual"),color="black")+
  geom_line(aes(x=CalDate,y=three_month,group=1,color="3-Month MA"),linetype=2)+
  geom_line(data=subset(ForPlot,FiscalMonth<=12),aes(x=CalDate,y=six_month,group=1,color="6-Month MA"),linetype=2)+
  geom_point(data=subset(ForPlot,FiscalMonth>12),aes(x=CalDate,y=six_month,group=1,shape="Forecasted 6-Month MA",color="6-Month MA"))+
  labs(title="Spending Per Member on Outpatient Hospital",y="Spending per Member ($)",x="Date")+
  scale_x_date(date_labels = "%b-%Y",date_breaks = "3 month")+
  scale_color_manual(name="Interval",values=cols)+
  scale_shape_manual(name="Actual / Forecasted",values = shape)+
  facet_grid(Group~.,scales = "free")

# ggsave(device="jpeg",filename = "~/desktop/Example_Relative.jpg")

ggplot(ForPlot)+
  geom_line(aes(x=Date,y=SpendPerElig_02,group=1),color="black")+
  geom_line(aes(x=Date,y=three_month,group=1),color="red")+
  geom_line(aes(x=Date,y=six_month,group=1),color="blue")+
  labs(title="Spending on Outpatient Hospital per Elig with Three and Six Month Lag by Group",y="Spending per Eligible",x="Date")+
  facet_grid(Group~.)

# ggsave(device="jpeg",filename = "~/desktop/Example_Absolute.jpg")

ggplot(AllSpendbyGroup %>% filter(Group!="Not Defined"))+
  geom_bar(stat = "identity",position="dodge",aes(x=Date,y=SpendPerElig_01,fill=Group))

