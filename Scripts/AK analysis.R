# AK
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(caTools)

enrollment<-read.csv("/volumes/Projects/Alaska DHSS/SpendingPerMemberReporting/FY2017_Enrollment.csv",stringsAsFactors = FALSE)
enrollment$merge<-paste(enrollment$BSysID,enrollment$FiscalYear,enrollment$FiscalMonth,sep="-")

spending<-read.csv("/volumes/Projects/Alaska DHSS/SpendingPerMemberReporting/FY2017_Spending.csv",stringsAsFactors = FALSE)
spending$merge<-paste(spending$BSysID,spending$FiscalYear,spending$FiscalMonth,sep="-")

enroll_spending<-left_join(enrollment,spending %>% select(-BSysID,-FiscalYear,-FiscalMonth),by="merge")

Agg<-enroll_spending %>% 
  filter(ElgClass==30) %>% 
  group_by(FiscalYear,FiscalMonth) %>% 
  summarise(
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
  select(FiscalYear,FiscalMonth,contains("02")) %>% 
  mutate(
    Date=as.Date(paste(FiscalYear,FiscalMonth,1,sep = "-")),
    three_month=runmean(SpendPerElig_02,3,align = "right",endrule = "NA"),
    six_month=runmean(SpendPerElig_02,6,align = "right",endrule = "NA"))

ggplot(Agg)+
  geom_line(aes(x=Date,y=SpendPerElig_02),color="black")+
  geom_line(aes(x=Date,y=three_month),color="red")+
  geom_line(aes(x=Date,y=six_month),color="blue")+
  labs(title="Spending on 02 per Elig Class 30 with Three and Six Month Lag",y="Spending per Eligible",x="Date")

ggsave(device="jpeg",filename = "~/desktop/Example.jpg")

