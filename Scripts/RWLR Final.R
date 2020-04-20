# RWLR Final
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(readr)

setwd("/Volumes/Projects/416044 - NEEA RWLR LTMT/Data/CONFIDENTIAL/")

EEcolors7<- c("#73B633","#2F2860","#095C9C","#5EBCDF","#C1C1C1","#FABC2B","#BBECCA")
EEcolors4<-EEcolors7[2:5]

parts_2018<-read_csv("51301_2018-2019_RW_Participant_Data.csv")
parts_2018$Date<-as.Date(parts_2018$Month,format="%m/%d/%y")
parts_2019<-read.csv("Full 2019 Monthly RWLR Dataset.csv")
parts_2019$Date<-as.Date(parts_2019$Month,format="%m/%d/%y")

other<-read_xlsx("2018 Non-Res Lighting Data (including TLEDs).xlsx",sheet = 1)

old.data<- read_excel("/volumes/Projects/416044 - NEEA RWLR LTMT/Admin/Proposal/RFP/Additional Documents/Detailed_2013-2017_Lighting_Sales_Data_Tables.xlsx", 
    sheet = "Data - Machine Readable")

# Participants combined
parts_comb<-parts_2018 %>% 
  filter(year(Date)==2018) %>% 
  select(-contains("Branch"),-contains("ZIP")) %>% 
  bind_rows(
    parts_2019 %>% 
      filter(year(Date)==2019) %>% 
      select(-contains("ZIP"))
  ) %>% 
  filter(Category%in%c("32W","28W","25W","T8LED4ft")) %>% #relevant technonogies
  filter(Ship_State%in%c("OR","WA","MT","ID")) #relevant states
  
MP_parts<-parts_comb %>% 
  group_by(Distributor,Year=year(Date)) %>% 
  summarise(
    Total=sum(Sales),p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total,
    p.RW=(p.28+p.25)/(1-p.TLED),Total_LFL=Total*(1-p.TLED)
    )

DI1_parts<-left_join(
  MP_parts %>% filter(Year==2018) %>% select(Distributor,Total,Total_LFL,p.RW,p.TLED),
  MP_parts %>% filter(Year==2019) %>% select(Distributor,Total,Total_LFL,p.RW,p.TLED),
  by="Distributor",suffix=c(".2018",".2019")
) %>% 
  filter(Distributor!="CED Columbia") %>% 
  mutate(change=p.RW.2019-p.RW.2018,p_change=change/p.RW.2018,type=((abs(p_change)>.1)*sign(p_change)),type2=ceiling(abs(p_change)/.1)*sign(p_change))

weighted.mean(DI1_parts$p.RW.2018,DI1_parts$Total_LFL.2018)
weighted.mean(DI1_parts$p.RW.2019,DI1_parts$Total_LFL.2019)
weighted.mean(DI1_parts$p.TLED.2018,DI1_parts$Total.2018)
weighted.mean(DI1_parts$p.TLED.2019,DI1_parts$Total.2019)

part_resp<-MP_parts %>% 
  filter(Year==2019) %>% 
  mutate(resp=Distributor%in%c("Platt","Portland Lighting","Stoneway","United Lamp and Supply","Pacific Lamp and Supply","Grainger","Graybar","CED Cascade","CED Big Sky","CED Puget Sound"))

all_parts<-MP_parts %>% 
  filter(Distributor!="CED Columbia") %>% 
  group_by(Year) %>% 
  summarise(Total_Bulbs=sum(Total),Total_LFL=sum(Total_LFL),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total)) %>% 
  mutate(RW=(p.28+p.25)/(1-p.TLED))

for_plot<-all_parts %>% select(Year,RW) %>% mutate(Year=as.factor(Year),W32=1-RW) %>% reshape2::melt(.,id.var="Year")
for_plot$variable<-as.character(as.vector(for_plot$variable))
for_plot$variable[for_plot$variable=="W32"]<-"32W"
levels.np<-rev(c("RW"="#5EBCDF","32W"="#639c2a"))

ggplot(for_plot)+
  geom_bar(aes(x=Year,y=value,fill=variable),position = "stack",stat = "identity",width = .5)+
  scale_fill_manual(values = levels.np)+
  scale_y_continuous(labels = scales::percent)+
  labs(fill="Bulb Type",y="LFL Market Penetration")+
  theme(text = element_text(family="Gill Sans",size = 12))
ggsave(file="~/desktop/RW1.jpg",device = "jpeg",height = 4,width = 6.5)

MP_non<-other %>% 
  filter(General_Category!="T5"&Subcategory!="T5 Replacements"&Subcategory!="Other") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty") %>% 
  group_by(Distributor=Company,Year=Sales_Year) %>% 
  summarise(Total=sum(value),p.32=sum(value[Subcategory=="32W"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total,p.TLED=sum(value[General_Category=="LED Tubes"])/Total) %>% 
  group_by(Distributor) %>% 
  mutate(LED_only=p.TLED[Year==max(Year)]==1) %>% 
  filter(!LED_only) %>% 
  filter(Distributor!="Northwest LED Lighting LLC") %>% 
  mutate(RW=(p.25+p.28)/(1-p.TLED))

DI1_np<-MP_non %>% 
  group_by(Year) %>% 
  summarise(Dist=n_distinct(Distributor),Total_bulbs=sum(Total),Total_LFL=sum(Total*(1-p.TLED)),avg_RW=weighted.mean(RW,Total*(1-p.TLED)),avg_tled=weighted.mean(p.TLED,Total))

for_plot<-DI1_np %>% filter(Year>=2015) %>% select(Year,avg_RW) %>% mutate(Year=as.factor(Year),W32=1-avg_RW) %>% reshape2::melt(.,id.var="Year")
for_plot$variable<-as.character(as.vector(for_plot$variable))
for_plot$variable[for_plot$variable=="avg_RW"]<-"RW"
for_plot$variable[for_plot$variable=="W32"]<-"32W"

levels.np<-rev(c("RW"="#5EBCDF","32W"="#639c2a"))

ggplot(for_plot)+
  geom_bar(aes(x=Year,y=value,fill=variable),position = "stack",stat = "identity",width = .5)+
  scale_fill_manual(values = levels.np)+
  scale_y_continuous(labels = scales::percent)+
  labs(fill="Bulb Type",y="LFL Market Penetration")+
  theme(text = element_text(family="Gill Sans",size=12))
ggsave(file="~/desktop/RW2.jpg",device = "jpeg",height = 4,width = 6.5)

respondents<-MP_non %>%
  filter(Distributor%in%c("Pacific Lamp Wholesale, Inc.","HD Supply","KIE Supply Corp."))

# old data
relevant.data1<-old.data %>% 
  filter(`General Category`=="LED Tubes"|`Lighting Technology Type`=="Linear Fluorescent") %>% 
  mutate(T8=ifelse(grepl("T8",`General Category`),"T8",""),Wattage=ifelse(Subcategory%in%c("32W","25W","28W"),Subcategory,"Other"),lamp.group=ifelse(T8=="T8",paste(T8,"-",ifelse(Wattage%in%c("25W","28W"),"Reduced Wattage",Wattage)),`General Category`))
table(relevant.data1$lamp.group)

relevant.data1$lamp.group[relevant.data1$lamp.group%in%c("T12","T5","T8 - Other")]<-"All Other LFL"

MP_old<-relevant.data1 %>% 
  filter(lamp.group!="All Other LFL") %>% 
  group_by(`Sales Year`) %>% 
  summarise(Total=sum(`Sales Qty`),p.32=sum(`Sales Qty`[lamp.group=="T8 - 32W"])/Total,p.RW=sum(`Sales Qty`[lamp.group=="T8 - Reduced Wattage"])/Total,p.TLED=sum(`Sales Qty`[lamp.group=="LED Tubes"])/Total) %>% 
  mutate(lfl.rw=p.RW/(1-p.TLED),lfl_bulbs=Total*(1-p.TLED))

relevant.data<-old.data %>% filter(`Lighting Technology Type`=="Linear Fluorescent") %>% 
  mutate(T8=ifelse(grepl("T8",`General Category`),"T8",""),Wattage=ifelse(Subcategory%in%c("32W","25W","28W"),Subcategory,"Other"),lamp.group=ifelse(T8=="T8",paste(T8,"-",Wattage),`General Category`)) %>% filter(T8=="T8")
table(relevant.data$lamp.group)

rwlr.agg<-relevant.data %>% group_by(State,`Sales Year`,lamp.group) %>% 
  summarise(Total.bulbs=sum(`Sales Qty`))
rwlr.agg$facet_lab<-NA
rwlr.agg$facet_lab[rwlr.agg$State=="ID"]<-"Idaho"
rwlr.agg$facet_lab[rwlr.agg$State=="MT"]<-"Montana"
rwlr.agg$facet_lab[rwlr.agg$State=="OR"]<-"Oregon"
rwlr.agg$facet_lab[rwlr.agg$State=="WA"]<-"Washington"

levels.t8<-rev(c("T8 - 25W"="#095C9C","T8 - 28W"="#5EBCDF","T8 - 32W"="#639c2a","T8 - Other"="#FABC2B"))
t8.plot<-ggplot(rwlr.agg)+
  geom_area(aes(x=`Sales Year`,y=Total.bulbs,fill=factor(lamp.group,levels = names(levels.t8))),stat = "identity",position = "fill")+
  facet_wrap(.~facet_lab,scales = "free",ncol = 2)+
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "gray95"),
    text = element_text(size = 12))+
  scale_fill_manual(values=levels.t8)+
  scale_y_continuous(labels = scales::percent)+
  labs(y="Percent of Market",fill="Lamp Type")
# ggsave(t8.plot,file="/volumes/Projects/416044 - NEEA RWLR LTMT/Admin/Proposal/T8_Plot.jpg",device = "jpeg",width = 7,height=3.8)

# National Chart
for.plot<-data.frame(
  A=all_parts$RW[all_parts$Year==2019],
  B=.3,
  C=DI1_np$avg_RW[DI1_np$Year==2018],
  D=.15
) %>% reshape2::melt()
for.plot$variable<-as.character(for.plot$variable)

for.plot$variable[for.plot$variable=="A"]<-"Regional Participants"
for.plot$variable[for.plot$variable=="B"]<-"National Manufacturer\nRegional Average"
for.plot$variable[for.plot$variable=="C"]<-"Regional Non-participants"
for.plot$variable[for.plot$variable=="D"]<-"National Manufacturer\nNational Average"
for.plot$variable<-factor(for.plot$variable,c("Regional Participants","National Manufacturer\nRegional Average","Regional Non-participants","National Manufacturer\nNational Average"))
for.plot$value<-round(for.plot$value,2)

ggplot(for.plot)+
  geom_bar(aes(x=variable, y=value,fill=variable),stat = "identity")+
  scale_fill_manual(values = EEcolors4)+
  scale_y_continuous(labels = scales::percent)+
  labs(x="RW Market Pentration Source",y="RW Market Penetration")+
  theme(text=element_text(family = "Gill Sans",size=12))+
  guides(fill=FALSE)
ggsave(file="~/desktop/RW3.jpg",device = "jpeg",height = 4,width = 6.5)

zzz<-rwlr.agg %>% 
  group_by(`Sales Year`,RW=lamp.group%in%c("T8 - 25W","T8 - 28W")) %>% 
  summarise(total=sum(Total.bulbs))
