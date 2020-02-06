library(dplyr)
library(lubridate)
library(ggplot2)
library(xlsx)

setwd("/Volumes/Projects/416044 - NEEA RWLR LTMT/Data/CONFIDENTIAL/")

parts<-read.csv("51301_2018-2019_RW_Participant_Data.csv",stringsAsFactors = FALSE)
other<-read.xlsx("51301_2018_Non-Res_Lighting_Survey_(RW_Only).xlsx",sheetIndex = 1)

parts.clean<-parts %>% 
  filter(Category%in%c("32W","28W","25W","T8LED4ft")) %>%
  mutate(date=as.Date(Month,format="%m/%d/%y"),ps=Price/Sales)
parts.clean$Price2<-ifelse(month(parts.clean$date)>=7&year(parts.clean$date)==2019&round(parts.clean$ps,2)==round(parts.clean$ps,10),parts.clean$ps,parts.clean$Price)
summary(parts.clean$Price2)

LS.2018<-parts.clean %>% 
  filter(!is.na(Distributor)) %>% 
  filter(year(date)==2018) %>% 
  group_by(Distributor) %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  summarise(
    Total=sum(Sales),MT=sum(Sales[Ship_State=="MT"]),ID=sum(Sales[Ship_State=="ID"]),OR=sum(Sales[Ship_State=="OR"]),WA=sum(Sales[Ship_State=="WA"]),
    p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total
    )

LS.2019<-parts.clean %>% 
  filter(!is.na(Distributor)) %>% 
  filter(year(date)==2019) %>% 
  group_by(Distributor) %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  summarise(
    Total=sum(Sales),MT=sum(Sales[Ship_State=="MT"]),ID=sum(Sales[Ship_State=="ID"]),OR=sum(Sales[Ship_State=="OR"]),WA=sum(Sales[Ship_State=="WA"]),
    p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total
    )

LS.weights<-full_join(LS.2018,LS.2019,by="Distributor",suffix=c("_2018","_2019"))
# write.csv(LS.weights,"Weights for Participant Analysis.csv",row.names = FALSE)

# Participants
Parts.Market.State<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  group_by(Part="Part",Year=year(date),State=Ship_State) %>% 
  summarise(Total=sum(Sales),p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total)

zzz<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  group_by(Distributor,Year=year(date)) %>% 
  summarise(Total=sum(Sales),p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total)

Parts.Market.Region<-Parts.Market.State %>%
  group_by(Part,Year) %>% 
  summarise(Region.Total=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total))


# Non-participants
NP.Market.State<-other %>% 
  filter(General_Category!="T5") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty") %>% 
  group_by(Part="Non-Part",Year=Sales_Year,State=substr(variable,1,2)) %>% 
  summarise(Total=sum(value),p.32=sum(value[Subcategory=="32W"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total) %>% 
  left_join(
    Parts.Market.State %>% ungroup() %>% select(Year,State,p.TLED)
  )
  
NP.Market.Region<-NP.Market.State %>% 
  group_by(Part,Year) %>% 
  summarise(Region.Total=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total)) 

sort(unique(parts$Distributor))
unique(other$Company)

All.State<-bind_rows(
  Parts.Market.State,
  NP.Market.State %>% 
    filter(Year>=2018) %>% 
    mutate(p.32=p.32*(1-p.TLED),p.28=p.28*(1-p.TLED),p.25=p.25*(1-p.TLED))
) 

All.Region<-All.State %>% group_by(Part,Year) %>% 
  summarise(Region.Total=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total))

Combined.Region<-All.Region %>% filter(Year==2018) %>% ungroup() %>% 
  mutate(weight=ifelse(Part=="Part",.39,.61)) %>% 
  group_by(Part="All",Year) %>% 
  summarise(p.32=weighted.mean(p.32,weight),p.28=weighted.mean(p.28,weight),p.25=weighted.mean(p.25,weight),p.TLED=weighted.mean(p.TLED,weight))

w1<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  filter(year(date)==2019) %>% 
  group_by(State=Ship_State) %>% 
  summarise(weight=sum(Sales)) %>% 
  mutate(weight=weight/sum(weight))

w2<-other %>% 
  filter(General_Category!="T5") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty"&Sales_Year==2018) %>% 
  group_by(State=substr(variable,1,2)) %>% 
  summarise(weight=sum(value)) %>% 
  mutate(weight=weight/sum(weight))

State.Weights<-left_join(w1,w2,by="State",suffix=c(".Part",".NP"))

# write.xlsx(Parts.Market.State %>% data.frame(),sheetName = "Parts by State",file="Data Analysis Results.xlsx",row.names = FALSE)
# write.xlsx(Parts.Market.Region%>% data.frame(),sheetName = "Parts by Region",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(NP.Market.State%>% data.frame(),sheetName ="Non by State",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(NP.Market.Region%>% data.frame(),sheetName ="Non by Region",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(All.State%>% data.frame(),sheetName = "Both by State",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(All.Region%>% data.frame(),sheetName = "Both by Region",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(Combined.Region%>% data.frame(),sheetName = "Combined by Region",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)
# write.xlsx(State.Weights%>% data.frame(),sheetName = "State to Region Weights",file="Data Analysis Results.xlsx",row.names = FALSE,append = TRUE)

# Taylor weights
TT.weights1<-other %>% 
  filter(General_Category!="T5"&Sales_Year==2018) %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  filter(Subcategory%in%c("28W","25W")) %>% 
  group_by(Company) %>% 
  summarise(Total2825=sum(Sales_Qty), p.28_2018=sum(Sales_Qty[Subcategory=="28W"])/Total2825,p.25_2018=sum(Sales_Qty[Subcategory=="25W"])/Total2825)
# write.csv(TT.weights1,"Non-part RW splits.csv",row.names = FALSE)

TT.weights2<-other %>% 
  filter(General_Category!="T5"&Sales_Year==2018) %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  group_by(Company) %>% 
  summarise(MT=sum(MT_Sales),ID=sum(ID_Sales),OR=sum(OR_Sales),WA=sum(WA_Sales))

a<-parts %>% group_by(Distributor,Category) %>% summarise(n=n(),m=mean(Price/Sales))
b<-parts %>% 
  # filter(Distributor=="Grainger") %>% 
  mutate(date=as.Date(Month,format="%m/%d/%y"),ps=Price/Sales)
b$Price2<-ifelse(month(b$date)>=7&year(b$date)==2019&round(b$ps,2)==round(b$ps,10),b$ps,b$Price)
summary(b$Price2)

c<-b %>% group_by(Category,Year=year(date)) %>% summarise(weighted.mean(Price2,Sales))

d<-b %>% filter(year(date)==2018&Category=="25W")

e<-b %>% filter(Category%in%c("32W","28W","25W","T8LED4ft")) %>% 
  group_by(Category,Year=year(date)) %>% summarise(sales=sum(Sales)) %>% group_by(Year) %>% mutate(psales=round(sales/sum(sales),2))

f<-b %>% 
  filter(Category%in%c("32W","28W","25W","T8LED4ft")) %>%
  filter(year(date)==2018) %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID"))

sum(f$Sales[f$Category!="T8LED4ft"])
