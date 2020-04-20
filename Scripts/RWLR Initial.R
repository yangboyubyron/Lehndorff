library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)

setwd("/Volumes/Projects/416044 - NEEA RWLR LTMT/Data/CONFIDENTIAL/")

parts<-read.csv("51301_2018-2019_RW_Participant_Data.csv",stringsAsFactors = FALSE)
# other.old<-read.xlsx("51301_2018_Non-Res_Lighting_Survey_(RW_Only).xlsx",sheetIndex = 1)
other<-read_xlsx("2018 Non-Res Lighting Data (including TLEDs).xlsx",sheet = 1)

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

DI1_part<-LS.weights %>% 
  mutate(LFL_2018=Total_2018*(1-p.TLED_2018),RW_2018=(p.28_2018+p.25_2018)/(1-p.TLED_2018),RW_2019=(p.28_2019+p.25_2019)/(1-p.TLED_2019)) %>% 
  select(Distributor,LFL_2018,RW_2018,RW_2019)

LS.2018.2<-parts.clean %>% 
  filter(!is.na(Distributor)) %>% 
  filter(year(date)==2018) %>% 
  group_by(Distributor,Ship_State) %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  summarise(
    Total=sum(Sales),MT=sum(Sales[Ship_State=="MT"]),ID=sum(Sales[Ship_State=="ID"]),OR=sum(Sales[Ship_State=="OR"]),WA=sum(Sales[Ship_State=="WA"]),
    p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total
    )

LS.2019.2<-parts.clean %>% 
  filter(!is.na(Distributor)) %>% 
  filter(year(date)==2019) %>% 
  group_by(Distributor,Ship_State) %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  summarise(
    Total=sum(Sales),MT=sum(Sales[Ship_State=="MT"]),ID=sum(Sales[Ship_State=="ID"]),OR=sum(Sales[Ship_State=="OR"]),WA=sum(Sales[Ship_State=="WA"]),
    p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total
    )

LS.weights2<-full_join(LS.2018.2,LS.2019.2,by=c("Distributor","Ship_State"),suffix=c("_2018","_2019"))
# write.csv(LS.weights2,"Weights for Participant Analysis 2.csv",row.names = FALSE)

# Participants
Parts.Market.State<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  group_by(Part="Part",Year=year(date),State=Ship_State) %>% 
  summarise(Total=sum(Sales),p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total)

zzz<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  group_by(Distributor,Year=year(date),Ship_State) %>% 
  summarise(Total=sum(Sales),p.32=sum(Sales[Category=="32W"])/Total,p.28=sum(Sales[Category=="28W"])/Total,p.25=sum(Sales[Category=="25W"])/Total,p.TLED=sum(Sales[Category=="T8LED4ft"])/Total)

Parts.Market.Region<-Parts.Market.State %>%
  group_by(Part,Year) %>% 
  summarise(Region.Total=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total))


# Non-participants
NP.Market.State<-other %>% 
  filter(General_Category!="T5"&Subcategory!="T5 Replacements"&Subcategory!="Other") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty") %>% 
  group_by(Part="Non-Part",Year=Sales_Year,State=substr(variable,1,2)) %>% 
  summarise(Total=sum(value),p.32=sum(value[Subcategory=="32W"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total,p.TLED=sum(value[General_Category=="LED Tubes"])/Total)
  
zzz<-other %>% 
  filter(General_Category!="T5"&Subcategory!="T5 Replacements"&Subcategory!="Other") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty") %>% 
  group_by(Part=Company,Year=Sales_Year) %>% 
  summarise(Total=sum(value),p.32=sum(value[Subcategory=="32W"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total,p.TLED=sum(value[General_Category=="LED Tubes"])/Total)
  
tled_only<-zzz %>% 
  group_by(Part) %>% 
  filter(Year==max(Year)) %>% 
  filter(p.TLED==1)

full_line<-zzz %>% 
  filter(!Part%in%tled_only$Part) %>% 
  group_by(Year) %>% 
  summarise(Bulbs=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total)) %>% 
  mutate(lfl.32=p.32/(1-p.TLED))

zzz<-other %>% 
  filter(General_Category!="T5"&Subcategory!="T5 Replacements") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty") %>% 
  group_by(Company,Year=Sales_Year,State=substr(variable,1,2)) %>% 
  summarise(Total=sum(value),p.32=sum(value[Subcategory=="32W"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total,p.TLED=sum(value[General_Category=="LED Tubes"])/Total)


NP.Market.Region<-NP.Market.State %>% 
  group_by(Part,Year) %>% 
  summarise(Region.Total=sum(Total),p.32=weighted.mean(p.32,Total),p.28=weighted.mean(p.28,Total),p.25=weighted.mean(p.25,Total),p.TLED=weighted.mean(p.TLED,Total)) 

sort(unique(parts$Distributor))
unique(other$Company)

All.State<-bind_rows(
  Parts.Market.State,
  NP.Market.State %>% 
    filter(Year>=2018)
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
  mutate(tled.weight=weight/sum(weight))%>% select(-weight)

w1.2<-parts.clean %>% 
  filter(Ship_State%in%c("OR","WA","MT","ID")) %>% 
  filter(year(date)==2019&Category!="T8LED4ft") %>% 
  group_by(State=Ship_State) %>% 
  summarise(weight=sum(Sales)) %>% 
  mutate(lfl.weight=weight/sum(weight))%>% select(-weight)

w2<-other %>% 
  filter(General_Category!="T5"&Subcategory!="T5 Replacements") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty"&Sales_Year==2018) %>% 
  group_by(State=substr(variable,1,2)) %>% 
  summarise(weight=sum(value)) %>% 
  mutate(tled.weight=weight/sum(weight))%>% select(-weight)

w2.1<-other %>% 
  filter(General_Category!="T5"&General_Category!="LED Tubes") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable!="Sales_Qty"&Sales_Year==2018) %>% 
  group_by(State=substr(variable,1,2)) %>% 
  summarise(weight=sum(value)) %>% 
  mutate(lfl.weight=weight/sum(weight)) %>% select(-weight)

State.Weights<-left_join(w1,w2.1,by="State") %>% 
  left_join(
    left_join(w2,w2.1,by="State"),by="State",suffix=c(".Part",".NP")
  )

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

#missing non-parts to align parts and non-parts
missing.state.weights1<-other %>% 
  filter(General_Category!="T5"&General_Category!="LED Tubes") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  filter(!Company%in%c("Pacific Lamp Wholesale, Inc.","KIE Supply Corp.","Rainier")) %>% 
  filter(Sales_Year==2018) %>% 
  group_by(is.HD=Company=="HD Supply") %>% 
  summarise(ID=sum(ID_Sales),MT=sum(MT_Sales),OR=sum(OR_Sales),WA=sum(WA_Sales))
a<-missing.state.weights1[1,]/missing.state.weights1[2,]

missing.state.weights2<-other %>% 
  filter(General_Category!="T5") %>% 
  filter(!Company%in%c("CED - Big Sky Division","CED - Cascade Division","CED - Columbia Division","CED - Puget Sound Division","Eoff","Interstate",
    "Grainger","Graybar","North Coast Electric","Pacific Lamp & Supply","Platt","Portland Lighting, Inc.","Stoneway","United Lamp Supply")) %>% 
  # filter(Company%in%c("Pacific Lamp Wholesale, Inc.","KIE Supply Corp.","Rainier","HD Supply")) %>% 
  filter(!Company%in%c("Pacific Lamp Wholesale, Inc.","KIE Supply Corp.","Rainier","HD Supply")) %>%
  filter(Sales_Year==2018) %>% 
  select(-Extrapolation_Flag) %>% 
  reshape2::melt(id.vars=c("General_Category", "Dimension", "Company", "Subcategory","Sales_Year")) %>% 
  filter(variable=="Sales_Qty") %>% 
  # group_by(Company) %>% 
  summarise(Part="All Other Non-Parts",Total=sum(value),p.TLED=sum(value[General_Category=="LED Tubes"])/Total,p.28=sum(value[Subcategory=="28W"])/Total,p.25=sum(value[Subcategory=="25W"])/Total,p.32=sum(value[Subcategory=="32W"])/Total)
new.row<-bind_cols(missing.state.weights2,a)
# write.csv(new.row,"TT_new_row.csv",row.names = FALSE)



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
