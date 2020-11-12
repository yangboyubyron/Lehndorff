library(dplyr)
library(ggplot2)
library(readr)

sku_level <- read_csv("/Volumes/Projects JBK/401012-  PG&E HEUS/Data - CONFIDENTIAL/NPD TV Data/Navitas Evergreen Economics SKU level sample.csv",skip = 9)
attribute_level <- read_csv("/Volumes/Projects JBK/401012-  PG&E HEUS/Data - CONFIDENTIAL/NPD TV Data/Navitas Evergreen Economics attribute level sample.csv",skip = 9)
tvs_onsite<-read.csv("/volumes/Projects/401013 - PG&E RBSA/Data/Data - MASTER/3 - Initial Cleaning and QC/SFtv_clean.csv",stringsAsFactors = F)

sku_level<-sku_level %>% 
  filter(!is.na(`Energy Star On Mode Power`))
attribute_level<-attribute_level %>% 
  filter(!is.na(`Energy Star On Mode Power`))

sum(sku_level$`Units Total`)
sum(attribute_level$`Units Total`)

sku_level %>% 
  mutate(ES=grepl("W",`Energy Star On Mode Power`)) %>% 
  group_by(`Time Period(s)`) %>% 
  summarise(avg=weighted.mean(ES,`Units Total`))

attribute_level %>% 
  mutate(ES=grepl("W",`Energy Star On Mode Power`)) %>% 
  group_by(`Display Size`) %>% 
  summarise(total=sum(`Units Total`),avg=weighted.mean(ES,`Units Total`)) %>% 
  filter(total>250000) %>% 
  ggplot()+
  geom_bar(aes(x=`Display Size`,y=avg),stat = "identity")

table(tvs_onsite$TV_Model%in%sku_level$Model)
match_onsite<-tvs_onsite %>% 
  filter(TV_Model_clean%in%sku_level$Model)
match_sku<-sku_level %>% 
  filter(Model%in%tvs_onsite$TV_Model_clean)
