# BPA Intake
library(dplyr)
library(xlsx)

setwd("/volumes/Projects/420002 - BPA Impact Evaluation/TO 1 - residential HVAC analysis/Res DHP Customer survey/Contact Data/")
flathead<-read.csv("FLATHEAD_contact.csv",stringsAsFactors = FALSE)
idfalls<-read.csv("IDAHO FALLS_contact.csv",stringsAsFactors = FALSE)
lincoln<-read.csv("LINCOLN MT_contact.csv",stringsAsFactors = FALSE)
mv<-read.csv("MISSION VALLEY_contact.csv",stringsAsFactors = FALSE)
okanogan<-read.csv("OKANOGAN PUD_contact.csv",stringsAsFactors = FALSE)

for.intake<-bind_rows(flathead,idfalls,lincoln,mv,okanogan) %>% 
  select(CustomerID:line3,Utility) %>% 
  mutate(Response=FALSE)
# write.xlsx(for.intake,"ZZZZ.xlsx",sheetName = "All Contacts",row.names = FALSE) #DO NOT OVERWRITE

## Completed
survey.progress<-readxl::read_xlsx("Survey Intake.xlsx")
response.utility<-survey.progress %>% 
  group_by(Utility) %>% 
  summarise(Total=n(),Responses=sum(Response),Percent=Responses/Total)

# Survey weights
survey_weights<-survey.progress %>% 
  filter(!is.na(Utility)) %>% 
  group_by(Utility) %>% 
  mutate(population=n()) %>% 
  filter(Response) %>% 
  group_by(Utility) %>% 
  mutate(weight=population/n()) %>% 
  select(CustomerID,Utility,weight)
# write.csv(survey_weights,"survey_weights.csv",row.names = FALSE)


## Next round
next.round<-survey.progress %>% 
  filter(!Response) %>% 
  filter(Utility!="OKANOGAN PUD")

for(i in unique(next.round$Utility)){
  out<-next.round %>% filter(Utility==i)
  # write.csv(out)
}

