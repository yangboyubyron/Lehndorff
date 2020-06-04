# Code for Analyzing Google Mobility Data
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

mobility<-read.csv("~/desktop/Covid Resources/Global_Mobility_Report.csv",stringsAsFactors = FALSE)
mob_states<-mobility %>%
  filter(country_region=="United States") %>% 
  # filter(sub_region_1%in%c("Oregon","California","Idaho","Montana","Washington")) %>% #relevant states
  mutate(date=as.Date(date)) #format dates

test<-mob_states %>% 
  arrange(date) %>% #order by date
  filter(sub_region_2=="") %>% #restrict to state level data only
  group_by(sub_region_1,sub_region_2) %>% #group by state and county
  mutate(moving_workplaces=zoo::rollmean(workplaces_percent_change_from_baseline,7,na.pad=TRUE)) %>% #calculate a moving average
  mutate(slope=moving_workplaces-lag(moving_workplaces)) #calculate slope from moving average

a_plot<-ggplot(test)+
  geom_point(aes(x=date,y=moving_workplaces,color=sub_region_1))+ #point plot
  geom_vline(xintercept = as.Date("2020-03-01"))+ #vertical line
  # facet_wrap(sub_region_1~.)+
  guides(color=FALSE)
plotly_build(a_plot) #generate plotly

#example of using reshape2 to melt data for ploting
plot_dat<-test %>% 
  filter(sub_region_1=="Idaho") %>%
  select(-country_region_code,-country_region) %>% 
  reshape2::melt(id.vars=c("sub_region_1","sub_region_2","date")) %>% 
  mutate(variable=gsub("_percent_change_from_baseline","",variable))

ggplot(plot_dat)+
  geom_point(aes(x=date,y=value/100,color=sub_region_2))+
  facet_wrap(.~variable,scale="free")+
  theme(strip.text.y = element_text(angle = 0))+
  scale_y_continuous(labels = scales::percent)+
  labs(y="Percent Change from Baseline")+
  guides(color=FALSE)
