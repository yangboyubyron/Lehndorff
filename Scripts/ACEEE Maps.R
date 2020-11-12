#ACEEE Maps
library(urbnmapr)
library(dplyr)
library(ggplot2)

# st <- read_csv("~/Downloads/st-2.csv")

states_gis<-urbnmapr::states %>% 
  filter(state_abbv%in%c("OR","WA","ID","MT","CA","NV","UT","AZ","WY")) %>%
  filter()

states_gis$study<-"Other"
states_gis$study[states_gis$state_abbv=="CA"]<-"HEUS"
states_gis$study[states_gis$state_abbv%in%c("WA","ID","MT","OR")]<-"EULR"
table(states_gis$study)

ggplot(
  states_gis %>% filter(study=="HEUS")
  )+
  geom_polygon(aes(x=long,y=lat,group=group,fill=study,alpha=study,color=study=="HEUS"))+
  scale_fill_brewer(type="qual",palette = 2)+
  scale_alpha_manual(values = c(.3,1,.1))+
  scale_color_manual(values=c(NA,"black"))+
  theme_void()+
  # coord_map(ylim=c(30,45),xlim=c(-125,-112))+
  coord_map(xlim=c(-125,-110))+
  guides(fill=F,alpha=F,color=F)
ggsave("~/desktop/ACEEE 2020/heus_map.jpg",height = 8,width = 4)

ggplot(
  states_gis
  )+
  geom_polygon(aes(x=long,y=lat,group=group,fill=study,alpha=study,color=study=="EULR"))+
  scale_fill_brewer(type="qual",palette = 2)+
  scale_alpha_manual(values = c(1,.3,.1))+
  scale_color_manual(values=c(NA,"black"))+
  theme_void()+
  # coord_map(ylim = c(35,50),xlim=c(-125,-110))+
  coord_map(xlim=c(-125,-110))+
  guides(fill=F,alpha=F,color=F)
ggsave("~/desktop/ACEEE 2020/eulr_map.jpg",height = 8,width = 4)


