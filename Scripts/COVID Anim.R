library(dplyr)
library(gganimate)
library(ggplot2)
library(maps)
# library(USAboundaries)


# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

load("~/desktop/check.Rdata")

counties_gis<-urbnmapr::counties %>% 
  mutate(county_fips=as.numeric(county_fips))
states_gis<-urbnmapr::states %>% 
  filter(state_abbv%in%c("OR","WA","ID","MT","CA")) %>%
  filter()

gis_mobcase<-counties_gis %>% 
  left_join(
    mob_cases %>% 
      group_by(FIPS) %>% 
      mutate(
        new_cases=zoo::rollmean(cases-lag(cases),7,na.pad=TRUE,align="right"),
        WFH=zoo::rollmean(workplaces_percent_change_from_baseline,7,na.pad=TRUE,align="right")) %>% 
      left_join(counties,by="FIPS") %>% 
      mutate(p_cases=cases/population,p_new=new_cases/population),
    by=c("county_fips"="FIPS"))


ggplot(
  gis_mobcase %>% filter(date==as.Date("2020-05-01"))
  )+
  geom_polygon(aes(x=long,y=lat,group=group,fill=WFH))+
  theme_void()+
  coord_map()

plot_dat<-gis_mobcase %>% 
  filter(state_abbv%in%c("OR","WA","ID","MT","CA")) %>%
  filter(state_abbv=="OR")

zzz<-ggplot(
  plot_dat %>% 
    # filter(date==as.Date("2020-05-01")) %>%
    filter()
  )+
  geom_polygon(aes(x=long,y=lat,group=group,fill=p_cases),color="black",size=.5)+
  # scale_fill_gradient2(low="blue",mid="white",high="red",midpoint = 0,labels=scales::percent,limits=c(-1,1))+
  scale_fill_gradient2(low="blue",mid="white",high="red",midpoint = 0,labels=scales::percent)+
  # geom_polygon(data=states_gis,aes(x=long,y=lat,group=group),color="black",fill=NA,size=1.5)+
  coord_map()+
  theme_void()+
  theme(plot.title = element_text(hjust = .5))+
  transition_manual(frames=date)+
  ease_aes("linear")+
  labs(title="{current_frame}",fill="% of Population")

animate(zzz,nframes = n_distinct(plot_dat$date),duration = n_distinct(plot_dat$date)/5,end_pause = 10)  
anim_save(filename="p_cases.gif",path="~/desktop/")

yyy<-ggplot(plot_dat)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=workplaces_percent_change_from_baseline),color="black")+
  scale_fill_gradient2(low="blue",mid="white",high="red",midpoint = 0,limits=c(-100,100))+
  coord_map()+
  theme_void()+
  theme(plot.title = element_text(hjust = .5))+
  transition_manual(frames=date)+
  ease_aes("linear")+
  labs(title="{current_frame}")

animate(yyy,nframes = n_distinct(plot_dat$date),duration = n_distinct(plot_dat$date)/5,end_pause = 10) 



