library(readr)
library(dplyr)
library(ggplot2)
library(ggalluvial)
survey <- read_csv("~/Desktop/NEEA+EULR+Participant+Web+Survey_November+10%2C+2020_17.14.csv")

zzz<-survey %>% 
  select(Q28:Q40) %>% 
  mutate(id=1)

dat=zzz %>% 
  select(id,Q28:Q29) %>% 
  reshape2::melt("id") %>% 
  filter(!is.na(value)) %>% 
  mutate(question=ifelse(variable=="Q28","Before COVID-19","During COVID-19"))

dat$value[dat$value%in%c("Was not working before COVID-19","Am not currently working")]<-"Not working"
dat$value[dat$value%in%c("Always or almost always worked from home","Always or almost always working from home")]<-"Always"
dat$value[dat$value%in%c("Very rarely","Never worked from home")]<-"Rarely or never"
dat$value[dat$value%in%c("About once a week","2 or 3 times a week")]<-"At least once a week"
table(dat$value)

dat$value<-factor(dat$value,levels = c("Not working","Rarely or never","About once a month","At least once a week","Always"))

plot<-ggplot(dat)+
  geom_bar(aes(x=question,fill=value),width = .5)+
  scale_fill_brewer(type="qual",palette = 2)+
  theme_classic()+
  theme(
    panel.grid.major = element_line(color="gray84",size = .25),
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), "cm"),
    legend.position = "bottom"
  )+
  scale_y_continuous(expand = c(0,0))+
  labs(title="How often do you work from home?",x="Timeframe",y="Count of respondents",fill="Response")
ggsave(plot,file="~/desktop/EULR Analysis plan.jpg",device = "jpeg",height = 6,width = 10)


sandat<-zzz %>% 
  select(id,Q28:Q29) %>% 
  group_by(from=Q28,to=Q29)
sandat$from[sandat$from%in%c("Was not working before COVID-19","Am not currently working")]<-"Not working"
sandat$from[sandat$from%in%c("Always or almost always worked from home","Always or almost always working from home")]<-"Always"
sandat$from[sandat$from%in%c("Very rarely","Never worked from home")]<-"Rarely or never"
sandat$from[sandat$from%in%c("About once a week","2 or 3 times a week")]<-"At least once a week"
sandat$to[sandat$to%in%c("Was not working before COVID-19","Am not currently working")]<-"Not working"
sandat$to[sandat$to%in%c("Always or almost always worked from home","Always or almost always working from home")]<-"Always"
sandat$to[sandat$to%in%c("Very rarely","Never worked from home")]<-"Rarely or never"
sandat$to[sandat$to%in%c("About once a week","2 or 3 times a week")]<-"At least once a week"

plot_dat<-sandat %>% 
  filter(!is.na(from)&!is.na(to)) %>% 
  group_by(from,to) %>% 
  summarise(n=n())
plot_dat$from<-factor(plot_dat$from,levels = c("Rarely or never","About once a month","At least once a week","Always","Not working"))
plot_dat$to<-factor(plot_dat$to,levels = c("Rarely or never","About once a month","At least once a week","Always","Not working"))

table(sandat$from)

is_alluvia_form(plot_dat)

ggplot(as.data.frame(plot_dat),
       aes(y = n)) +
  geom_alluvium(width = 1/12,aes(fill=from, axis2 = to, axis1 = from)) +
  geom_stratum(data=plot_dat %>% ungroup(),width = 1/12, color = "black",aes(fill=to,axis1=from,axis2=to)) +
  geom_stratum(data=plot_dat %>% ungroup() %>% select(-to),width = 1/12, color = "black",aes(fill=from,axis2=from)) +
  geom_label(stat = "stratum", aes(axis1=from,axis2=to,label = after_stat(stratum)))+
  labs(y="Count of Responses",title="How often do you work from home?")+
  scale_fill_brewer(type="qual",palette = 6)+
  theme_classic()+
  theme(
    panel.grid.major = element_line(color="gray84",size = .25),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), "cm"),
    legend.position = "bottom"
  )+
  guides(fill=F)+
  scale_x_continuous(breaks = c(1,2),labels = c("Before COVID-19","After COVID-19"),expand = c(.1,.1))+
  scale_y_continuous(expand = c(0,0),breaks = c(0,113))
ggsave(file="~/desktop/survey.jpg",device="jpeg",width = 8, height=6)
