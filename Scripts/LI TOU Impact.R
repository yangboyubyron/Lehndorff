library(dplyr)
library(ggplot2)
library(readxl)

EEcolors7<- c("#73B633","#2F2860","#095C9C","#5EBCDF","#C1C1C1","#FABC2B","#BBECCA")

plot_dat<-read_excel("/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/LI TOU Impact.xlsx")
plot_dat$point<-as.numeric(substr(plot_dat$Text,1,(regexpr("±",plot_dat$Text,fixed = TRUE)-2)))
plot_dat$error<-as.numeric(substr(plot_dat$Text,(regexpr("±",plot_dat$Text,fixed = TRUE)+1),100))
plot_dat$Type[plot_dat$Type=="PCT"]<-"Ecobee"
plot_dat$Type[plot_dat$Type=="PCT with Eco+"]<-"Ecobee with eco+"

plot_dat$Type<-factor(plot_dat$Type,levels = c("TOU","Ecobee","Ecobee with eco+"))

chart_impact<-function(data){
  output<-ggplot(data)+
    theme_gray()+
    geom_bar(aes(x=Utility,y=point,fill=Utility),stat = "identity",width = .75)+
    geom_errorbar(aes(x=Utility,ymin=point-error,ymax=point+error),size=0.15,width=.6)+
    facet_grid(.~Type)+
    theme(text=element_text(family="Gill Sans MT"), plot.title=element_text(color='#666633', size=12, hjust = 0.5), legend.position="bottom")+
    labs(y="Impact (kWh)")+
    scale_fill_manual(values = EEcolors7[1:3])
    
  return(output)
}

all_plot<-chart_impact(data=plot_dat %>% filter(Time=="All"))
ggsave(all_plot,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/all_plot.jpg",device="jpeg",width = 6,height=4)

peak_plot<-chart_impact(plot_dat %>% filter(Time=="Peak"))
ggsave(peak_plot,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/peak_plot.jpg",device="jpeg",width = 6,height=4)

chart_impact2<-function(data){
  output<-ggplot(data)+
    theme_gray()+
    geom_bar(aes(x=Type,y=point,fill=Utility),stat = "identity",width = 0)+
    geom_bar(data = data %>% filter(Type=="TOU") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(data = data %>% filter(Type=="PCT") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(data = data %>% filter(Type=="PCT with\nEco+") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(aes(x=Type,y=point,fill=Utility),stat = "identity",width = .75)+
    geom_errorbar(aes(x=Type,ymin=point-error,ymax=point+error),size=0.15,width=.6)+
    facet_grid(.~Utility)+
    theme(text=element_text(family="Gill Sans MT"), plot.title=element_text(color='#666633', size=12, hjust = 0.5), legend.position="bottom")+
    labs(y="Impact (kWh)",x="Program Intervention")+
    scale_fill_manual(values = EEcolors7[1:3])+
    guides(fill=FALSE)
    
  return(output)
}

plot_dat2<-read_excel("/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/LI TOU Impact.xlsx",sheet=2)
plot_dat2$point<-as.numeric(substr(plot_dat2$Text,1,(regexpr("±",plot_dat2$Text,fixed = TRUE)-2)))
plot_dat2$error<-as.numeric(substr(plot_dat2$Text,(regexpr("±",plot_dat2$Text,fixed = TRUE)+1),100))
plot_dat2$error[is.na(plot_dat2$point)]<-NA
plot_dat2$point[is.na(plot_dat2$point)]<-as.numeric(plot_dat2$Text[is.na(plot_dat2$point)])

plot_dat2$Type[plot_dat2$Type=="Total Change"]<-"Total\nChange"
plot_dat2$Type<-factor(plot_dat2$Type,levels = c("TOU","PCT","Total\nChange"))
plot_dat2$Type<-factor(plot_dat2$Type,levels = c("TOU","PCT","Total\nChange"))

all_tc<-chart_impact2(data = plot_dat2 %>% filter(Time=="All"))
ggsave(all_tc,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/all_tc.jpg",device="jpeg",width = 6,height=4)

peak_tc<-chart_impact2(data = plot_dat2 %>% filter(Time=="Peak"))
ggsave(peak_tc,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/peak_tc.jpg",device="jpeg",width = 6,height=4)

plot_dat3<-read_excel("/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/LI TOU Impact.xlsx",sheet=3)
plot_dat3$point<-as.numeric(substr(plot_dat3$Text,1,(regexpr("±",plot_dat3$Text,fixed = TRUE)-2)))
plot_dat3$error<-as.numeric(substr(plot_dat3$Text,(regexpr("±",plot_dat3$Text,fixed = TRUE)+1),100))
plot_dat3$error[is.na(plot_dat3$point)]<-NA
plot_dat3$point[is.na(plot_dat3$point)]<-as.numeric(plot_dat3$Text[is.na(plot_dat3$point)])

plot_dat3$Type[plot_dat3$Type=="PCT with Eco+"]<-"PCT with\nEco+"
plot_dat3$Type[plot_dat3$Type=="Total Change"]<-"Total\nChange"
plot_dat3$Type<-factor(plot_dat3$Type,levels = c("TOU","PCT with\nEco+","Total\nChange"))

all_tc2<-chart_impact2(data = plot_dat3 %>% filter(Time=="All"))
ggsave(all_tc2,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/all_tc2.jpg",device="jpeg",width = 6,height=4)

peak_tc2<-chart_impact2(data = plot_dat3 %>% filter(Time=="Peak"))
ggsave(peak_tc2,file="/Volumes/Projects Berkeley/401037 - PGE LI PCTs/Data - CONFIDENTIAL/HL Plot data/peak_tc2.jpg",device="jpeg",width = 6,height=4)
