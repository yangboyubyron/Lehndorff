library(readxl)
library(dplyr)
library(ggplot2)

rwlr.data<- read_excel("/volumes/Projects/416044 - NEEA RWLR LTMT/Admin/Proposal/RFP/Additional Documents/Detailed_2013-2017_Lighting_Sales_Data_Tables.xlsx", 
    sheet = "Data - Machine Readable")

# linear
relevant.data1<-rwlr.data %>% filter(`General Category`=="LED Tubes"|`Lighting Technology Type`=="Linear Fluorescent") %>% 
  mutate(T8=ifelse(grepl("T8",`General Category`),"T8",""),Wattage=ifelse(Subcategory%in%c("32W","25W","28W"),Subcategory,"Other"),lamp.group=ifelse(T8=="T8",paste(T8,"-",ifelse(Wattage%in%c("25W","28W"),"Reduced Wattage",Wattage)),`General Category`))
table(relevant.data1$lamp.group)

relevant.data1$lamp.group[relevant.data1$lamp.group%in%c("T12","T5","T8 - Other")]<-"All Other LFL"

rwlr.agg1<-relevant.data1 %>% group_by(State,`Sales Year`,lamp.group) %>% 
  summarise(Total.bulbs=sum(`Sales Qty`))

levels1<-c("T8 - Reduced Wattage","T8 - 32W","All Other LFL","LED Tubes")
ggplot(rwlr.agg1)+
  geom_bar(aes(x=`Sales Year`,y=Total.bulbs,fill=factor(lamp.group,levels = levels1)),stat = "identity",position = "fill")+
  facet_grid(State~.,scales = "free")+
  labs(y="Bulbs from All Sources",fill="Lamp Type",title = "Linear Lighting Market Share")

ggplot(rwlr.agg1)+
  geom_line(aes(x=`Sales Year`,y=Total.bulbs,color=lamp.group))+
  facet_grid(State~.,scales = "free")+
  labs(y="Bulbs from All Sources",fill="Lamp Type")

# T8
relevant.data<-rwlr.data %>% filter(`Lighting Technology Type`=="Linear Fluorescent") %>% 
  mutate(T8=ifelse(grepl("T8",`General Category`),"T8",""),Wattage=ifelse(Subcategory%in%c("32W","25W","28W"),Subcategory,"Other"),lamp.group=ifelse(T8=="T8",paste(T8,"-",Wattage),`General Category`)) %>% filter(T8=="T8")
table(relevant.data$lamp.group)

rwlr.agg<-relevant.data %>% group_by(State,`Sales Year`,lamp.group) %>% 
  summarise(Total.bulbs=sum(`Sales Qty`))
rwlr.agg$facet_lab<-NA
rwlr.agg$facet_lab[rwlr.agg$State=="ID"]<-"Idaho"
rwlr.agg$facet_lab[rwlr.agg$State=="MT"]<-"Montana"
rwlr.agg$facet_lab[rwlr.agg$State=="OR"]<-"Oregon"
rwlr.agg$facet_lab[rwlr.agg$State=="WA"]<-"Washington"

EEcolors7<- c("#73B633","#2F2860","#095C9C","#5EBCDF","#C1C1C1","#FABC2B","#BBECCA")
EEcolors4<-EEcolors7[2:5]

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

# ggplot(rwlr.agg)+
#   geom_line(aes(x=`Sales Year`,y=Total.bulbs,color=lamp.group))+
#   facet_grid(State~.,scales = "free")+
#   labs(y="Bulbs from All Sources",fill="Lamp Type")

# Market share change %
share.agg<-relevant.data1 %>% group_by(State,`Sales Year`,lamp.group) %>% 
  summarise(Total.bulbs=sum(`Sales Qty`)) %>% group_by(State,`Sales Year`) %>% mutate(share=Total.bulbs/sum(Total.bulbs)) %>%
  group_by(State,lamp.group) %>% mutate(total.change=(Total.bulbs-Total.bulbs[`Sales Year`==2015])/Total.bulbs[`Sales Year`==2015],share.change=share/share[`Sales Year`==2015]-1) %>% 
  filter(`Sales Year`==2017&lamp.group!="All Other LFL")

share.agg$range<-NA
share.agg$range[between(share.agg$total.change,-.8,-.4)]<-"More than 40% Decrease"
share.agg$range[between(share.agg$total.change,-.4,0)]<-"0-40% Decrease"
share.agg$range[between(share.agg$total.change,0,.4)]<-"0-40% Increase"
share.agg$range[between(share.agg$total.change,.4,.8)]<-"41-80% Increase"
share.agg$range[between(share.agg$total.change,.8,2)]<-"81-200% Increase"
share.agg$range[between(share.agg$total.change,2,Inf)]<-"More than 200% Increase"
table(share.agg$range,exclude = NULL)

share.agg$label<-paste0(round(share.agg$total.change,2)*100,"%")

region<-map_data("state",c("washington","oregon","idaho","montana"))
region$join<-NA
region$join[region$region=="montana"]<-"MT"
region$join[region$region=="oregon"]<-"OR"
region$join[region$region=="idaho"]<-"ID"
region$join[region$region=="washington"]<-"WA"
table(region$join,region$region,exclude = NULL)

region.dat<-left_join(region,share.agg,by=c("join"="State"))

colors<-c("More than 40% Decrease"="#9E0022","0-40% Decrease"="#CD6A71","0-40% Increase"="#E6E6E6","41-80% Increase"="#5EBCDF","81-200% Increase"="#005C9C","More than 200% Increase"="#2F2860")
levels2<-names(colors)

map.plot<-ggplot(region.dat)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=factor(range,levels=levels2)),color="black")+
  # facet_wrap(lamp.group~.,ncol=1)+
  facet_wrap(.~lamp.group)+
  labs(fill="% Change\n  in Sales")+
  scale_fill_manual(values = colors)+
  # scale_fill_brewer(type="div",palette = 6)+
  # scale_fill_gradient2(high="dark green",low="blue",mid="white",trans="log",midpoint = 0,
  #   breaks=round(2^seq(-.8,3,.8),4),labels=paste(round(2^seq(-.8,3,.8)*100,0),"%"))+
  ggthemes::theme_map()+
  theme(
    legend.position = "bottom",
    strip.background.y = element_rect(fill="gray70"),
    text = element_text(size = 12))+
  coord_map()
# ggsave(map.plot,file="/volumes/Projects/416044 - NEEA RWLR LTMT/Admin/Proposal/RWLR_map.jpg",device = "jpeg",width = 7, height = 4)
 
