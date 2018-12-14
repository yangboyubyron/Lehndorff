# PPC Regional Charts
library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(maps)

# data

OHCS1<-read.xlsx("/Users/lehndorff/Desktop/Projects/PPC Plots/PPC_map_data_new.xlsx",sheetName = "OHCS_1") %>% mutate(merge=tolower(as.character(as.vector(County))))
OHCS2<-read.xlsx("/Users/lehndorff/Desktop/Projects/PPC Plots/PPC_map_data_new.xlsx",sheetName = "OHCS_2") %>% mutate(merge=tolower(as.character(as.vector(County))))
Schools<-read.xlsx("/Users/lehndorff/Desktop/Projects/PPC Plots/PPC_map_data_new.xlsx",sheetName = "Schools") %>% mutate(merge=gsub(" county","",tolower(as.character(as.vector(County)))))
ETO<-read.xlsx("/Users/lehndorff/Desktop/Projects/PPC Plots/PPC_map_data_new.xlsx",sheetName = "ETO") %>% mutate(merge=tolower(as.character(as.vector(County))))
SDI<-read.xlsx("/Users/lehndorff/Desktop/Projects/PPC Plots/PPC_map_data_new.xlsx",sheetName = "Self_Direct") %>% mutate(merge=tolower(as.character(as.vector(County))))

oregon_counties<-map_data('county') %>% subset(region == 'oregon')

# OHCS 1
OHCS_map1<-left_join(
  oregon_counties,
  OHCS1 %>% select(merge,Number.of.Units.in.County),
  by=c("subregion"="merge"))

n_distinct(OHCS_map1$subregion[!is.na(OHCS_map1$Number.of.Units.in.County)])

ggplot(OHCS_map1)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = OHCS_map1, aes(x = long, y = lat,group=group,fill = Number.of.Units.in.County), color = "black", size = .3)+
  scale_fill_gradient2(low = "#f5e6d6",high="#AB6E29",na.value = "white")+
  labs(fill="Number \n of Units",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.25,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size=6))

# ggsave("/Users/lehndorff/Desktop/Projects/PPC Plots/OHCS_1.jpg",device = "jpeg",width = 6, height = 6)

# OHCS 2
OHCS_map2<-left_join(
  oregon_counties,
  OHCS2 %>% select(merge,ECHO.Units),
  by=c("subregion"="merge"))

n_distinct(OHCS_map2$subregion[!is.na(OHCS_map2$ECHO.Units)])

ggplot(OHCS_map2)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = OHCS_map2, aes(x = long, y = lat,group=group,fill = ECHO.Units), color = "black", size = .3)+
  scale_fill_gradient2(low = "#f5e6d6",high="#AB6E29",na.value = "white")+
  labs(fill="ECHO \nUnits",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.25,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size=6))

# ggsave("/Users/lehndorff/Desktop/Projects/PPC Plots/OHCS_2.jpg",device = "jpeg",width = 6, height = 6)

# ETO
ETO_map<-left_join(
  oregon_counties,
  ETO %>% select(merge,Total,Region),
  by=c("subregion"="merge"))

n_distinct(ETO_map$subregion[!is.na(ETO_map$Total)])

ggplot(ETO_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data=map_data("state") %>% subset(region=="oregon"),aes(x=long,y=lat,group=group),color="black",size=1.5)+
  geom_polygon(data = ETO_map, aes(x = long, y = lat,group=group,fill = Total,color=Total), size = 1)+
  scale_fill_gradient(low = "#e0fbff",high="#00A4BE",na.value = "white")+
  scale_color_gradient(low = "#e0fbff",high="#00A4BE",na.value = "white")+
  labs(fill="Total",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.25,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size=6))

# ggsave("/Users/lehndorff/Desktop/Projects/PPC Plots/ETO.jpg",device = "jpeg",width = 6, height = 6)

# Schools
schools_map<-left_join(
  oregon_counties,
  Schools %>% select(merge,Total),
  by=c("subregion"="merge"))

n_distinct(schools_map$subregion[!is.na(schools_map$Total)])

ggplot(schools_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = schools_map, aes(x = long, y = lat,group=group,fill = Total), color = "black", size = .3)+
  scale_fill_gradient(low = "#cfe7fc",high="#042A4C",na.value = "white")+
  labs(fill="Total \nMeasures",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.25,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size=6))

# ggsave("/Users/lehndorff/Desktop/Projects/PPC Plots/Schools.jpg",device = "jpeg",width = 6, height = 6)

# SDI
SDI_map<-left_join(
  oregon_counties,
  SDI %>% select(merge,Sites),
  by=c("subregion"="merge"))

n_distinct(SDI_map$subregion[!is.na(SDI_map$Sites)])

ggplot(SDI_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = SDI_map, aes(x = long, y = lat,group=group,fill = Sites), color = "black", size = .3)+
  scale_fill_gradient(low = "#deefd1",high="#4A7729",na.value = "white")+
  labs(fill="Sites",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.25,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size=6))

# ggsave("/Users/lehndorff/Desktop/Projects/PPC Plots/SDI.jpg",device = "jpeg",width = 6, height = 6)
