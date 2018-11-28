# PPC Regional Charts
library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(maps)

# data

OHCS<-read.xlsx("/Users/lehndorff/Desktop/PPC_map_data.xlsx",sheetName = "OHCS") %>% mutate(merge=tolower(as.character(as.vector(County))))
Schools<-read.xlsx("/Users/lehndorff/Desktop/PPC_map_data.xlsx",sheetName = "Schools") %>% mutate(merge=tolower(as.character(as.vector(County))))
ETO<-read.xlsx("/Users/lehndorff/Desktop/PPC_map_data.xlsx",sheetName = "ETO") %>% mutate(merge=tolower(as.character(as.vector(County))))
SDI<-read.xlsx("/Users/lehndorff/Desktop/PPC_map_data.xlsx",sheetName = "Self_Direct") %>% mutate(merge=tolower(as.character(as.vector(County))))

oregon_counties<-map_data('county') %>% subset(region == 'oregon')

# OHCS
OHCS_map<-left_join(
  oregon_counties,
  OHCS %>% select(merge,Number.of.Projects),
  by=c("subregion"="merge"))

n_distinct(OHCS_map$subregion[!is.na(OHCS_map$Number.of.Projects)])

ggplot(OHCS_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = OHCS_map, aes(x = long, y = lat,group=group,fill = Number.of.Projects), color = "black", size = .3)+
  scale_fill_gradient2(low = "#f5e6d6",high="#AB6E29",na.value = "white")+
  labs(fill="Number of \nProjects",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.15,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size=6))

# ggsave("~/desktop/PPC Plots/OHCS.jpg",device = "jpeg",width = 6, height = 6)

# ETO
ETO_map<-left_join(
  oregon_counties,
  ETO %>% select(merge,Total),
  by=c("subregion"="merge"))

n_distinct(ETO_map$subregion[!is.na(ETO_map$Total)])

ggplot(ETO_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = ETO_map, aes(x = long, y = lat,group=group,fill = Total), color = "black", size = .3)+
  scale_fill_gradient(low = "#e0fbff",high="#00A4BE",na.value = "white")+
  labs(fill="Total",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.15,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size=6))

# ggsave("~/desktop/PPC Plots/ETO.jpg",device = "jpeg",width = 6, height = 6)

# Schools
schools_map<-left_join(
  oregon_counties,
  Schools %>% select(merge,Count.of.Installed.Measures),
  by=c("subregion"="merge"))

n_distinct(schools_map$subregion[!is.na(schools_map$Count.of.Installed.Measures)])

ggplot(schools_map)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = schools_map, aes(x = long, y = lat,group=group,fill = Count.of.Installed.Measures), color = "black", size = .3)+
  scale_fill_gradient(low = "#cfe7fc",high="#042A4C",na.value = "white")+
  labs(fill="Count of \nInstalled Measures",title=NULL)+
  theme_classic()+
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key.size = unit(.15,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size=6))

# ggsave("~/desktop/PPC Plots/Schools.jpg",device = "jpeg",width = 6, height = 6)

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
    legend.key.size = unit(.15,"in"),
    axis.line = element_blank(),
    text = element_text(size=9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size=6))

# ggsave("~/desktop/PPC Plots/SDI.jpg",device = "jpeg",width = 6, height = 6)
