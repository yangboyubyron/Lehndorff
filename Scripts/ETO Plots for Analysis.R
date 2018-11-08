load("/volumes/projects/430011 - ETO Existing Buildings/Data/Data_for_summaries.RData")

library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(knitr)
library(maps)

peneplots<-mget(apropos("penetration"))
mapplots<-mget(c(
  "map_Government",
  "map_Grocery",
  "map_Healthcare",
  "map_Higher Education",
  "map_Hospitality",
  "map_Laundry/Dry Cleaner",
  "map_Office",
  "map_Recreation",
  "map_Religious",
  "map_Repair",
  "map_Restaurant",
  "map_Retail",
  "map_School K-12",
  "map_Warehouse"
))

forheats<-mget(apropos("for_heat"))
sizecounts<-mget(apropos("size_count"))
combfulls<-mget(apropos("CombFull_"))
textfulls<-mget(apropos("TextFull_"))
piedats<-mget(apropos("pie_dat_"))

overviews<-mget(c(
  "overview_Government",
  "overview_Grocery",
  "overview_Healthcare",
  "overview_Higher Education",
  "overview_Hospitality",
  "overview_Laundry/Dry Cleaner",
  "overview_Office",
  "overview_Recreation",
  "overview_Religious",
  "overview_Repair",
  "overview_Restaurant",
  "overview_Retail",
  "overview_School K-12",
  "overview_Warehouse"
))

for (i in 1:14){
  name<-gsub("/","",sectors[i])
  
  print(name)
  
  pene_plot(peneplots[[i]])
  ggsave(paste("~/desktop/ETO Plots/pene_plot_",name,".jpg",sep=""),device = "jpeg")
  
  map_plot(mapplots[[i]])
  ggsave(paste("~/desktop/ETO Plots/map_plot_",name,".jpg",sep=""),device = "jpeg")
  
  freq_plot(for_heat = forheats[[i]],size_count = sizecounts[[i]])
  ggsave(paste("~/desktop/ETO Plots/freq_plot_",name,".jpg",sep=""),device = "jpeg")
  
  comb_plot(CombFull = combfulls[[i]],TextFull = textfulls[[i]],Size_Count = sizecounts[[i]])
  ggsave(paste("~/desktop/ETO Plots/comb_plot_",name,".jpg",sep=""),device = "jpeg",width = 7,height = 5)
  
  pie_plot(piedats[[i]],Size_Count = sizecounts[[i]])
  ggsave(paste("~/desktop/ETO Plots/pie_plot_",name,".jpg",sep=""),device = "jpeg")
  
  write.csv(overviews[[i]],file = paste("~/desktop/ETO Plots/overview_",name,".csv",sep=""))

}

write.csv(Summary_Table,"~/desktop/ETO Plots/Summary_Table.csv")
