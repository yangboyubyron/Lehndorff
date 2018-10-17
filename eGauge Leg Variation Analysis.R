# Packages
library(dplyr)
library(lubridate)


# Functions

# Data
test_site<-read.csv("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/eGauge41400.csv",stringsAsFactors = FALSE)

# Clean timestamp
test_site$timestamp<-as.POSIXct(test_site$Date...Time)

# Independent comparison
check_list_kW<-c("Mains_kW","Well_Pump","Furnace","Water_Heater")

for (i in check_list_kW){
  print(i)
  
  check_site<-select(test_site,timestamp,L1=paste(i,"..kW.",sep = ""),L2=paste(i,"_ALT..kW.",sep = ""))
  check_site$p_diff<-(check_site$L1-check_site$L2)/check_site$L1
  
  ggplot(check_site %>% filter(abs(L1)>=.001))+
    geom_point(aes(x=L1,y=p_diff*100),shape=1)+
    labs(x="L1 kW",y="% diff L1, L2",title=paste(i,"- % diff L1 L2 by L1 kW"))
  
  ggsave(filename = paste("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/",i,"_p_diff_L1.jpg",sep = ""),device = "jpeg")
    
  ggplot(check_site)+
    geom_point(aes(x=L1,y=(L1-L2)*1000),shape=1)+
    labs(x="L1 kW",y="Watt diff L1, L2",title=paste(i,"- abs diff L1 L2 by L1 kW"))
  
  ggsave(filename = paste("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/",i,"_abs_diff_L1.jpg",sep = ""),device = "jpeg")
  
  ggplot(check_site %>% filter(abs(L1)>=.001))+
    geom_point(aes(x=timestamp,y=p_diff*100),shape=1)+
    labs(x="Date",y="% diff L1, L2",title=paste(i,"- % diff L1 L2 over Time"))

  ggsave(filename = paste("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/",i,"_p_diff_date.jpg",sep = ""),device = "jpeg")
  
  ggplot(check_site %>% filter(abs(L1)>=.001))+
    geom_point(aes(x=hour(timestamp),y=p_diff*100),shape=1)+
    labs(x="Hour",y="% diff L1, L2",title=paste(i,"- % diff L1 L2 by Hour"))

  ggsave(filename = paste("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/",i,"_p_diff_hour.jpg",sep = ""),device = "jpeg")


}

plot(check_site$L1,(check_site$p_diff)*100)
plot(check_site$L1,check_site$L2)
