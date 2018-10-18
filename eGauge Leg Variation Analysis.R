# Packages
library(dplyr)
library(lubridate)
library(reshape2)


# Functions

# Data
test_site<-read.csv("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/eGauge41400.csv",stringsAsFactors = FALSE)

# Clean timestamp
test_site$timestamp<-as.POSIXct(test_site$Date...Time)

# Calculate Differences
test_site$abs_diff_volts<-test_site$Mains_L1_volts..V.-test_site$Mains_L2_volts..V.
test_site$p_diff_volts<-test_site$abs_diff_volts/test_site$Mains_L1_volts..V.*100

test_site$abs_diff_Mains<-(test_site$Mains_kW..kW.-test_site$Mains_kW_ALT..kW.)*1000
test_site$p_diff_Mains<-test_site$abs_diff_Mains/test_site$Mains_kW..kW.
test_site$p_diff_Mains[abs(test_site$Mains_kW..kW.)<.001]<-NA

test_site$abs_diff_Furnace<-(test_site$Furnace..kW.-test_site$Furnace_ALT..kW.)*1000
test_site$p_diff_Furnace<-test_site$abs_diff_Furnace/test_site$Furnace..kW./1000*100
test_site$p_diff_Furnace[abs(test_site$Furnace..kW.)<.001]<-NA

test_site$abs_diff_Water_Heater<-(test_site$Water_Heater..kW.-test_site$Water_Heater_ALT..kW.)*1000
test_site$p_diff_Water_Heater<-test_site$abs_diff_Water_Heater/test_site$Water_Heater..kW./1000*100
test_site$p_diff_Water_Heater[abs(test_site$Water_Heater..kW.)<.001]<-NA

test_site$abs_diff_Well_Pump<-(test_site$Well_Pump..kW.-test_site$Well_Pump_ALT..kW.)*1000
test_site$p_diff_Well_Pump<-test_site$abs_diff_Well_Pump/test_site$Well_Pump..kW./1000*100
test_site$p_diff_Well_Pump[abs(test_site$Well_Pump..kW.)<.001]<-NA

# Show equality
Summary<-test_site %>% 
  select(timestamp,
    `Mains diff W`=abs_diff_Mains,`Mains diff %`=p_diff_Mains,
    `Furnace diff W`=abs_diff_Furnace,`Furnace diff %`=p_diff_Furnace,
    `Water Heater diff W`=abs_diff_Water_Heater,`Water Heater diff %`=p_diff_Water_Heater,
    `Well Pump diff W`=abs_diff_Well_Pump,`Well Pump diff %`=p_diff_Well_Pump) %>% 
  melt(id.vars="timestamp") %>% 
  group_by(variable) %>% 
  summarise(min=min(value,na.rm = TRUE),percentile_10=quantile(value,.1,na.rm = TRUE),median=quantile(value,.5,na.rm = TRUE),mean=mean(value,na.rm=TRUE),percentile_90=quantile(value,.9,na.rm = TRUE),max=max(value,na.rm = TRUE))

# Plots
melted_data<-test_site %>% 
  select(timestamp,
    abs_diff_Mains,p_diff_Mains,
    abs_diff_Furnace,p_diff_Furnace,
    abs_diff_Water_Heater,p_diff_Water_Heater,
    abs_diff_Well_Pump,p_diff_Well_Pump) %>% 
  melt(id.vars="timestamp")

ggplot(melted_data %>% filter(grepl("abs_diff",variable)))+
  geom_boxplot(aes(x=variable,y=value),outlier.size = .3)+
  labs(y="Watts",x="EU",title="Distribution of Wattage Difference")+
  scale_y_continuous(limits = c(-1,1))

ggplot(melted_data %>% filter(grepl("p_diff",variable)))+
  geom_boxplot(aes(x=variable,y=value),outlier.size = .3)+
  labs(y="% Difference",x="EU",title="Distribution of % Wattage Difference")+
  scale_y_continuous(limits = c(-3,3))

# Investigate patterns
## bivairiate continuous

ggplot(test_site,aes(x=p_diff_Mains,y=p_diff_Water_Heater))+
  geom_point(size=.01)+
  # geom_density2d()+
  geom_rug()
  geom_bin2d(binwidth=c(.2,.06))

zzz<-as.data.frame(
  round(cor(select(test_site,Mains_kW..kW.,Well_Pump..kW.,Furnace..kW.,Water_Heater..kW.,
    abs_diff_Mains,p_diff_Mains,
    abs_diff_Furnace,p_diff_Furnace,
    abs_diff_Water_Heater,p_diff_Water_Heater,
    abs_diff_Well_Pump,p_diff_Well_Pump),use = "pairwise.complete.obs"),3))

zzz$type<-row.names(zzz)

cor_data<-melt(zzz,id.vars = "type")

ggplot(cor_data)+
  geom_tile(aes(y=factor(variable),x=factor(type,levels = levels(factor(variable))),fill=value))+
  scale_fill_gradient2(low="red",high="green",mid="yellow",midpoint = 0,limits=c(-1,1))+
  theme(axis.text.x = element_text(angle = 90))+
  coord_fixed(ratio = 1)

## v time
ggplot(test_site,aes(x=timestamp,y=p_diff_Mains))+
  geom_point(size=.01)

ggplot(test_site,aes(x=as.factor(hour(timestamp)),y=p_diff_Mains))+
  geom_boxplot()+
  scale_y_continuous(limits = c(-3,1))
  # scale_y_continuous(limits = c(0,3))


# Independent comparison
check_list_kW<-c("Mains_kW","Well_Pump","Furnace","Water_Heater")

for (i in check_list_kW){
  print(i)
  
  check_site<-select(test_site,timestamp,L1=paste(i,"..kW.",sep = ""),L2=paste(i,"_ALT..kW.",sep = ""))
  check_site$p_diff<-(check_site$L1-check_site$L2)/check_site$L1
  
  summary(check_site$p_diff[abs(check_site$L1)>=.001]*100)
  summary((check_site$L1-check_site$L2)*1000)
  quantile(check_site$p_diff[abs(check_site$L1)>=.001]*100,probs = c(.1,.9))
  quantile((check_site$L1-check_site$L2)*1000,probs = c(.05,.95))
    
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

p_summary<-NULL
abs_summary<-NULL

for (i in check_list_kW){
  print(i)
  
  check_site<-select(test_site,timestamp,L1=paste(i,"..kW.",sep = ""),L2=paste(i,"_ALT..kW.",sep = ""))
  check_site$p_diff<-(check_site$L1-check_site$L2)/check_site$L1
  
  p_out<-c(EU=i,quantile(check_site$p_diff[abs(check_site$L1)>=.001]*100,probs = c(0,.1,.5,.9,1)),mean=mean(check_site$p_diff[abs(check_site$L1)>=.001]*100))
  p_summary<-bind_rows(p_summary,p_out)
  
  ab_out<-c(EU=i,quantile((check_site$L1-check_site$L2)*1000,probs = c(0,.1,.5,.9,1),mean=mean((check_site$L1-check_site$L2)*1000)))
  abs_summary<-bind_rows(abs_summary,ab_out)

}

zzz<-bind_rows(summary(check_site$p_diff[abs(check_site$L1)>=.001]*100),summary(check_site$p_diff[abs(check_site$L1)>=.001]*100))
