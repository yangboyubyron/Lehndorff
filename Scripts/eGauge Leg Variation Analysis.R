# Packages
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(tdr)
library(xlsx)

# Functions

# Data
test_site<-read.csv("/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/eGauge41400.csv",stringsAsFactors = FALSE)

# Clean timestamp
test_site$timestamp<-as.POSIXct(test_site$Date...Time)

# Calculate Differences
test_site$abs_diff_volts<-test_site$Mains_L1_volts..V.-test_site$Mains_L2_volts..V.
test_site$p_diff_volts<-test_site$abs_diff_volts/test_site$Mains_L1_volts..V.*100

test_site$abs_diff_Mains<-(test_site$Mains_kW..kW.-test_site$Mains_kW_ALT..kW.)*1000
test_site$abs_diff_Mains[test_site$Mains_kW..kW.<.05]<-NA
test_site$p_diff_Mains<-test_site$abs_diff_Mains/test_site$Mains_kW..kW.
test_site$p_diff_Mains[abs(test_site$Mains_kW..kW.)<.001]<-NA

test_site$abs_diff_Furnace<-(test_site$Furnace..kW.-test_site$Furnace_ALT..kW.)*1000
test_site$abs_diff_Furnace[test_site$Furnace..kW.<.05]<-NA
test_site$p_diff_Furnace<-test_site$abs_diff_Furnace/test_site$Furnace..kW./1000*100
test_site$p_diff_Furnace[abs(test_site$Furnace..kW.)<.001]<-NA

test_site$abs_diff_Water_Heater<-(test_site$Water_Heater..kW.-test_site$Water_Heater_ALT..kW.)*1000
test_site$abs_diff_Water_Heater[test_site$Water_Heater..kW.<.05]<-NA
test_site$p_diff_Water_Heater<-test_site$abs_diff_Water_Heater/test_site$Water_Heater..kW./1000*100
test_site$p_diff_Water_Heater[abs(test_site$Water_Heater..kW.)<.001]<-NA

test_site$abs_diff_Well_Pump<-(test_site$Well_Pump..kW.-test_site$Well_Pump_ALT..kW.)*1000
test_site$abs_diff_Well_Pump[test_site$Well_Pump..kW.<.05]<-NA
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
  summarise(min=min(value,na.rm = TRUE),percentile_5=quantile(value,.05,na.rm = TRUE),median=quantile(value,.5,na.rm = TRUE),mean=mean(value,na.rm=TRUE),percentile_95=quantile(value,.95,na.rm = TRUE),max=max(value,na.rm = TRUE))

# write.csv(Summary,"/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Summary_Table_50W.csv",row.names = FALSE)

# Plots
melted_data<-test_site %>% 
  select(timestamp,
    abs_diff_Mains,p_diff_Mains,
    abs_diff_Furnace,p_diff_Furnace,
    abs_diff_Water_Heater,p_diff_Water_Heater,
    abs_diff_Well_Pump,p_diff_Well_Pump) %>% 
  melt(id.vars="timestamp")

ggplot(melted_data %>% filter(grepl("abs_diff",variable)))+
  geom_boxplot(aes(x=variable,y=value),outlier.size = .001)+
  labs(y="Watts",x="Circuit",title=NULL)+
  coord_cartesian(ylim = c(-20,50))+
  scale_x_discrete(labels = c(
    "abs_diff_Mains" = "Mains",
    "abs_diff_Furnace" = "Furnace",
    "abs_diff_Water_Heater" = "Water Heater",
    "abs_diff_Well_Pump" = "Well Pump"))+
  theme_minimal()
# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Watt_Difference_by_EU_50W.jpg",device = "jpeg",width = 6, height = 4)

ggplot(melted_data %>% filter(grepl("p_diff",variable)))+
  geom_boxplot(aes(x=variable,y=value),outlier.size = .001)+
  labs(y="% Difference",x="Circuit",title=NULL)+
  coord_cartesian(ylim = c(-2,2))+
  scale_x_discrete(labels = c(
    "p_diff_Mains" = "Mains",
    "p_diff_Furnace" = "Furnace",
    "p_diff_Water_Heater" = "Water Heater",
    "p_diff_Well_Pump" = "Well Pump"))+
  theme_minimal()

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Percent_Difference_by_EU_50W.jpg",device = "jpeg",width = 6, height = 4)

# Investigate patterns
## bivairiate continuous

ggplot(test_site,aes(x=Furnace..kW.,y=abs_diff_Furnace))+
  geom_rug(size=.01)+
  geom_bin2d(binwidth=c(.004,.08),alpha=1)+
  geom_vline(xintercept =.05,linetype=2,size=.5)+
  theme_minimal()+
  theme(
    legend.text = element_text(size=6),
    legend.title = element_text(size = 8))+
  scale_fill_gradient(high = "dark green",low = "light green")+
  labs(x="Furnace kW",y="Wattage Difference",title=NULL,fill="Count")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Difference_by_Usage_Furnace.jpg",device = "jpeg",width = 6, height = 4)

ggplot(test_site,aes(x=Furnace..kW.,y=p_diff_Furnace))+
  geom_rug(size=.01)+
  geom_bin2d(binwidth=c(.004,.04),alpha=1)+
  geom_vline(xintercept =.05,linetype=2,size=.5)+
  theme_minimal()+
  theme(
    legend.text = element_text(size=6),
    legend.title = element_text(size = 8))+
  scale_fill_gradient(high = "dark green",low = "light green")+
  labs(x="Furnace kW",y="Percent Difference",title=NULL,fill="Count")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Percent_by_Usage_Furnace.jpg",device = "jpeg",width = 6, height = 4)

ggplot(test_site,aes(x=Mains_kW..kW.,y=abs_diff_Mains))+
  geom_rug()+
  geom_bin2d(binwidth=c(.03,.3))+
  labs(x="Mains kW",y="Watt difference",title="Watt diff by Usage")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Difference_by_Usage_Mains.jpg",device = "jpeg")

# zzz<-as.data.frame(
#   round(cor(select(test_site,Mains_kW..kW.,Well_Pump..kW.,Furnace..kW.,Water_Heater..kW.,
#     abs_diff_Mains,p_diff_Mains,
#     abs_diff_Furnace,p_diff_Furnace,
#     abs_diff_Water_Heater,p_diff_Water_Heater,
#     abs_diff_Well_Pump,p_diff_Well_Pump),use = "pairwise.complete.obs"),3))
# 
# zzz$type<-row.names(zzz)
# 
# cor_data<-melt(zzz,id.vars = "type")
# 
# ggplot(cor_data)+
#   geom_tile(aes(y=factor(variable),x=factor(type,levels = levels(factor(variable))),fill=value))+
#   scale_fill_gradient2(low="red",high="green",mid="yellow",midpoint = 0,limits=c(-1,1))+
#   theme(axis.text.x = element_text(angle = 90))+
#   coord_fixed(ratio = 1)

## Difference over Time
ggplot(test_site,aes(x=timestamp,y=p_diff_Mains))+
  geom_point(size=.01)+
  labs(x="Time",y="Mains % Diff",title="Mains % diff over time")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Percent_Difference_Mains_OT.jpg",device = "jpeg")

ggplot(test_site,aes(x=as.factor(hour(timestamp)),y=p_diff_Mains))+
  geom_boxplot(outlier.size = .1)+
  theme_minimal()+
  labs(x="Hour of Day",y="% Difference",title=NULL)+
  scale_y_continuous(limits = c(-2,1))
  # scale_y_continuous(limits = c(0,3))

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Percent_Difference_Mains_by_Hour.jpg",device = "jpeg",height = 4, width = 6)

ggplot(test_site,aes(x=as.factor(hour(timestamp)),y=Mains_kW..kW.))+
  geom_boxplot(outlier.size = .1)+
  theme_minimal()+
  labs(x="Hour of Day",y="Home Home kW Usage",title=NULL)+
  scale_y_continuous(limits = c(0,2))
  # scale_y_continuous(limits = c(0,3))

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Power_Mains_by_Hour.jpg",device = "jpeg",height = 4, width = 6)

ggplot(test_site,aes(x=as.factor(hour(timestamp)),y=p_diff_Furnace))+
  geom_boxplot()+
  labs(x="Hour of Day",y="% Difference",title="Furnace % Diff by Hour")+
  scale_y_continuous(limits = c(-3,1))
  # scale_y_continuous(limits = c(0,3))

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Percent_Difference_Furnace_by_Hour.jpg",device = "jpeg")

# Surprising Patterns
ggplot(test_site,aes(x=abs_diff_Mains,y=abs_diff_Water_Heater))+
  geom_rug()+
  # geom_density_2d()+
  geom_point(size=.01)+
  labs(x="Wattage diff on Mains",y="Wattage diff on Water Heater",title="Contemporaneous Wattage diff between Mains and Water Heater")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Surprising_Pattern_1.jpg",device = "jpeg")

ggplot(test_site,aes(x=p_diff_Mains,y=p_diff_Water_Heater))+
  geom_rug()+
  # geom_density_2d()+
  geom_point(size=.01)+
  labs(x="% diff on Mains",y="% diff on Water Heater",title="Contemporaneous % diff between Mains and Water Heater")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Surprising_Pattern_2.jpg",device = "jpeg")

ggplot(test_site,aes(x=Mains_kW..kW.,y=Water_Heater..kW.))+
  geom_rug()+
  # geom_density_2d()+
  geom_point(size=.01)+
  labs(x="kW on Mains",y="kW on Water Heater",title="Mains Usage v Water Heater Usage")

# ggsave(filename = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Surprising_Pattern_3.jpg",device = "jpeg")

# Error Comparison
error_calc<-function(data,subset){
  data %>% summarise(
    Subset=subset,
    obs=n(),
    Mains_nmbe=tdStats(Mains_kW..kW.,Mains_kW_ALT..kW.,functions="nmbe"),
    Furnace_nmbe=tdStats(Furnace..kW.,Furnace_ALT..kW.,functions="nmbe"),
    Water_Heater_nmbe=tdStats(Water_Heater..kW.,Water_Heater_ALT..kW.,functions="nmbe"),
    Well_Pump_nmbe=tdStats(Well_Pump..kW.,Well_Pump_ALT..kW.,functions="nmbe"),
    Mains_cvrmse=tdStats(Mains_kW..kW.,Mains_kW_ALT..kW.,functions="cvrmse"),
    Furnace_cvrmse=tdStats(Furnace..kW.,Furnace_ALT..kW.,functions="cvrmse"),
    Water_Heater_cvrmse=tdStats(Water_Heater..kW.,Water_Heater_ALT..kW.,functions="cvrmse"),
    Well_Pump_cvrmse=tdStats(Well_Pump..kW.,Well_Pump_ALT..kW.,functions="cvrmse")
  )
}

All<-test_site %>% filter() %>% error_calc(.,subset = "All")
On_Furnace<-test_site %>% filter(Furnace..kW.>=.015) %>% error_calc(.,subset = "Furnace On")
On_WH<-test_site %>% filter(Water_Heater..kW.>=.015) %>% error_calc(.,subset = "Water Heater On")
On_WP<-test_site %>% filter(Well_Pump..kW.>=.015) %>% error_calc(.,subset = "Water Pump On")
High_Main<-test_site %>% filter(Mains_kW..kW.>=quantile(Mains_kW..kW.,.9)) %>% error_calc(.,subset = "High Main")
High_Furnace<-test_site %>% filter(Furnace..kW.>=quantile(Furnace..kW.,.9)) %>% error_calc(.,subset = "High Furnace")
High_WH<-test_site %>% filter(Water_Heater..kW.>=quantile(Water_Heater..kW.,.9)) %>% error_calc(.,subset = "High Water Heater")
High_WP<-test_site %>% filter(Well_Pump..kW.>=quantile(Well_Pump..kW.,.9)) %>% error_calc(.,subset = "High Water Pump")
Peak<-test_site %>% filter(hour(timestamp)>=15&hour(timestamp)<20) %>% error_calc(.,subset = "Peak")
Non_Peak<-test_site %>% filter(!(hour(timestamp)>=15&hour(timestamp)<20)) %>% error_calc(.,subset = "Non-Peak")
After_6pm<-test_site %>% filter(hour(timestamp)>=18) %>% error_calc(.,subset = "After 6 PM")

Error_Summary<-bind_rows(All,On_Furnace,On_WH,On_WP,High_Main,High_Furnace,High_WH,High_WP,Peak,Non_Peak,After_6pm)

# write.xlsx(Error_Summary,file = "/volumes/Projects Berkeley/416034 - NEEA EULR/Analysis/egauge test data from Site/Plots/Error_Summary.xlsx",sheetName = "Errors by Period",row.names = FALSE)

