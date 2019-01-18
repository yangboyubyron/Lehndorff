# ComEd Needs Propensity
library(dplyr)
library(pROC)
library(xlsx)
library(lubridate)
library(ggplot2)
options(scipen = 999)

# Load customer/usage data
# customers<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/ComEd Residential Base 101618.txt",stringsAsFactors = FALSE)
# use_2016<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2016 to 2017 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# use_2017<-read.delim("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/2017 to 2018 Usage for Accounts 101618.txt",stringsAsFactors = FALSE)
# save(list = c("customers","use_2016","use_2017"),file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

load(file="/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/ComEd customer data anon/customers and usage.RData")

# tracking/census
census<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/Latest matrix - estimated ie density by geography.xlsx",sheetName = "Sheet1")
# PRIZM<-read.xlsx("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/PRIZM Premier Master Demographic Spreadsheet External 2017 HL.xlsx",sheetName = "Essentials") %>% mutate(code_merge=as.numeric(Full_Code))
PRIZM<-read.csv("/volumes/Projects/466002 - ComEd Needs Assessment/Confidential Data/Task 1/PrizmDataForTargeting.csv",stringsAsFactors = FALSE)

# within LI, high users - avg monthly, minimum monthly, cooling in summer, heat with elec, summer/winter sholder ratio
# Usage aggregation
use_2016$month<-month(as.Date(paste(as.character(use_2016$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))
use_2017$month<-month(as.Date(paste(as.character(use_2017$MM_ADJ_BILLING_YEARMO),"01"),format="%Y%m%d"))

use_agg<-bind_rows(use_2016,use_2017) %>%
  group_by(ID,month) %>%
  summarise(avg=mean(BILLING_USAGE_QTY,na.rm = TRUE)) %>%
  group_by(ID) %>% 
  summarise(
    M01=max(avg[month==1],-Inf),
    M02=max(avg[month==2],-Inf),
    M03=max(avg[month==3],-Inf),
    M04=max(avg[month==4],-Inf),
    M05=max(avg[month==5],-Inf),
    M06=max(avg[month==6],-Inf),
    M07=max(avg[month==7],-Inf),
    M08=max(avg[month==8],-Inf),
    M09=max(avg[month==9],-Inf),
    M10=max(avg[month==10],-Inf),
    M11=max(avg[month==11],-Inf),
    M12=max(avg[month==12],-Inf))

use_agg[use_agg==-Inf]<-NA
table(is.na(use_agg))

use_agg$min<-apply(use_agg %>% select(-ID),1,FUN=min,na.rm=TRUE)
use_agg$avg<-apply(use_agg %>% select(-ID,-min),1,FUN=mean)
use_agg$S_JA<-apply(use_agg %>% select(M07,M08),1,FUN=mean)
use_agg$W_DJF<-apply(use_agg %>% select(M12,M01,M02),1,FUN=mean)
use_agg$Shoulder<-apply(use_agg %>% select(M03,M04,M05,M10,M11),1,FUN=mean)

# metrics
# use_agg$sm_ratio<-use_agg$S_JJA/use_agg$min
# use_agg$wm_ratio<-use_agg$W_DJF/use_agg$min
# use_agg$sa_ratio<-use_agg$S_JJA/use_agg$avg
# use_agg$wa_ratio<-use_agg$W_DJF/use_agg$avg
# use_agg$am_ratio<-use_agg$avg/use_agg$min
use_agg$ss_ratio<-use_agg$S_JA/use_agg$Shoulder
use_agg$ws_ratio<-use_agg$W_DJF/use_agg$Shoulder
use_agg$sw_ratio<-use_agg$S_JA/use_agg$W_DJF
summary(use_agg)

usable<-use_agg %>% filter((min>0|Shoulder>0)&!is.na(avg)&!is.na(S_JA)&!is.na(W_DJF))
quantile(usable$ws_ratio,probs = c(.1,.25,.5,.75,.9),na.rm = TRUE)

hist(usable$ss_ratio[usable$ss_ratio<quantile(usable$ss_ratio,probs = .99)],breaks = 100)

usable$summer_group<-"NOT DEFINED"
usable$summer_group[usable$ss_ratio>=1.4&usable$summer_group=="NOT DEFINED"]<-"Up"
usable$summer_group[usable$ss_ratio<1.4&usable$ss_ratio>=.9&usable$summer_group=="NOT DEFINED"]<-"Flat"
usable$summer_group[usable$ss_ratio<.9&usable$summer_group=="NOT DEFINED"]<-"Down"
table(usable$summer_group)/nrow(usable)

# https://www.eia.gov/consumption/residential/data/2015/hc/php/hc6.7.php

hist(usable$ws_ratio[usable$ws_ratio<quantile(usable$ws_ratio,probs = .99)],breaks = 100)

usable$winter_group<-"NOT DEFINED"
usable$winter_group[usable$ws_ratio>=1.4&usable$winter_group=="NOT DEFINED"]<-"Up"
usable$winter_group[usable$ws_ratio<1.4&usable$ws_ratio>=.9&usable$winter_group=="NOT DEFINED"]<-"Flat"
usable$winter_group[usable$ws_ratio<.9&usable$winter_group=="NOT DEFINED"]<-"Down"
table(usable$winter_group)/nrow(usable)

# Tech groups
usable$e_cool<-"NOT DEFINED"
usable$e_cool[usable$summer_group=="Up"]<-"CAC"
usable$e_cool[usable$summer_group=="Flat"]<-"Window Unit"
usable$e_cool[usable$summer_group=="Down"]<-"No Cooling"
table(usable$e_cool)

usable$e_heat<-ifelse(usable$winter_group=="Up","Elec Heat","Gas Heat")
table(usable$e_heat)

# usage groups
use_groups<-usable %>% 
  group_by(e_cool,e_heat) %>% 
  mutate(high_use=ifelse(avg>=as.numeric(quantile(avg,probs = .8)),"High","Low"),use_quant=percent_rank(avg))

# ggplot(use_groups %>% mutate(group=paste(e_cool,e_heat)))+
#   geom_point(aes(x=avg,y=high_use),size=.01)+
#   facet_grid(group~.)

test<-use_groups %>% 
  group_by(high_use,e_cool,e_heat) %>% 
  summarise(n=n(),sw=mean(sw_ratio),sd_sw=sd(sw_ratio),avg=mean(avg),Shoulder=mean(Shoulder),summer=mean(S_JA),winter=mean(W_DJF),group=unique(paste(high_use,e_cool,e_heat))) %>%
  group_by(high_use) %>% 
  mutate(p_use=n/sum(n)) %>% 
  arrange(-n)

test2<-use_groups %>% group_by(high_use,e_cool,e_heat) %>% mutate(rand=rank(runif(n())),group=paste(high_use,e_cool,e_heat))

for(i in unique(test$group)){
  print(i)
  plot_dat<-test2 %>% ungroup %>% filter(group==i&rand<9) %>% select(ID,contains("M",ignore.case = FALSE),Shoulder) %>% reshape2::melt(.,id.vars="ID")
  plot<-ggplot(plot_dat %>% filter(variable!="Shoulder"))+
    geom_hline(data = plot_dat %>% filter(variable=="Shoulder"),aes(yintercept=value),color="black")+
    geom_hline(data = plot_dat %>% filter(variable=="Shoulder"),aes(yintercept=value*1.4),color="green",alpha=.5)+
    geom_line(aes(x=as.Date(paste("2018",as.numeric(substr(variable,2,3)),"01",sep="-")),y=value),color="blue")+
    geom_point(aes(x=as.Date(paste("2018",as.numeric(substr(variable,2,3)),"01",sep="-")),y=value),color="black")+
    geom_vline(xintercept=c(as.numeric(as.Date("2018-02-15")),as.numeric(as.Date("2018-06-15")),as.numeric(as.Date("2018-08-15")),as.numeric(as.Date("2018-11-15"))),alpha=.5,color="red")+
    labs(x="Month",y="kWh",title=paste(i,test$n[test$group==i]))+
    facet_grid(ID~.,scales = "free")
  ggsave(plot,filename = paste0("~/desktop/ComEd Plots/",i,".jpg"),width = 7, height = 5)
}

table(use_groups$e_cool)/nrow(use_groups)
table(use_groups$e_heat)/nrow(use_groups)
table(use_groups$high_use)/nrow(use_groups)

# prizm + census for likelood of LI scale of 1-9 -> surveys. 
library(dplyr)
library(jsonlite)

#ACS
#----------------------------------------------------------------------------------------------------
#this requires an ACS API key (https://www.census.gov/data/developers/data-sets/acs-5year.html)
#key is registered to Ross Donaldson (donaldson@evergreen.com)
#S0101_C01_001E = Total!!Estimate!!Total population
#S1901_C01_012E = Households!!Estimate!!Median income (dollars) (S1901)
#S1901_C01_013E = Households!!Estimate!!Mean income (dollars) (S1901)
#S1501_C02_009E = Percent!!Estimate!!Population 25 years and over!!High school graduate (includes equivalency) (S1501)
#S1501_C02_012E = Percent!!Estimate!!Population 25 years and over!!Bachelor's degree (S1501)
#S1501_C02_013E = Percent!!Estimate!!Population 25 years and over!!Graduate or professional degree (S1501)
#S1101_C01_002E = Total!!Estimate!!Average household size (S1101)
#S0801_C01_046E = Total!!Estimate!!TRAVEL TIME TO WORK!!Mean travel time to work (minutes) (S0801)
#S2501_C01_001E = Occupied housing units!!Estimate!!Occupied housing units (S2501)
#S2501_C03_001E = Owner-occupied housing units!!Estimate!!Occupied housing units (S2501)
#S1701_C01_001E	= Estimate!!Total!!Population for whom poverty status is determined	POVERTY STATUS IN THE PAST 12 MONTHS (S1701)

#input state as census numeric code
#IL = 17
acs_pull <- function(state = 17) {
  data <- data.frame(fromJSON(paste("https://api.census.gov/data/2017/acs/acs5/subject?get=NAME,S0101_C01_001E,S1901_C01_002E,S1901_C01_003E,S1901_C01_004E,S1901_C01_005E,S1901_C01_006E,S1901_C01_007E,S1901_C01_008E,S1901_C01_009E,S1901_C01_010E,S1901_C01_011E,S1901_C01_012E,S1901_C01_013E,S1701_C01_038E,S1701_C01_039E,S1701_C01_040E,S1701_C01_041E,S1701_C01_042E,S1701_C01_043E,S1701_C01_044E,S1701_C01_045E,S1701_C01_001E,S1501_C02_009E,S1501_C02_012E,S1501_C02_013E,S1101_C01_002E,S0801_C01_046E,S2501_C01_001E,S2501_C03_001E,S2501_C05_001E&for=tract:*&in=state:", toString(state), "&key=623020359418e43f907eddc1c27bbf7b9814d102", sep = "")))[-1,]
  colnames(data) <- c("Name", "total_population",
    "inc_ls10k","inc_10k15k","inc_15k25k","inc_25k35k","inc_35k50k","inc_50k75k","inc_75k100k","inc_100k150k","inc_150k200k","inc_mr200k","median_income", "mean_income",
    "pov_50p","pov_125p","pov_150p","pov_185p","pov_200p","pov_300p","pov_400p","pov_500p","pov_den",
    "high_school_graduate", "bachelors_degree", "graduate_professional_degree",
    "household_size", "travel_time", "housing_units", "owned", "rented", "state", "county", "tract")
  data[,2:31] <- as.numeric(sapply(data[,2:31],as.vector))
  data <- data %>%
    group_by(Name) %>%
    mutate(owned_percentage = owned / housing_units) %>%
    mutate(rent_percentage = rented / housing_units) %>%
    mutate(CENSUS_TRACT_CODE = paste(state,county,tract,sep = "")) %>% 
    ungroup()
  data
}

census_pull<-acs_pull()

census_pull[census_pull==-666666666.0]<-NA

# census_pull$vlow_inc<-percent_rank(apply(census_pull %>% select(inc_ls10k,inc_10k15k,inc_15k25k),1,sum))
# census_pull$low_inc<-percent_rank(apply(census_pull %>% select(inc_25k35k,inc_35k50k),1,sum))
# census_pull$high_pov<-percent_rank(census_pull$pov_125p/census_pull$pov_den)
# census_pull$med_pov<-percent_rank((census_pull$pov_300p-census_pull$pov_125p)/census_pull$pov_den)
# census_pull$med_inc<-percent_rank(census_pull$median_income)
# census_pull$house_rank<-percent_rank(census_pull$household_size)
# census_pull$pov_score<-apply(census_pull %>% select(vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank),1,mean)
census_pull$vlow_inc<-apply(census_pull %>% select(inc_ls10k,inc_10k15k,inc_15k25k),1,sum)
census_pull$low_inc<-apply(census_pull %>% select(inc_25k35k,inc_35k50k),1,sum)
census_pull$high_pov<-census_pull$pov_125p/census_pull$pov_den
census_pull$med_pov<-(census_pull$pov_300p-census_pull$pov_125p)/census_pull$pov_den
census_pull$med_inc<-census_pull$median_income
census_pull$house_rank<-census_pull$household_size
census_pull$pov_score<-apply(census_pull %>% select(vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank),1,mean)


summary(census_pull)

# census_pull$pov_score<-census_pull$inc_ls10k*10+census_pull$inc_10k15k*8+census_pull$inc_15k25k*6+census_pull$inc_25k35k*5+
#   census_pull$inc_35k50k*4+census_pull$inc_50k75k*3+census_pull$inc_75k100k*2+census_pull$inc_100k150k+
#   (census_pull$pov_50p/census_pull$pov_den+census_pull$pov_125p/census_pull$pov_den+census_pull$pov_150p/census_pull$pov_den+census_pull$pov_185p/census_pull$pov_den+
#       census_pull$pov_200p/census_pull$pov_den+census_pull$pov_300p/census_pull$pov_den+census_pull$pov_400p/census_pull$pov_den)*(census_pull$household_size-1)

pov_score<-left_join(
  census %>% select(geoid,city,county,per_lihh_80) %>% mutate(geoid=as.character(geoid)),
  census_pull %>% select(CENSUS_TRACT_CODE,pov_score,vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank,median_income, total_population),
  by=c("geoid"="CENSUS_TRACT_CODE")
) 

pov_score$li_bin<-ntile(pov_score$per_lihh_80,9)
pov_score$score_bin<-ntile(pov_score$pov_score,9)
table(pov_score$li_bin,pov_score$score_bin)

pov_score$final_score<-apply(pov_score %>% select(li_bin,score_bin),1,FUN = mean)
table(pov_score$final_score)

# usage and LI
use_LI<-use_groups %>% left_join(customers %>% select(ID,LOWINCOME, CENSUS_TRACT_CODE,PRIZM.Code) %>% mutate(CENSUS_TRACT_CODE=as.character(CENSUS_TRACT_CODE)),by="ID") %>% 
  left_join(pov_score %>% select(geoid,per_lihh_80,vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank,median_income),by=c("CENSUS_TRACT_CODE"="geoid")) %>% 
  left_join(PRIZM,by=c("PRIZM.Code"="Code"))

# use_LI_summary<-use_LI %>% group_by(high_use,e_cool,e_heat) %>% summarise(n=n(),min_LI=min(final_score,na.rm = TRUE),median_LI=median(final_score,na.rm = TRUE),max_LI=max(final_score,na.rm = TRUE),t_LI=sum(final_score>=7,na.rm=TRUE),p_LI=sum(final_score>=7,na.rm = TRUE)/n)

cor_table<-cor(use_LI %>% ungroup() %>% mutate(is.li=LOWINCOME=="Yes") %>% select(is.li,per_lihh_80,vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank,HHI...30k,Median.Income..Household.Based.,Average.Income..Neighborhood.Based.,Median.Age.in.Years,College.Grad,Net.Worth),
  use_LI %>% ungroup() %>% mutate(is.li=LOWINCOME=="Yes") %>% select(is.li,per_lihh_80,vlow_inc,low_inc,high_pov,med_pov,med_inc,house_rank,HHI...30k,Median.Income..Household.Based.,Average.Income..Neighborhood.Based.,Median.Age.in.Years,College.Grad,Net.Worth),
  use="complete.obs")

# write.csv(cor_table,file="~/desktop/correlation table.csv",row.names = TRUE)

use_LI$vlow_inc_p<-round(percent_rank(use_LI$vlow_inc),2)
use_LI$high_pov_p<-round(percent_rank(use_LI$high_pov),2)
use_LI$med_inc_p<-round(percent_rank(desc(use_LI$med_inc)),2)
use_LI$Median.Income..Household.Based._p<-round(percent_rank(desc(use_LI$Median.Income..Household.Based.)),2)
use_LI$Net.Worth_p<-round(percent_rank(desc(use_LI$Net.Worth)),2)

use_LI$LI_score<-apply(use_LI %>% ungroup() %>% select(vlow_inc_p,high_pov_p,med_inc_p,Median.Income..Household.Based._p,Net.Worth_p),
  1,FUN = mean,na.rm=TRUE)

summary(use_LI$LI_score[use_LI$LOWINCOME=="Yes"])
summary(use_LI$LI_score[use_LI$LOWINCOME=="No"])

hist(use_LI$LI_score[use_LI$LOWINCOME=="Yes"],breaks = 100)
hist(use_LI$LI_score[use_LI$LOWINCOME=="No"],breaks = 100)
hist(use_LI$LI_score,breaks = 100)

tech_group<-use_LI %>% group_by(e_cool,e_heat) %>% summarise(n=n(),p.with_score=mean(!is.na(LI_score)),p.high_use=mean(high_use=="High"),p.LI=mean(LI_score>=.8,na.rm = TRUE),p.LI_high_use=mean(LI_score>=.8&high_use=="High",na.rm = TRUE))

# write.csv(tech_group,"~/desktop/ComEd Plots/LI by tech group.csv",row.names = FALSE)

li_plot<-ggplot(use_LI %>% mutate(group=paste(high_use,e_cool,e_heat)))+
  geom_histogram(aes(x=LI_score),bins=100)+
  labs(x="LI Likelihood",title="LI Scores for Full Population",y="Count")+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(group~.,scales = "free")

# ggsave(plot=li_plot,filename = "~/desktop/ComEd Plots/High Use by LI.jpg",height = 9,width = 8)

pov_plot<-ggplot(use_LI %>% filter(high_use=="High") %>% mutate(group=paste(high_use,e_cool,e_heat)))+
  geom_histogram(aes(x=LI_score),bins=100)+
  labs(x="Census LI Likelihood Score",title="LI Scores Among High Users",y="Count")+
  theme(strip.text.y = element_text(angle = 0))+
  facet_grid(group~.,scales = "free")

# ggsave(plot=pov_plot,filename = "~/desktop/ComEd Plots/Tech by LI.jpg",height = 9,width = 8)

ComEd.LI<-ggplot(use_LI %>% filter(LOWINCOME=="Yes"))+
  geom_histogram(aes(x=LI_score),bins=100)+
  labs(title="LI Score Among 'LOWINCOME'",x="LI Likelihood",y="Count")

# ggsave(plot = ComEd.LI,filename = "~/desktop/ComEd Plots/LOWINCOME LI Score.jpg",height = 9,width = 8)

ComEd.nLI<-ggplot(use_LI %>% filter(LOWINCOME=="No"))+
  geom_histogram(aes(x=LI_score),bins=100)+
  labs(title="LI Score Among non 'LOWINCOME'",x="LI Likelihood",y="Count")

# ggsave(plot = ComEd.nLI,filename = "~/desktop/ComEd Plots/non-LOWINCOME LI Score.jpg",height = 9,width = 8)

tech_use_by_LI<-ggplot(use_LI)+
  geom_bar(position="fill",aes(x=round(LI_score,1),fill=paste(high_use,e_cool)))+
  labs(title="Cooling and Usage by LI Score",x="LI Likelihood",y="Count",fill="Usage/Cooling Tech")
# ggsave(plot=tech_use_by_LI,filename = "~/desktop/ComEd Plots/Cooling Usage by LI.jpg",height = 9, width = 8)

use_by_LI<-ggplot(use_LI)+
  geom_bar(position="fill",aes(x=round(LI_score,1),fill=high_use))+
  labs(title="Usage by LI Score",x="LI Likelihood",y="Count",fill="Usage")
# ggsave(plot=use_by_LI,filename = "~/desktop/ComEd Plots/Usage by LI.jpg",height = 9, width = 8)

tech_by_LI<-ggplot(use_LI)+
  geom_bar(position="fill",aes(x=round(LI_score,1),fill=e_cool))+
  labs(title="Cooling by LI Score",x="LI Likelihood",y="Count",fill="Cooling Tech")
# ggsave(plot=tech_by_LI,filename = "~/desktop/ComEd Plots/Cooling by LI.jpg",height = 9, width = 8)

heat_agg<-use_LI %>% group_by(LI=round(percent_rank(LI_score),1),Usage=round(percent_rank(avg),1)) %>% summarise(n=n())

ggplot(heat_agg)+
  geom_point(aes(x=LI,y=Usage,color=n/nrow(use_LI)))+
  scale_colour_gradient(low="white",high = "black")

modeling_data<-left_join(customers,agg_2016,by="ID") %>% left_join(agg_2017,"ID") %>% left_join(census,"ID") %>% left_join(PRIZM,by=c("PRIZM.Code"="code_merge"))
table(is.na(modeling_data$Full_Code))


test<-usable %>% select(ID,contains("M",ignore.case = FALSE)) %>% reshape2::melt(.,id.vars="ID") %>% 
  # group_by(ID) %>% mutate(rank=rank(-as.numeric(value)))
  group_by(ID) %>% mutate(rank=value/max(value))

test2<-test %>% group_by(variable) %>% summarise(n=n(),med_rank=median(rank),mean_rank=mean(rank),sd_rank=sd(rank))
