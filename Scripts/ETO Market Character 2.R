# ETO MARKET CHARACTERIZATION CHARTS

# packages
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

# functions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#data
population2<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)
counties<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Oregon and SW Washington zip codes.csv",stringsAsFactors = FALSE) %>% group_by(Zip.Code) %>% summarise(County=unique(County))
regions<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ETO Regions.csv",stringsAsFactors = FALSE)
industries<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Employment by Industry.csv",stringsAsFactors = FALSE)
sector_groups<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB market sector categories.csv",stringsAsFactors = FALSE) %>% select(et_marketname,Evergreen.categories)
SEM<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/SEM impact data/SEM and Capital Participation Data to Evergreen.csv",stringsAsFactors = FALSE)

# join sector definitions to population data (existing ETO defintions)
population<-population2 %>% left_join(sector_groups,by=c("market"="et_marketname"))

# Nick's color scheme
EEcolors7<- c("#73B633","#2F2860","#095C9C","#5EBCDF","#C1C1C1","#FABC2B","#BBECCA")
EEcolors4<-EEcolors7[2:5]
EEcolors5<-EEcolors7[1:5]

# assign NAICS group to population (to fill in undefined sites)
population$naics_code_group<-"Unknown"
population$naics_code_group[substr(population$naics_code,1,2)==11]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==21]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==22]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==23]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==42]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,2)==44]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,3)==445]<-"Grocery"
population$naics_code_group[substr(population$naics_code,1,2)==45]<-"Retail"
population$naics_code_group[substr(population$naics_code,1,2)==48]<-"Warehouse"
population$naics_code_group[substr(population$naics_code,1,2)==49]<-"Warehouse"
population$naics_code_group[substr(population$naics_code,1,1)==5]<-"Office"
population$naics_code_group[substr(population$naics_code,1,4)==6111]<-"School K-12"
population$naics_code_group[substr(population$naics_code,1,2)==61&substr(population$naics_code,1,4)!=6111]<-"Higher Education"
population$naics_code_group[substr(population$naics_code,1,2)==71]<-"Recreation"
population$naics_code_group[substr(population$naics_code,1,1)==3]<-"Industrial"
population$naics_code_group[substr(population$naics_code,1,2)==62]<-"Healthcare"
population$naics_code_group[substr(population$naics_code,1,3)==721]<-"Hospitality"
population$naics_code_group[substr(population$naics_code,1,3)==722]<-"Restaurant"
population$naics_code_group[substr(population$naics_code,1,3)==811]<-"Repair"
population$naics_code_group[substr(population$naics_code,1,3)==812]<-"Repair"
population$naics_code_group[substr(population$naics_code,1,3)==813]<-"Religious"
population$naics_code_group[substr(population$naics_code,1,2)==92]<-"Government"
population$naics_code_group[substr(population$naics_code,1,4)==8123]<-"Laundry/Dry Cleaner"
table(population$naics_code_group,exclude = NULL)

# Use our categories first
population$naicsgroup<-population$Evergreen.categories
population$naicsgroup[population$naicsgroup=="Residential -- we'll probably want to exclude this"|is.na(population$naicsgroup)]<-"Unknown"
table(population$naicsgroup,exclude = NULL)

# then use NAICS defined
population$naicsgroup[population$naicsgroup=="Unknown"]<-population$naics_code_group[population$naicsgroup=="Unknown"]
table(population$naicsgroup,exclude = NULL)

# else Assign NAICS by other data
population$naicsgroup[population$naicsgroup=="Unknown"&(population$multifamily==1|population$residential==1)]<-"Multifamily/Residential"
population$naicsgroup[population$naicsgroup=="Unknown"&(population$commercial==1|population$nonresidential==1)]<-"Unknown Commercial"
table(population$naicsgroup)

# define Fuel type
population$fuel_group<-"Unknow Fuel"
population$fuel_group[!is.na(population$kwh2017)&population$kwh2017>0]<-"Electric"
population$fuel_group[!is.na(population$therms2017)&population$therms2017>0]<-"Gas"
population$fuel_group[!is.na(population$kwh2017)&!is.na(population$therms2017)&population$kwh2017>0&population$therms2017>0]<-"Electric and Gas"
table(population$fuel_group)

# Size definitions (determined by ETO/TH)
population$elec_fuel_size<-"Unknown Size"
population$elec_fuel_size[!is.na(population$kwh2017)&population$kwh2017>0]<-"Large"
population$elec_fuel_size[population$kwh2017<500000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Medium"
population$elec_fuel_size[population$kwh2017<50000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Small"

population$gas_fuel_size<-"Unknown Size"
population$gas_fuel_size[!is.na(population$therms2017)&population$therms2017>0]<-"Large"
population$gas_fuel_size[population$therms2017<50000&!is.na(population$therms2017)&population$therms2017>0]<-"Medium"
population$gas_fuel_size[population$therms2017<10000&!is.na(population$therms2017)&population$therms2017>0]<-"Small"

# Size categories (largest of elec and gas size)
population$fuel_comb<-paste(population$elec_fuel_size,population$gas_fuel_size)
population$fuel_size<-"Unknown"
population$fuel_size[grepl("Large",population$fuel_comb)]<-"Large"
population$fuel_size[grepl("Medium",population$fuel_comb)]<-"Medium"
population$fuel_size[grepl("Small",population$fuel_comb)]<-"Small"

table(population$elec_fuel_size,population$fuel_group)
table(population$gas_fuel_size,population$fuel_group)
table(population$fuel_size,population$fuel_group)
table(population$fuel_size,population$naicsgroup)

# assign regions to data
zip_region<-left_join(counties,regions,by="County")
population<-left_join(population,zip_region,by=c("et_zip"="Zip.Code"))
table(population$Regions.for.EB.Process,exclude=NULL)

# define SW Washington region
population<-population %>% group_by(et_city) %>% mutate(Region=ifelse(et_state=="WA","Southwest Washington",first(sort(unique(Regions.for.EB.Process)))))
table(population$Region,exclude = NULL)

test<-subset(population, is.na(Region))

## region order for charts
region_levels<-c("Portland Metro","Northwest Oregon","Central Oregon","Southern Oregon","Eastern Oregon","Southwest Washington")

# Determine participation
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

population$participation<-!(population$et_siteid%in%NonPartCon$et_siteid&population$participanttype=="Non-Participant")

table(population$participation,population$participanttype)

population$participation<-ifelse(population$participation,"Participant","Non-Participant")

# determine site project track based on heirarchy
projects$trackval<-1000
projects$trackval[projects$projecttrackdescription=="SEM Cohort"]<-1
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Custom"]<-2
projects$trackval[projects$projecttrackdescription=="Existing Bldgs - Direct Install"]<-3
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Standard"]<-4
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Lighting"]<-5
table(projects$trackval)

projects$date<-as.Date(projects$installeddate)
projects$date[is.na(projects$date)]<-as.Date(projects$maxrecognizeddate[is.na(projects$date)])
projects$year<-year(projects$date)

projects$confrim_SEM<-projects$projectid%in%subset(SEM,Is.SEM.customer.=="Yes")$ProjectId&projects$trackval==1
table(projects$trackval,projects$confrim_SEM)

projects$trackval[projects$trackval==1&!projects$confrim_SEM]<-1000

Parts<-projects %>% filter(programdescription!="") %>% group_by(et_siteid) %>% arrange(desc(date)) %>% summarise(track=min(trackval),date=max(date))

zzz<-population

population<-zzz %>% left_join(Parts,by="et_siteid")

population$track[is.na(population$track)&population$participation=="Participant"]<-1000
population$track[population$participation=="Non-Participant"]<-"Non-Participant"
population$track[population$track=="1"]<-"SEM"
population$track[population$track=="2"]<-"Custom"
population$track[population$track=="3"]<-"DI"
population$track[population$track=="4"]<-"Standard"
population$track[population$track=="5"]<-"Lighting"
population$track[population$track=="1000"]<-"Other"

table(population$track,population$participation,exclude = NULL)

## track order for charts
track_levels<-c("Non-Participant","SEM","Custom","DI","Standard","Lighting","Other")

# determine participation recency
population$recent_part<-"Non-Participant"
population$recent_part[(population$date<"2017-01-01"&!is.na(population$date))|population$participation=="Participant"]<-"Past Participant"
population$recent_part[population$date>="2017-01-01"&!is.na(population$date)]<-"Recent Participant"

table(population$recent_part,population$participation)

# recency order for charts
recent_levels<-c("Non-Participant","Recent Participant","Past Participant")

# fuel size participation hybrid category
population$fuel_part<-paste(population$fuel_size,population$participation)

# sector/NAICS order for charts
NAICS_levels<-rev(c("Government","Grocery","Healthcare","Hospitality","Industrial","Laundry/Dry Cleaner","Multifamily",
    "Multifamily/Residential", "Office", "Recreation", "Religious","Repair","Restaurant","Retail","School K-12","Higher Education","Warehouse","Unknown Commercial"))

# size order for charts
Size_levels<-rev(c("Unknown","Small","Medium","Large"))

# •	Summary of total commercial building/customers by sector (unadjusted)
ggplot(population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Industrial"))+
  geom_bar(position="identity",aes(x=factor(naicsgroup,levels = NAICS_levels),fill=factor(fuel_size,levels = Size_levels)))+
  scale_fill_manual(values = EEcolors4)+
  facet_grid(participation~.)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank(),axis.text.y = )+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Count of Sites",x="Business Sector",fill="Site Size")

# ggsave("unadj_count.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

unadj_table<-population %>% 
  filter(naicsgroup!="Industrial"&naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(
    `Total Commercial Sites`=n(),
    `Count Participants`=sum(participation=="Participant"),
    `Percent of Participants` = `Count Participants`/sum(population$participation=="Participant"),
    `Count Non-Participants` = sum(participation=="Non-Participant"),
    `Percent of Non-Participants`=`Count Non-Participants`/sum(population$participation=="Non-Participant"),
    `Count of Large Sites`=sum(fuel_size=="Large"),
    `Percent of Large Sites`=`Count of Large Sites`/sum(population$fuel_size=="Large"),
    `Count of Medium Sites`=sum(fuel_size=="Medium"),
    `Percent of Medium Sites`=`Count of Medium Sites`/sum(population$fuel_size=="Medium"),
    `Count of Small Sites`=sum(fuel_size=="Small"),
    `Percent of Small Sites`=`Count of Small Sites`/sum(population$fuel_size=="Small")) %>% 
  data.frame()

# write.xlsx(unadj_table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "UnAdj Table",row.names = FALSE)

# compare with ODE/state data
## assign NAICS group to state data
industries$naicsgroup<-"Unknown Commercial"
industries$naicsgroup[substr(industries$Naics,1,2)==11]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==21]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==22]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==23]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==42]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,2)==44]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,3)==445]<-"Grocery"
industries$naicsgroup[substr(industries$Naics,1,2)==45]<-"Retail"
industries$naicsgroup[substr(industries$Naics,1,2)==48]<-"Warehouse"
industries$naicsgroup[substr(industries$Naics,1,2)==49]<-"Warehouse"
industries$naicsgroup[substr(industries$Naics,1,1)==5]<-"Office"
industries$naicsgroup[substr(industries$Naics,1,4)==6111]<-"School K-12"
industries$naicsgroup[substr(industries$Naics,1,2)==61&substr(industries$Naics,1,4)!=6111]<-"Higher Education"
industries$naicsgroup[substr(industries$Naics,1,2)==71]<-"Recreation"
industries$naicsgroup[substr(industries$Naics,1,1)==3]<-"Industrial"
industries$naicsgroup[substr(industries$Naics,1,2)==62]<-"Healthcare"
industries$naicsgroup[substr(industries$Naics,1,3)==721]<-"Hospitality"
industries$naicsgroup[substr(industries$Naics,1,3)==722]<-"Restaurant"
industries$naicsgroup[substr(industries$Naics,1,3)==811]<-"Repair"
industries$naicsgroup[substr(industries$Naics,1,3)==812]<-"Repair"
industries$naicsgroup[substr(industries$Naics,1,3)==813]<-"Religious"
industries$naicsgroup[substr(industries$Naics,1,2)==92]<-"Government"
industries$naicsgroup[substr(industries$Naics,1,4)==8123]<-"Laundry/Dry Cleaner"
table(industries$naicsgroup,exclude = NULL)

# calculate state adjustments
pop_by_naics<-population %>% group_by(naicsgroup) %>% summarise(in_eto=n(),parts=sum(participation=="Participant"))
IndAgg<-industries %>% group_by(naicsgroup) %>% summarise(Units=sum(as.numeric(gsub(",","",Units))))
State_adj<-full_join(pop_by_naics,IndAgg,by="naicsgroup") %>% mutate(adj=(Units-parts)/(in_eto-parts))
State_adj$adj[is.na(State_adj$adj)]<-1
State_adj$adj[State_adj$naicsgroup!="Unknown Commercial"&State_adj$adj<1]<-1

# adjusted plots

## count by sector
counts_adj<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(fuel_size,naicsgroup,participation) %>% 
  summarise(n=n()) %>%
  left_join(State_adj,by="naicsgroup") %>% 
  ungroup() %>%
  mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n))

counts_adj$fuel_part<-ifelse(counts_adj$participation=="Participant",counts_adj$fuel_size,"Non-Participant")

## fuel part levels for charts
fuel_part_levels<-c("Non-Participant","Large","Medium","Small","Unknown")

## count by track
counts_adj_track<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(track,naicsgroup,participation) %>% 
  summarise(n=n()) %>% 
  left_join(State_adj,by="naicsgroup") %>% 
  ungroup() %>% 
  mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n))

counts_adj_track$fuel_part<-ifelse(counts_adj_track$participation=="Participant",counts_adj_track$track,"Non-Participant")

## count by region
counts_reg<-population %>% filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Industrial") %>%
  group_by(track,Region,participation) %>% 
  summarise(count=n()) %>% 
  mutate(fuel_part=ifelse(participation=="Participant",track,"Non-Participant"))

##charts
#•	Summary of total commercial sites/customers by sector (adjusted)
ggplot(counts_adj %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity", aes(x=participation,y=count_adj,fill=factor(fuel_part,levels = fuel_part_levels)))+
  scale_fill_manual(
    values = EEcolors5,
    breaks = fuel_part_levels,
    labels = c("Non-Participant","Large Participant","Medium Participant","Small Participant","Unknown Participant"))+
  facet_grid(naicsgroup~.,switch = "y")+
  coord_flip()+
  theme_minimal()+
  theme(
    text = element_text(family = "Helvetica",size=10),
    panel.grid.major.y = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 180,hjust = 1),
    axis.text.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Adjusted Count of Sites",x="Business Sector",fill="Site Size")

# ggsave("adj_count.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)


#•	Proportion of program participation by market sector - # of sites
ggplot(counts_adj %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",position = "fill",aes(x=factor(naicsgroup,levels = NAICS_levels),y=count_adj,fill=factor(fuel_part,levels = fuel_part_levels)))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(y="Proportion of Sites",x="Business Sector",fill="Site Size / Participation")

# ggsave("adj_count_prop.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

# by sector and track %
# •	Proportion of program participation by market sector and program track 
ggplot(counts_adj_track %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",position = "fill",aes(x=factor(naicsgroup,levels = NAICS_levels),y=count_adj,fill=factor(track,levels = track_levels)))+
  scale_fill_manual(values = EEcolors7)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(y="Proportion of Sites",x="Business Sector",fill="Program Track")

# ggsave("track_count_prop.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

# by region and track %
# •	Proportion of program participation by region and program track 
ggplot(counts_reg %>% filter(!is.na(Region)) %>% ungroup())+
  geom_bar(stat="identity",position = "fill",aes(x=factor(gsub(" ","\n",Region),rev(gsub(" ","\n",region_levels))),y=count,fill=factor(track,levels = track_levels)))+
  scale_fill_manual(values = EEcolors7)+
  scale_x_discrete(
    labels = c(
      "Portland\nMetro" = "Portland\nMetro\n(n = 73,078)",
      "Northwest\nOregon" = "Northwest\nOregon\n(n = 31,970)",
      "Central\nOregon" = "Central\nOregon\n(n = 10,516)",
      "Southern\nOregon" = "Southern\nOregon\n(n = 23,509)",
      "Eastern\nOregon" = "Eastern\nOregon\n(n = 7,812)",
      "Southwest\nWashington" = "Southwest\nWashington\n(n = 6,282)")
  )+
  coord_flip(ylim = c(0,.25))+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(x="Region",y="Proportion of Sites",fill="Program Track")
  
# ggsave("region_count_prop.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

# by sector and participant date
# •	stacked adj count sector / participation date
counts_adj_part<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(recent_part,naicsgroup) %>% 
  summarise(n=n()) %>% 
  left_join(State_adj,by="naicsgroup") %>%
  ungroup() %>% 
  mutate(count_adj=ifelse(recent_part=="Non-Participant",round(n*adj),n)) %>% 
  group_by(naicsgroup) %>% 
  arrange(desc(recent_part)) %>% 
  mutate(text=cumsum(count_adj)+4000)

# fine-tune chart text location
counts_adj_part$text[counts_adj_part$text>40000]<-counts_adj_part$text[counts_adj_part$text>40000]-8500
counts_adj_part$text[counts_adj_part$recent_part!="Non-Participant"]<-NA

ggplot(counts_adj_part %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",aes(x=factor(naicsgroup,levels = NAICS_levels),y=count_adj,fill=factor(recent_part,levels = recent_levels)))+
  geom_text(aes(x=factor(naicsgroup,levels = NAICS_levels),y=text,label=format(count_adj,big.mark = ",",format="d")))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Adjusted Count of Sites",x="Business Sector",fill="Participation Status")

# ggsave("adj_count_recent.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

## sector part date count table
SPDC_Table<-counts_adj_part %>% 
  filter(naicsgroup!="Industrial") %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(Sites=sum(count_adj),
    `% Sites`=Sites/sum(counts_adj_part$count_adj),
    Recent=sum(count_adj[recent_part=="Recent Participant"]),
    Past=sum(count_adj[recent_part=="Past Participant"]),
    `Non-Participant`=sum(count_adj[recent_part=="Non-Participant"])) %>% 
  data.frame()

# write.xlsx(SPDC_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Count Sector Date Table",append = TRUE,row.names = FALSE)

# by Region and participant date
# •	stacked adj count Region / participation date
counts_adj_part_region<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Industrial") %>% 
  filter(!is.na(Region)) %>% 
  group_by(recent_part,Region) %>% 
  summarise(n=n()) %>% 
  group_by(Region) %>% 
  arrange(desc(recent_part)) %>% 
  mutate(text=cumsum(n)+6500)

# fine tune chart text location
counts_adj_part_region$text[counts_adj_part_region$text>40000]<-counts_adj_part_region$text[counts_adj_part_region$text>40000]-12500
counts_adj_part_region$text[counts_adj_part_region$recent_part!="Non-Participant"]<-NA

ggplot(counts_adj_part_region %>% ungroup())+
  geom_bar(stat="identity",aes(x=factor(Region,levels = rev(region_levels)),y=n,fill=factor(recent_part,levels = recent_levels)))+
  geom_text(aes(x=factor(Region,levels = region_levels),y=text,label=format(n,big.mark = ",",format="d")))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Adjusted Count of Sites",x="Business Region",fill="Participation Status")

# ggsave("adj_count_recent_region.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

## region part date count table
RPDC_Table<-counts_adj_part_region %>% 
  group_by(Region) %>% 
  summarise(Sites=sum(n),
    `% Sites`=Sites/sum(counts_adj_part_region$n),
    Recent=sum(n[recent_part=="Recent Participant"]),
    Past=sum(n[recent_part=="Past Participant"]),
    `Non-Participant`=sum(n[recent_part=="Non-Participant"])) %>% 
  data.frame()

# write.xlsx(RPDC_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Count Region Date Table",append = TRUE,row.names = FALSE)

# by Size and participant date
# •	stacked adj count Size / participation date
counts_adj_part_size<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential"&naicsgroup!="Industrial") %>% 
  group_by(recent_part,fuel_size) %>% 
  summarise(n=n()) %>% 
  group_by(fuel_size) %>% 
  arrange(desc(recent_part)) %>% 
  mutate(text=cumsum(n)+8000)

# fine tune chart text location
counts_adj_part_size$text[counts_adj_part_size$text>40000]<-counts_adj_part_size$text[counts_adj_part_size$text>40000]-20000
counts_adj_part_size$text[counts_adj_part_size$recent_part!="Non-Participant"]<-NA

ggplot(counts_adj_part_size %>% ungroup())+
  geom_bar(stat="identity",aes(x=factor(fuel_size,levels = rev(Size_levels)),y=n,fill=factor(recent_part,levels = recent_levels)))+
  geom_text(aes(x=factor(fuel_size,levels = rev(Size_levels)),y=text,label=format(n,big.mark = ",",format="d")))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Adjusted Count of Sites",x="Business Size",fill="Participation Status")

# ggsave("adj_count_recent_size.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

## size part date count table
SiPDC_Table<-counts_adj_part_size %>% 
  group_by(Size=fuel_size) %>% 
  summarise(Sites=sum(n),
    `% Sites`=Sites/sum(counts_adj_part_size$n),
    Recent=sum(n[recent_part=="Recent Participant"]),
    Past=sum(n[recent_part=="Past Participant"]),
    `Non-Participant`=sum(n[recent_part=="Non-Participant"])) %>% 
  data.frame()

# write.xlsx(SiPDC_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Count Size Date Table",append = TRUE,row.names = FALSE)

# adjusted table
adj_table<-counts_adj %>% 
  filter(naicsgroup!="Industrial"&naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(
    `Total Commercial Sites`=sum(count_adj),
    `Count Participants`=sum(count_adj[participation=="Participant"]),
    `Percent of Participants` = `Count Participants`/sum(counts_adj$count_adj[participation=="Participant"]),
    `Count Non-Participants` = sum(count_adj[participation=="Non-Participant"]),
    `Percent of Non-Participants`=`Count Non-Participants`/sum(counts_adj$count_adj[participation=="Non-Participant"]),
    `Count of Large Sites`=sum(count_adj[fuel_size=="Large"]),
    `Percent of Large Sites`=`Count of Large Sites`/sum(counts_adj$count_adj[fuel_size=="Large"]),
    `Count of Medium Sites`=sum(count_adj[fuel_size=="Medium"]),
    `Percent of Medium Sites`=`Count of Medium Sites`/sum(counts_adj$count_adj[fuel_size=="Medium"]),
    `Count of Small Sites`=sum(count_adj[fuel_size=="Small"]),
    `Percent of Small Sites`=`Count of Small Sites`/sum(counts_adj$count_adj[fuel_size=="Small"])) %>% 
  data.frame()

# write.xlsx(adj_table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Table",append = TRUE,row.names = FALSE)

# alternate size participation levels for charts
fuel_part_levels2<-c("Non-Participant","Large Participant","Medium Participant","Small Participant","Unknown Size Participant")

## kwh by sector
characterization_adj_kwh<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(elec_fuel_size,naicsgroup,participation) %>% summarise(n=n(),kwh=sum(kwh2017,na.rm = TRUE)) %>% 
  left_join(State_adj,by="naicsgroup") %>%
  ungroup() %>% 
  mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n),kwh_adj=ifelse(participation=="Non-Participant",kwh*adj,kwh),fuel_part=ifelse(participation=="Non-Participant","Non-Participant",paste(elec_fuel_size,participation))) %>% 
  group_by(naicsgroup,fuel_part) %>% 
  arrange(naicsgroup,desc(fuel_part)) %>% 
  summarise(kwh_adj=sum(kwh_adj),count_adj=sum(count_adj)) %>% 
  group_by(naicsgroup) %>% arrange(desc(factor(fuel_part,levels = fuel_part_levels2))) %>% 
  mutate(text=cumsum(kwh_adj)+1.5e8)

## kwh by region
characterization_reg_kwh<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(elec_fuel_size,Region,participation) %>% 
  summarise(n=n(),kwh=sum(as.numeric(kwh2017),na.rm = TRUE)) %>% 
  mutate(fuel_part=ifelse(participation=="Non-Participant","Non-Participant",paste(elec_fuel_size,participation))) #%>% 

## kwh by recency
characterization_recent_kwh<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(recent_part,naicsgroup,participation) %>% summarise(n=n(),kwh=sum(as.numeric(kwh2017),na.rm = TRUE)) %>% 
  left_join(State_adj,by="naicsgroup") %>%
  ungroup() %>% 
  mutate(count_adj=ifelse(recent_part=="Non-Participant",round(n*adj),n),kwh_adj=ifelse(recent_part=="Non-Participant",kwh*adj,kwh)) %>% 
  group_by(naicsgroup,recent_part) %>% 
  arrange(naicsgroup,desc(recent_part)) %>% 
  summarise(kwh_adj=sum(kwh_adj),count_adj=sum(count_adj)) %>% 
  group_by(naicsgroup) %>% 
  arrange(desc(factor(recent_part,levels = recent_levels))) %>% 
  mutate(text=cumsum(kwh_adj)+1e8)

# format chart text
characterization_recent_kwh$count_adj<-format(characterization_recent_kwh$count_adj,big.mark = ",",format="d")
characterization_recent_kwh$count_adj[characterization_recent_kwh$recent_part!="Non-Participant"]<-""

characterization_recent_kwh$label<-format(round(characterization_recent_kwh$kwh_adj/1e6,0),big.mark = ",",format="d")
characterization_recent_kwh$label[characterization_recent_kwh$recent_part!="Non-Participant"]<-""

characterization_recent_kwh$text[characterization_recent_kwh$kwh_adj>600000000&characterization_recent_kwh$count_adj!=""]<-
  characterization_recent_kwh$text[characterization_recent_kwh$kwh_adj>600000000&characterization_recent_kwh$count_adj!=""]-3.5e8

# •	Summary of commercial customers by kWh usage
ggplot(characterization_recent_kwh %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",aes(x=factor(naicsgroup,levels=NAICS_levels),y=kwh_adj/1e6,fill=factor(recent_part,recent_levels)))+
  scale_fill_manual(values = EEcolors5,labels=c("Non-Participant","Recent Participant","Past Participant"))+
  geom_text(aes(x=factor(naicsgroup,levels=NAICS_levels),y=text/1e6,label=label))+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  labs(y="Annual GWh Usage",x="Business Sector",fill="Participation Status")

# ggsave("adj_kwh.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

## sector part date kWh table
SPDW_Table<-characterization_recent_kwh %>% 
  filter(naicsgroup!="Industrial") %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(`Total GWh`=sum(kwh_adj/1e6),
    `% GWh`=`Total GWh`/sum(characterization_recent_kwh$kwh_adj/1e6),
    Recent=sum(kwh_adj[recent_part=="Recent Participant"]/1e6),
    Past=sum(kwh_adj[recent_part=="Past Participant"]/1e6),
    `Non-Participant`=sum(kwh_adj[recent_part=="Non-Participant"]/1e6)) %>% 
  data.frame()

# write.xlsx(SPDW_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj GWh Sector Date Table",append = TRUE,row.names = FALSE)

# •	Proportion of program participation by market sector - kWh usage
ggplot(characterization_adj_kwh %>% filter(fuel_part!="Unknown Size Participant"&naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",position="fill",aes(x=factor(naicsgroup,levels=NAICS_levels),y=kwh_adj,fill=factor(fuel_part,fuel_part_levels2)))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(y="Proportion of kWh Usage",x="Business Sector",fill="Site Size")

# ggsave("adj_kwh_prop.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

# sector proportion kWh table
SPWT_Table <- characterization_adj_kwh %>% filter(fuel_part!="Unknown Size Participant"&naicsgroup!="Industrial") %>% 
  group_by(sector=naicsgroup) %>% 
  summarise(
    `Total GWh`=sum(kwh_adj/1e6),
    `Participant %` = sum(kwh_adj[fuel_part!="Non-Participant"]/1e6)/`Total GWh`,
    `Non-Participant %` = sum(kwh_adj[fuel_part=="Non-Participant"]/1e6)/`Total GWh`,
    `Proportion of Total` = `Total GWh`/sum(characterization_adj_kwh$kwh_adj/1e6)) %>% 
  data.frame()

# write.xlsx(SPWT_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj GWh Sector Proportion Table",append = TRUE,row.names = FALSE)

##therms by bus type
characterization_adj_therms<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(gas_fuel_size,naicsgroup,participation) %>% 
  summarise(n=n(),therms=sum(therms2017,na.rm = TRUE)) %>% 
  left_join(State_adj,by="naicsgroup") %>%
  ungroup() %>% 
  mutate(count_adj=ifelse(participation=="Non-Participant",round(n*adj),n),therms_adj=ifelse(participation=="Non-Participant",therms*adj,therms),fuel_part=ifelse(participation=="Non-Participant","Non-Participant",paste(gas_fuel_size,participation))) %>% 
  group_by(naicsgroup,fuel_part) %>% 
  arrange(naicsgroup,desc(fuel_part)) %>% 
  summarise(therms_adj=sum(therms_adj),count_adj=sum(count_adj)) %>% 
  group_by(naicsgroup) %>% 
  arrange(desc(factor(fuel_part,levels = fuel_part_levels2))) %>% 
  mutate(text=cumsum(therms_adj)+4.25e6)

## therms by region
characterization_reg_therms<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(gas_fuel_size,Region,participation) %>% summarise(n=n(),therms=sum(therms2017,na.rm = TRUE)) %>% 
  mutate(fuel_part=ifelse(participation=="Non-Participant","Non-Participant",paste(gas_fuel_size,participation)))  

## therms by recency
characterization_recent_therms<-population %>% 
  filter(naicsgroup!="Multifamily"&naicsgroup!="Multifamily/Residential") %>% 
  group_by(recent_part,naicsgroup) %>% summarise(n=n(),therms=sum(therms2017,na.rm = TRUE)) %>% 
  left_join(State_adj,by="naicsgroup") %>% 
  ungroup() %>% 
  mutate(count_adj=ifelse(recent_part=="Non-Participant",round(n*adj),n),therms_adj=ifelse(recent_part=="Non-Participant",therms*adj,therms)) %>% 
  group_by(naicsgroup,recent_part) %>%
  arrange(naicsgroup,desc(recent_part)) %>% 
  summarise(therms_adj=sum(therms_adj),count_adj=sum(count_adj)) %>%
  group_by(naicsgroup) %>% 
  arrange(desc(factor(recent_part,levels = recent_levels))) %>%
  mutate(text=cumsum(therms_adj)+4e6)

# chart text adjustments
characterization_recent_therms$count_adj<-format(round(characterization_recent_therms$therms_adj/1e6,2),big.mark = ",",format="d")
characterization_recent_therms$count_adj[characterization_recent_therms$recent_part!="Non-Participant"]<-""

characterization_recent_therms$text[characterization_recent_therms$therms_adj>20000000&characterization_recent_therms$count_adj!=""]<-
  characterization_recent_therms$text[characterization_recent_therms$therms_adj>20000000&characterization_recent_therms$count_adj!=""]-8.5e6
  
# •	Summary of commercial customer by therm usage
ggplot(characterization_recent_therms %>% filter(naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",aes(x=factor(naicsgroup,levels=NAICS_levels),y=therms_adj/1e6,fill=factor(recent_part,recent_levels)))+
  scale_fill_manual(values = EEcolors5,labels=c("Non-Participant","Recent Participant","Past Participant"))+
  geom_text(aes(x=factor(naicsgroup,levels=NAICS_levels),y=text/1e6,label=count_adj))+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(y="Annual Therms Usage (Millions)",x="Business Sector",fill="Participation Status")

# ggsave("adj_therms.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

## sector part date Therms table
SPDT_Table<-characterization_recent_therms %>% 
  filter(naicsgroup!="Industrial") %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(`Total Therms (MM)`=sum(therms_adj/1e6),
    `% Therms`=`Total Therms (MM)`/sum(characterization_recent_therms$therms_adj/1e6),
    Recent=sum(therms_adj[recent_part=="Recent Participant"]/1e6),
    Past=sum(therms_adj[recent_part=="Past Participant"]/1e6),
    `Non-Participant`=sum(therms_adj[recent_part=="Non-Participant"]/1e6)) %>% 
  data.frame()

# write.xlsx(SPDT_Table,file = "/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Therms Sector Date Table",append = TRUE,row.names = FALSE)

# •	Proportion of program participation by market sector - gas usage
ggplot(characterization_adj_therms %>% filter(fuel_part!="Unknown Size Participant"&naicsgroup!="Industrial") %>% ungroup())+
  geom_bar(stat="identity",position="fill",aes(x=factor(naicsgroup,levels=NAICS_levels),y=therms_adj,fill=factor(fuel_part,fuel_part_levels2)))+
  scale_fill_manual(values = EEcolors5)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica",size=10),panel.grid.major.y = element_blank())+
  labs(y="Proportion of Therms Usage",x="Business Sector",fill="Site Size")

# ggsave("adj_therms_prop.jpg",device = "jpeg",path = "~/desktop/ETO Plots/",width = 6.5,height = 6)

# sector proportion therms table
SPTT_Table <- characterization_adj_therms %>% filter(fuel_part!="Unknown Size Participant"&naicsgroup!="Industrial") %>% 
  group_by(sector=naicsgroup) %>% 
  summarise(
    `Total Therms (MM)`=sum(therms_adj/1e6),
    `Participant %` = sum(therms_adj[fuel_part!="Non-Participant"]/1e6)/`Total Therms (MM)`,
    `Non-Participant %` = sum(therms_adj[fuel_part=="Non-Participant"]/1e6)/`Total Therms (MM)`,
    `Proportion of Total` = `Total Therms (MM)`/sum(characterization_adj_therms$therms_adj/1e6)) %>% 
  data.frame()

# write.xlsx(SPTT_Table,file="/users/lehndorff/desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Adj Therms Sector Proportion Table",append = TRUE,row.names = FALSE)

# sector savings proportion table
SSPT_Table <- population %>% filter(participation=="Participant") %>% 
  left_join(
    projects %>% 
    group_by(et_siteid) %>% 
    summarise(kwh_save=sum(workingkwh),therms_save=sum(workingtherms)),
    by="et_siteid") %>% 
  filter(naicsgroup!="Industrial"&!grepl("Multi",naicsgroup)) %>% 
  group_by(Sector=naicsgroup) %>% 
  summarise(
    Participants=n(),
    `2017 GWh Usage`=sum(kwh2017/1e6,na.rm = TRUE),
    `Total GWh Savings` = sum(kwh_save/1e6,na.rm = TRUE),
    `GWh Savings Ratio`=`Total GWh Savings`/`2017 GWh Usage`,
    `2017 Therms Usage (MM)`=sum(therms2017/1e6,na.rm = TRUE),
    `Total Therms Savings (MM)` = sum(therms_save/1e6,na.rm = TRUE),
    `Therms Savings Ratio`=`Total Therms Savings (MM)`/`2017 Therms Usage (MM)`) %>% 
  data.frame()
  
# write.xlsx(SSPT_Table,file = "/Users/Lehndorff/Desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Sector Savings Ratios",append = TRUE,row.names = FALSE)

# count tables
## recent projects by track
table1<-projects %>% 
  filter(year>=2017&(programdescription=="Existing Buildings"|trackval==1)) %>% filter(projectid!="") %>%
  group_by(trackval) %>% 
  summarise(Parts_2017=n_distinct(projectid[year==2017]),Parts_2018=n_distinct(projectid[year==2018]),Total=n_distinct(projectid[year>=2017]))

table1$trackval<-c("SEM","Custom","Direct Install","Standard","Lighting","Other")

## Recent measures by type
table2<-projects %>%
  filter(year>=2017&(programdescription=="Existing Buildings"|trackval==1)) %>% 
  filter(bcreportdescription!="") %>% group_by(bcreportdescription) %>% 
  summarise(Measures_2017=sum(year==2017),Measures_2018=sum(year==2018),Total=n())

EBsites<-unique(subset(projects,year>=2017&(programdescription=="Existing Buildings"|trackval==1))$et_siteid)

## recent sites by sector
table3<-population %>% 
  filter(participation=="Participant"&et_siteid%in%EBsites) %>% 
  filter(year(date)>=2017) %>% 
  filter(naicsgroup!="Industrial"&!grepl("Multi",naicsgroup)) %>% 
  group_by(naicsgroup) %>%
  summarise(Parts_2017=n_distinct(et_siteid[year(date)==2017]),Parts_2018=n_distinct(et_siteid[year(date)==2018]),Total=n_distinct(et_siteid[year(date)>=2017])) 

## recent sites by region
table4<-population %>% 
  filter(participation=="Participant"&et_siteid%in%EBsites) %>% 
  filter(year(date)>=2017) %>% group_by(Region) %>% 
  summarise(Parts_2017=n_distinct(et_siteid[year(date)==2017]),Parts_2018=n_distinct(et_siteid[year(date)==2018]),Total=n_distinct(et_siteid[year(date)>=2017])) 

# write.xlsx(as.data.frame(table1),file="/Users/Lehndorff/Desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Project by Track",append = TRUE,row.names = FALSE)
# write.xlsx(as.data.frame(table2),file="/Users/Lehndorff/Desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Measure by Type",append = TRUE,row.names = FALSE)
# write.xlsx(as.data.frame(table3),file="/Users/Lehndorff/Desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Site by Sector",append = TRUE,row.names = FALSE)
# write.xlsx(as.data.frame(table4),file="/Users/Lehndorff/Desktop/ETO Plots/ETO_Tables.xlsx",sheetName = "Site by Region",append = TRUE,row.names = FALSE)

# Final analysis data
final_data<-population %>%
  ungroup() %>% 
  filter(!grepl("Multifamily",naicsgroup)) %>%
  select(et_siteid,naicsgroup,kwh2017,therms2017,fuel_group,elec_fuel_size,gas_fuel_size,fuel_size,County,Region,participation,track,date,recent_part,fuel_part) %>% 
  left_join(State_adj %>% select(naicsgroup,adj),by="naicsgroup")

final_data$adj[final_data$recent_part!="Non-Participant"]<-1

counts<-final_data %>% 
  group_by(naicsgroup,recent_part) %>% 
  summarise(sum(adj))

colnames(final_data)<-c("et_siteid","Business Sector","kwh2017","therms2017","Fuel Type","Electric Size","Gas Size","Overall Size","County","Region","Participation","Program Track","Most Recent Participation Date","Participation Status","Size/Participation","Weight")

# write.csv(final_data,"/volumes/Projects/430011 - ETO Existing Buildings/Data/Analysis_Dataset.csv",row.names = FALSE)

final_data<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Analysis_Dataset.csv",stringsAsFactors = FALSE)

counts<-final_data %>% group_by(Business.Sector,Participation.Status) %>% summarise(sum(Weight))
