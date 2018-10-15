library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Data
population2<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)
counties<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Oregon and SW Washington zip codes.csv",stringsAsFactors = FALSE) %>% group_by(Zip.Code) %>% summarise(County=unique(County))
regions<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/ETO Regions.csv",stringsAsFactors = FALSE)
industries<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/Employment by Industry.csv",stringsAsFactors = FALSE)
sector_groups<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/EB market sector categories.csv",stringsAsFactors = FALSE) %>% select(et_marketname,Evergreen.categories)
SEM<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/SEM impact data/SEM and Capital Participation Data to Evergreen.csv",stringsAsFactors = FALSE)

population<-population2 %>% left_join(sector_groups,by=c("market"="et_marketname"))

# assign NAICS group to population
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

population$naicsgroup<-population$Evergreen.categories
population$naicsgroup[population$naicsgroup=="Residential -- we'll probably want to exclude this"|is.na(population$naicsgroup)]<-"Unknown"
table(population$naicsgroup)

population$naicsgroup[population$naicsgroup=="Unknown"]<-population$naics_code_group[population$naicsgroup=="Unknown"]
table(population$naicsgroup)

# then general
population$naicsgroup[population$naicsgroup=="Unknown"&(population$multifamily==1|population$residential==1)]<-"Multifamily/Residential"
population$naicsgroup[population$naicsgroup=="Unknown"&(population$commercial==1|population$nonresidential==1)]<-"Unknown Commercial"
table(population$naicsgroup)

# size categories
population$fuel_group<-"Unknow Fuel"
population$fuel_group[!is.na(population$kwh2017)&population$kwh2017>0]<-"Electric"
population$fuel_group[!is.na(population$therms2017)&population$therms2017>0]<-"Gas"
population$fuel_group[!is.na(population$kwh2017)&!is.na(population$therms2017)&population$kwh2017>0&population$therms2017>0]<-"Electric and Gas"
table(population$fuel_group)

summary(population$kwh2017[population$fuel_group=="Electric and Gas"]*0.0034121412+population$therms2017[population$fuel_group=="Electric and Gas"]*.1)

population$elec_fuel_size<-"Unknown Size"
population$elec_fuel_size[!is.na(population$kwh2017)&population$kwh2017>0]<-"Large"
population$elec_fuel_size[population$kwh2017<500000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Medium"
population$elec_fuel_size[population$kwh2017<50000&!is.na(population$kwh2017)&population$kwh2017>0]<-"Small"
population$gas_fuel_size<-"Unknown Size"
population$gas_fuel_size[!is.na(population$therms2017)&population$therms2017>0]<-"Large"
population$gas_fuel_size[population$therms2017<50000&!is.na(population$therms2017)&population$therms2017>0]<-"Medium"
population$gas_fuel_size[population$therms2017<10000&!is.na(population$therms2017)&population$therms2017>0]<-"Small"
# population$fuel_size[population$fuel_group=="Electric and Gas"]<-"Large"
# population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<700]<-"Medium"
# population$fuel_size[population$fuel_group=="Electric and Gas"&(population$kwh2017*0.0034121412+population$therms2017*.1)<100]<-"Small"

population$fuel_comb<-paste(population$elec_fuel_size,population$gas_fuel_size)
population$fuel_size<-"Unknown"
population$fuel_size[grepl("Large",population$fuel_comb)]<-"Large"
population$fuel_size[grepl("Medium",population$fuel_comb)]<-"Medium"
population$fuel_size[grepl("Small",population$fuel_comb)]<-"Small"

table(population$elec_fuel_size,population$fuel_group)
table(population$gas_fuel_size,population$fuel_group)
table(population$fuel_size,population$fuel_group)
table(population$fuel_size,population$naicsgroup)

# Region
zip_region<-left_join(counties,regions,by="County")
population<-left_join(population,zip_region,by=c("et_zip"="Zip.Code"))
table(population$Regions.for.EB.Process,exclude=NULL)

population<-population %>% group_by(et_city) %>% mutate(Region=ifelse(et_state=="WA","Southwest Washington",first(sort(unique(Regions.for.EB.Process)))))
table(population$Region,exclude = NULL)

test<-subset(population, is.na(Region))

region_levels<-c("Portland Metro","Northwest Oregon","Central Oregon","Southern Oregon","Eastern Oregon","Southwest Washington")

# Participation
nonpartproj<-subset(projects,programdescription=="")$et_siteid

NonPartCon<-contacts %>% filter(et_siteid%in%nonpartproj&CRMContactName==""&CRMContactEmail==""&CRMContactBusinessPhone==""&CRMContactMobilePhone=="") %>% 
  group_by(et_siteid) %>% mutate(row=1:n()) %>% filter(row==1)

population$participation<-!(population$et_siteid%in%NonPartCon$et_siteid&population$participanttype=="Non-Participant")

table(population$participation,population$participanttype)

population$participation<-ifelse(population$participation,"Participant","Non-Participant")

# Project Type
projects$SEM<-projects$projecttrackdescription=="SEM Cohort"
projects$SEM[!projects$projectid%in%SEM$ProjectId]<-FALSE
projects$Custom<-projects$projecttrackdescription=="Existing Buildings - Custom"
projects$DI<-projects$projecttrackdescription=="Existing Bldgs - Direct Install"
projects$Standard<-projects$projecttrackdescription=="Existing Buildings - Standard"
projects$Lighting<-projects$projecttrackdescription=="Existing Buildings - Lighting"

summary(projects[,c("SEM","Custom","DI","Standard","Lighting")])

# calculate year
projects$date<-as.Date(projects$installeddate)
projects$date[is.na(projects$date)]<-as.Date(projects$maxrecognizeddate[is.na(projects$date)])
projects$year<-year(projects$date)

ProjectAgg<-projects %>% 
  group_by(et_siteid) %>% 
  summarise(n_proj=n(),kWh_savings=sum(workingkwh,na.rm = TRUE),Therms_savings=sum(workingtherms,na.rm = TRUE),
    SEM=ifelse(sum(SEM)>0,"SEM ","X"),Custom=ifelse(sum(Custom)>0,"Custom ","X"),DI=ifelse(sum(DI)>0,"DI ","X"),Standard=ifelse(sum(Standard)>0,"Standard ","X"),Lighting=ifelse(sum(Lighting)>0,"Lighting ","X"),
    combo=gsub("[[:space:]]$","",gsub("X","",paste(SEM,Custom,DI,Standard,Lighting,sep=""))))

table(ProjectAgg$combo)
summary(ProjectAgg)

Proj_Pop<-left_join(ProjectAgg,population %>% select(et_siteid,participation,Region,naicsgroup,fuel_size, kwh2017,therms2017),by="et_siteid") %>% filter(!is.na(Region))

table(Proj_Pop$kWh_savings>Proj_Pop$kwh2017)  

# compare with ODE
## assign NAICS group
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

pop_by_naics<-population %>% group_by(naicsgroup) %>% summarise(in_eto=n(),parts=sum(participation=="Participant"))
IndAgg<-industries %>% group_by(naicsgroup) %>% summarise(Units=sum(as.numeric(gsub(",","",Units))))
State_adj<-full_join(pop_by_naics,IndAgg,by="naicsgroup") %>% mutate(adj=(Units-parts)/(in_eto-parts))
State_adj$adj[is.na(State_adj$adj)]<-1
State_adj$adj[State_adj$naicsgroup!="Unknown Commercial"&State_adj$adj<1]<-1

State_adj$eto_NonPart<-State_adj$in_eto-State_adj$parts
State_adj$EST_NonPart<-State_adj$Units-State_adj$parts
State_adj$EST_NonPart[is.na(State_adj$EST_NonPart)|State_adj$EST_NonPart<0]<-0

# save out files for markdown
# write.csv(Proj_Pop,"/volumes/Projects/430011 - ETO Existing Buildings/Data/Project_info_for_Summaries.csv",row.names = FALSE)
# write.csv(State_adj,"/volumes/Projects/430011 - ETO Existing Buildings/Data/State_info_for_Summaries.csv",row.names = FALSE)

grid(plot)

grid.arrange(zzz,yyy,ncol=2)

zzz<-grob(plot)
yyy<-grob(plot2)

grid.draw(aaa)
grid.draw(plot,plot2)
grid.draw(plot2)

grid.arrange(plot,plot2)

fff<-marrangeGrob(grobs=list(zzz,yyy),ncol=2,nrow=1)

aaa<-gList(zzz,yyy)

grid.draw(fff)
ggplot(fff)
ggsave("test.pdf",fff)


combos2<-subset(combos,fuel_size=="Medium")

SAVE<-Proj_Pop

Proj_Pop<-subset(Proj_Pop,combo)

draw.quintuple.venn(
  area1 = sum(Proj_Pop$SEM!="X"),#379
  area2 = sum(Proj_Pop$Custom!="X"),#4480
  area3 = sum(Proj_Pop$DI!="X"),#1121
  area4 = sum(Proj_Pop$Standard!="X"),#6774
  area5 = sum(Proj_Pop$Lighting!="X"),#10021
  # area1 = 96,
  # area2 = 2545,
  # area3 = 908,
  # area4 = 4210,
  # area5 = 7680,
  n12 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting=="X"),#60
	n13 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting=="X"),#0
	n14 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#29
	n15 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#30
	n23 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting=="X"),#30
	n24 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#889
	n25 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#741
	n34 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#77
	n35 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#67
	n45 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#960
	n123 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting=="X"),#0
	n124 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#34
	n125 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#41
	n134 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#0
	n135 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#0
	n145 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#21
	n234 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#8
	n235 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#7
	n245 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#454
	n345 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#21
	n1234 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting=="X"),#0
	n1235 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard=="X"&Proj_Pop$Lighting!="X"),#0
	n1245 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI=="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#68
	n1345 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom=="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#0
	n2345 = sum(Proj_Pop$SEM=="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#3
	n12345 = sum(Proj_Pop$SEM!="X"&Proj_Pop$Custom!="X"&Proj_Pop$DI!="X"&Proj_Pop$Standard!="X"&Proj_Pop$Lighting!="X"),#0
  category=c("SEM","Custom","DI","Standard","Lighting")
  )

one<-Proj_Pop$SEM=="SEM "
two<-Proj_Pop$Custom=="Custom "
three<-Proj_Pop$DI=="DI "
four<-Proj_Pop$Standard=="Standard "
five<-Proj_Pop$Lighting=="Lighting "

plot<- draw.quintuple.venn(
  area1 = sum(Proj_Pop$SEM!="X"),#379
  area2 = sum(Proj_Pop$Custom!="X"),#4480
  area3 = sum(Proj_Pop$DI!="X"),#1121
  area4 = sum(Proj_Pop$Standard!="X"),#6774
  area5 = sum(Proj_Pop$Lighting!="X"),#10021
  n12 = sum(one&two),
	n13 = sum(one&three),
	n14 = sum(one&four),
	n15 = sum(one&five),
	n23 = sum(two&three),
	n24 = sum(two&four),
	n25 = sum(two&five),
	n34 = sum(three&four),
	n35 = sum(three&five),
	n45 = sum(four&five),
	n123 = sum(one&two&three),
	n124 = sum(one&two&four),
	n125 = sum(one&two&five),
	n134 = sum(one&three&four),
	n135 = sum(one&three&five),
	n145 = sum(one&four&five),
	n234 = sum(two&three&four),
	n235 = sum(two&three&five),
	n245 = sum(two&four&five),
	n345 = sum(three&four&five),
	n1234 = sum(one&two&three&four),
	n1235 = sum(one&two&three&five),
	n1245 = sum(one&two&four&five),
	n1345 = sum(one&three&four&five),
	n2345 = sum(two&three&four&five),
	n12345 = sum(one&two&three&four&five),
  category=c("SEM","Custom","DI","Standard","Lighting")
  )

grid.draw(plot)


map <- map_data('county') %>% subset(region == 'oregon') %>% left_join(regions %>% mutate(merge=tolower(County)),by=c("subregion"="merge"))

county_agg<-Proj_Pop %>% filter(naicsgroup=="Office") %>% group_by(Region) %>% summarise(mean_save=median(kWh_savings[kWh_savings>0]))

for_map_plot<-left_join(map,county_agg,by=c("Regions.for.EB.Process"="Region"))

ggplot() +
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = for_map_plot, aes(x = long, y = lat, group = group,fill = mean_save), color = "black", size = .3)
  
+
  geom_polygon(data = subset(map, is.na(CZ)==FALSE), aes(x = long, y = lat, group = group, fill=cdd_predict), color = "black", size = .3) +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 4, limits = c(0, 8)) +
  labs(x = NULL, y = NULL, fill = "Predicted Error", title = "Local Weather Station Temperature Error (CDD)") + 
  theme(plot.title = element_text(color = '#666633', size = 12),
       axis.text = element_blank(), axis.ticks = element_blank(), legend.position = 'bottom')

for (i in unique(Proj_Pop$naicsgroup)){
  print(i)
  if(grepl("multifamily",i,ignore.case = TRUE)){next}
  if(i=="Industrial"){next}
  plot<-map_plot(sector=i)
  ggsave(filename = paste("~/desktop/plots temp/",gsub("/","-",i),".jpg",sep=""),plot=plot)
}

comb_funct<-function(data=combos,track,metric="percent"){
  if(metric=="percent"){
    comb_out<-data[as.vector(data[,track]==TRUE),] %>% 
  group_by(Track=as.character(track),fuel_size) %>% 
  summarise(
    total=sum(n),
    SEM=ifelse(track=="SEM",sum(0,n[types==1])/total,sum(n*SEM)/total),
    Custom=ifelse(track=="Custom",sum(0,n[types==1])/total,sum(n*Custom)/total),
    DI=ifelse(track=="DI",sum(0,n[types==1])/total,sum(n*DI)/total),
    Standard=ifelse(track=="Standard",sum(0,n[types==1])/total,sum(n*Standard)/total),
    Lighting=ifelse(track=="Lighting",sum(0,n[types==1])/total,sum(n*Lighting)/total),
    Total=sum(n)/0
    ) %>% 
  select(-total) %>% 
  melt(id.vars=c("Track","fuel_size"))
    
    return(comb_out)
    }
  if(metric=="count"){
    comb_out<-data[as.vector(data[,track]==TRUE),] %>% 
  group_by(Track=as.character(track),fuel_size) %>% 
  summarise(
    total=sum(n),
    SEM=ifelse(track=="SEM",sum(0,n[types==1])/total,sum(n*SEM)/total),
    Custom=ifelse(track=="Custom",sum(0,n[types==1])/total,sum(n*Custom)/total),
    DI=ifelse(track=="DI",sum(0,n[types==1])/total,sum(n*DI)/total),
    Standard=ifelse(track=="Standard",sum(0,n[types==1])/total,sum(n*Standard)/total),
    Lighting=ifelse(track=="Lighting",sum(0,n[types==1])/total,sum(n*Lighting)/total),
    Total=sum(n)
    ) %>% 
  select(-total) %>% 
  melt(id.vars=c("Track","fuel_size")) %>% 
      mutate(value=ifelse(variable=="Total",round(value,0),paste(round(value*100,0),"%",sep="")))
    
  return(comb_out)
  }
}


CombSEM<-comb_funct(track = "SEM")
CombCust<-comb_funct(track = "Custom")
CombDI<-comb_funct(track = "DI")
CombStan<-comb_funct(track = "Standard")
CombLite<-comb_funct(track = "Lighting")

CombFull<-bind_rows(CombSEM,CombCust,CombDI,CombStan,CombLite)

TextSEM<-comb_funct(track = "SEM",metric = "count")
TextCust<-comb_funct(track = "Custom",metric = "count")
TextDI<-comb_funct(track = "DI",metric = "count")
TextStan<-comb_funct(track = "Standard",metric = "count")
TextLite<-comb_funct(track = "Lighting",metric = "count")

TextFull<-bind_rows(TextSEM,TextCust,TextDI,TextStan,TextLite)

ggplot(CombFull %>% filter(fuel_size!="Unknown") %>% 
    mutate(t_f=factor(Track,levels = c("SEM","Custom","DI","Standard","Lighting","Total"))))+
  geom_tile(aes(x=variable,fill=value*100,y=1))+
  geom_text(data=TextFull %>% filter(fuel_size!="Unknown") %>%
      mutate(t_f=factor(Track,levels = c("SEM","Custom","DI","Standard","Lighting","Total"))),
    aes(x=variable,y=1,label=value),size=3)+
  theme(text=element_text(size=6),
    axis.text.x = element_text(angle = 0, hjust = .5,size=6),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    strip.text.y = element_text(angle = 180))+
  scale_fill_gradient2(high = "red",low="green",mid = "yellow",midpoint = 50,na.value = "gray90")+
  scale_x_discrete(position = "top")+
  coord_fixed(ratio = 1)+
  labs(y=NULL,x="Sub-Track",fill="% (by row)")+
  facet_grid(t_f~fuel_size,switch = "both")

pie_dat<-combos %>% 
  # filter(naicsgroup==sector) %>% 
  group_by(fuel_size) %>% 
  summarise(N1=sum(n[types==1])/sum(n),N2=sum(n[types==2])/sum(n),N3=sum(n[types==3])/sum(n),N4=sum(n[types==4])/sum(n),N5=sum(n[types==5])/sum(n)) %>% 
  melt(id.vars="fuel_size")

ggplot(zzz %>% filter(fuel_size!="Unknown"))+
  geom_bar(aes(x="",y=value,fill=variable),stat = "identity",position = "fill",width = 1,color="black",size=0)+
  # geom_text(aes(x="",y=value,label=round(value*100,0)))+
  coord_polar("y")+
  theme(text=element_text(size=6), axis.text = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  labs(fill="Track Combinations")+
  scale_fill_manual(
    values = c("#99CC33","darkorange","#999933","#666699","black"),
    breaks = c("N1","N2","N3","N4","N5"),
    labels = c("One Track","Two Tracks","Three Tracks","Four Tracks","Five Tracks")
  )+
  facet_grid(.~fuel_size)
