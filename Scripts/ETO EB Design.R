# ETO EB Sample Design
library(xlsx)
library(dplyr)

population<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Eligible Commercial Sites.csv",stringsAsFactors = FALSE)
projects<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Projects.csv",stringsAsFactors = FALSE)
contacts<-read.csv("/volumes/Projects/430011 - ETO Existing Buildings/Data/All Commercial Site Contacts.csv",stringsAsFactors = FALSE)

table(projects$projecttrackdescription)
table(projects$projecttrackdescription[projects$programdescription=="Existing Buildings"])
projects$trackval<-1000
projects$trackval[projects$projecttrackdescription=="SEM Cohort"]<-1
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Custom"]<-2
projects$trackval[projects$projecttrackdescription=="Existing Bldgs - Direct Install"]<-3
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Standard"]<-4
projects$trackval[projects$projecttrackdescription=="Existing Buildings - Lighting"]<-5
table(projects$trackval)

projects$date<-as.Date(projects$maxrecognizeddate)

Parts<-projects %>% group_by(et_siteid) %>% summarise(ProgramTrack=min(trackval[as.Date(date)>="2017-01-01"]),Repeat=n_distinct(projectid))
Parts<-projects %>% group_by(et_siteid) %>% summarise(ProgramTrack=min(trackval),Repeat=n_distinct(projectid))

table(Parts$ProgramTrack,Parts$Repeat>1)

n_distinct(projects$projectid[projects$trackval==2&as.Date(projects$installeddate)>="2017-01-01"])
