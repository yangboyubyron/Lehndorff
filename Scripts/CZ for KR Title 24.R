# packages
library(ggplot2)
library(dplyr)

# ca.counties<-map_data('county') %>% filter(region=="california")
# load table
title.24<-read.csv("~/desktop/Title 24 Map.csv",stringsAsFactors = FALSE)

# load map
load("/Volumes/Projects/Common Data Files/CECBuildingClimateZonesMap/CEC_CZs.Rdata")
cz_map$CZ<-as.numeric(as.vector(cz_map$CZ))

# translate title 24 CZ to map CZ.
title.24$trans.CZ<-NA
title.24$trans.CZ[title.24$CZ==13]<-5
title.24$trans.CZ[title.24$CZ==12]<-4
title.24$trans.CZ[title.24$CZ==10]<-2
title.24$trans.CZ[title.24$CZ==9]<-16
title.24$trans.CZ[title.24$CZ==8]<-15
title.24$trans.CZ[title.24$CZ==7]<-14
title.24$trans.CZ[title.24$CZ==6]<-13
title.24$trans.CZ[title.24$CZ==4]<-11
title.24$trans.CZ[title.24$CZ==3]<-10
title.24$trans.CZ[title.24$CZ==2]<-9

title.24$trans.CZ[title.24$CZ==1]<-1
title.24$trans.CZ[title.24$CZ==5]<-12
title.24$trans.CZ[title.24$CZ==11]<-3
title.24$trans.CZ[title.24$CZ==14]<-6
title.24$trans.CZ[title.24$CZ==15]<-7
title.24$trans.CZ[title.24$CZ==16]<-8

# hide irrelevant CZs
cz_map$CZ[!cz_map$CZ%in%title.24$trans.CZ]<-NA

# join CZ names
map.out<-cz_map %>% left_join(title.24,by=c("CZ"="trans.CZ"))

# CZ hybrid name
# map.out$name<-ifelse(is.na(map.out$County),NA,paste(map.out$CZ.y,map.out$County,sep = " - "))
# map.out$name<-factor(map.out$name,levels = c(
#   "2 - Santa Rosa",
#   "3 - Oakland",
#   "4 - San Jose-Reid",
#   "6 - Torrance",
#   "7 - San Diego-Lindbergh",
#   "8 - Fullerton",
#   "9 - Burbank-Glendale",
#   "10 - Riverside",
#   "12 - Sacramento",
#   "13 - Fresno"
# ))

map.out$Revisit<-factor(map.out$Revisit,levels = c(
  "New",
  "Revisit",
  "Not Included"
))

map.plot<-ggplot(map.out, aes(long, lat, group=group, fill=Revisit)) + 
  geom_polygon(alpha=1) +
  geom_path(size=.3,color="black") +
  # geom_path(data=map.out %>% filter(Revisit=="Revisit"),size=.6,color="green")+
  coord_map()+
  theme_void()+
  theme(
    legend.box.margin = margin(0,4,0,0)
  )+
  labs(fill="Audit Type")+
  scale_fill_manual(
    values = c(
      "New"="#73B633",
      "Revisit"="#095C9C",
      "Not Included"="light gray"
    )
  )
ggsave(map.plot,file="~/desktop/MF Map.jpg",width = 8,height = 8)

