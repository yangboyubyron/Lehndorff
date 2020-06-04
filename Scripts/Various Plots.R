# Plot examples
#### AMICS-HOPPS2 ####
load("~/desktop/AMICS_HOPPS.Rdata")
comp_plot<-ggplot(comp_data %>% select(hour,ws_hvac,ws_non_hvac,non_ws_hvac,non_non) %>% reshape2::melt(id.vars="hour"))+
  geom_bar(aes(x=hour,y=value,fill=variable),position = "stack",stat="identity")+
  scale_fill_manual(
    breaks=c("ws_hvac","ws_non_hvac","non_ws_hvac","non_non"),
    labels=c("WS HVAC","WS non-HVAC","Baseline HVAC","Baseline non-HVAC"),
    values=c("blue","red","gray50","black")
  )+
  labs(y="kWh",x="Hour",fill="Component")

#### ETO EB Summaries ####
load(file="~/desktop/ETO_EB.Rdata")

ggplot(map_Office)+
  coord_quickmap(xlim = c(-124.6, -116.4), ylim = c(42, 46.3)) +
  geom_polygon(data = map_Office, aes(x = long, y = lat,group=group,fill = Regions.for.EB.Process), color = "black", size = .3)+
  labs(fill="Region",title=NULL)+
  scale_fill_manual(
    values = c("#73b633","#005c9c","#584caa","#fabc2b","#5ebcdf"),
    breaks = c("Central Oregon","Eastern Oregon","Northwest Oregon","Portland Metro","Southern Oregon",NA),
    labels = c("Central Oregon","Eastern Oregon","Northwest Oregon","Portland Metro","Southern Oregon","Not Served"),
    na.value="gray40"
  )+
  theme_classic()+
  theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),
  text = element_text(size=8),
  plot.margin = margin(-10,0,-10,0),
  plot.title = element_text(size=8))
pene_plot(data=penetration_Government)
map_plot(data=map_Government)
freq_plot(for_heat = for_heat_Government,size_count = size_count_Government)
comb_plot(CombFull = CombFull_Government,TextFull = TextFull_Government,Size_Count = size_count_Government)
pie_plot(data = pie_dat_Government,Size_Count= size_count_Government)

#### RWLR Proposal ####
load("~/desktop/RWLR.Rdata")

t8.plot<-ggplot(rwlr.agg)+
  geom_area(aes(x=`Sales Year`,y=Total.bulbs,fill=factor(lamp.group,levels = names(levels.t8))),stat = "identity",position = "fill")+
  facet_wrap(.~facet_lab,scales = "free",ncol = 2)+
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "gray95"),
    text = element_text(size = 12))+
  scale_fill_manual(values=levels.t8)+
  scale_y_continuous(labels = scales::percent)+
  labs(y="Percent of Market",fill="Lamp Type")

map.plot<-ggplot(region.dat)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=factor(range,levels=levels2)),color="black")+
  # facet_wrap(lamp.group~.,ncol=1)+
  facet_wrap(.~lamp.group)+
  labs(fill="% Change\n  in Sales")+
  scale_fill_manual(values = colors)+
  # scale_fill_brewer(type="div",palette = 6)+
  # scale_fill_gradient2(high="dark green",low="blue",mid="white",trans="log",midpoint = 0,
  #   breaks=round(2^seq(-.8,3,.8),4),labels=paste(round(2^seq(-.8,3,.8)*100,0),"%"))+
  ggthemes::theme_map()+
  theme(
    legend.position = "bottom",
    strip.background.y = element_rect(fill="gray70"),
    text = element_text(size = 12))+
  coord_map()

#### LI TOU Impact ####
load("~/desktop/LI TOU.Rdata")
chart_impact<-function(data){
  output<-ggplot(data)+
    theme_gray()+
    geom_bar(aes(x=Utility,y=point,fill=Utility),stat = "identity",width = .75)+
    geom_errorbar(aes(x=Utility,ymin=point-error,ymax=point+error),size=0.15,width=.6)+
    facet_grid(.~Type)+
    theme(text=element_text(family="Gill Sans"), plot.title=element_text(color='#666633', size=12, hjust = 0.5), legend.position="bottom")+
    labs(y="Impact (kWh)")+
    scale_fill_manual(values = EEcolors7[1:3])
    
  return(output)
}

chart_impact2<-function(data){
  output<-ggplot(data)+
    theme_gray()+
    geom_bar(aes(x=Type,y=point,fill=Utility),stat = "identity",width = 0)+
    geom_bar(data = data %>% filter(Type=="TOU") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(data = data %>% filter(Type=="PCT") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(data = data %>% filter(Type=="PCT with\nEco+") %>% mutate(Type="Total\nChange"),aes(x=Type,y=point,fill=Utility),alpha=.4,width = .75,stat = "identity")+
    geom_bar(aes(x=Type,y=point,fill=Utility),stat = "identity",width = .75)+
    geom_errorbar(aes(x=Type,ymin=point-error,ymax=point+error),size=0.15,width=.6)+
    facet_grid(.~Utility)+
    theme(text=element_text(family="Gill Sans"), plot.title=element_text(color='#666633', size=12, hjust = 0.5), legend.position="bottom")+
    labs(y="Impact (kWh)",x="Program Intervention")+
    scale_fill_manual(values = EEcolors7[1:3])+
    guides(fill=FALSE)
    
  return(output)
}

all_plot<-chart_impact(data=plot_dat %>% filter(Time=="All"))+labs(y="Daily Usage Impact (kWh)")

peak_tc<-chart_impact2(data = plot_dat2 %>% filter(Time=="Peak"))+labs(y="Peak Usage Impact (kWh)")

#### AK analysis ####
load("~/desktop/AK.Rdata")
ggplot(ForPlot)+
  geom_line(aes(x=CalDate,y=SpendPerElig_02,group=1,color="Actual"),linetype=3)+
  geom_point(aes(x=CalDate,y=SpendPerElig_02,group=1,shape="Actual"),color="black")+
  geom_line(aes(x=CalDate,y=three_month,group=1,color="3-Month MA"),linetype=2)+
  geom_line(data=subset(ForPlot,FiscalMonth<=12),aes(x=CalDate,y=six_month,group=1,color="6-Month MA"),linetype=2)+
  geom_point(data=subset(ForPlot,FiscalMonth>12),aes(x=CalDate,y=six_month,group=1,shape="Forecasted 6-Month MA",color="6-Month MA"))+
  labs(title="Spending Per Member on Outpatient Hospital",y="Spending per Member ($)",x="Date")+
  scale_x_date(date_labels = "%b-%Y",date_breaks = "3 month")+
  scale_color_manual(name="Interval",values=cols)+
  scale_shape_manual(name="Actual / Forecasted",values = shape)+
  facet_grid(Group~.,scales = "free")

#### Avista/Demographic Analysis ####
load("~/desktop/Avista Demo.Rdata")
demo.plot2<-ggplot(demo_comb2)+
  geom_abline(intercept = 0,slope = 1)+
  geom_smooth(aes(x=ACS,y=Parts,group=variable,color=variable),method = "lm",se=FALSE)+
  geom_point(aes(x=ACS,y=Parts,color=variable))+
  theme_bw()+
  scale_y_continuous(limits = c(0,1))+
  coord_equal()+
  scale_color_brewer(type="qual",palette = 3)+
  labs(color="Demographic",title = "PUMA-Level Demographic Relationships",y="Proportion of Participant HHs",x="Proportion of PUMA HHs")

factor.label<-function(x){
  sign=sign(x)
  output<-x
  output[sign(x)==1]<-paste0(x[sign(x)==1]+1,":1")
  output[sign(x)==-1]<-paste0("1:",abs(x[sign(x)==-1])+1)
  output[sign(x)==0]<-"1:1"
  
  return(output)
}

demo.plot3<-ggplot(demo_comb2)+
  geom_hline(yintercept = 0)+
  geom_smooth(aes(x=Parts,y=serve.factor,group=variable,color=variable),method = "lm",se=FALSE)+
  geom_point(aes(x=Parts,y=serve.factor,color=variable))+
  theme_bw()+
  scale_color_brewer(type="qual",palette = 3)+
  scale_y_continuous(limits = c(-4,1),breaks = seq(-4,1,1),labels = factor.label(seq(-4,1,1)))+
  labs(color="Demographic",y="Service Factor",title = "PUMA-Level Demographic Service Factors",x="Proportion of Participant HHs")

senior.heat.plot<-ggplot(tract.map.data)+
  geom_polygon(aes(x=long,y=lat,fill=hhs_with_seniors,group=group),color=NA)+
  geom_polygon(data=region,aes(x=long,y=lat,group=group),color="black",fill=NA)+
  scale_fill_gradient(na.value = "grey90")+
  theme_void()+
  coord_map()+
  labs(title="Count of Seniors by Tract",fill="Count of Seniors")

#### Avista/Severance Analysis ####
load("~/desktop/Avista Sev.Rdata")
sev.model.plot2<-ggplot(plot.dat %>% filter(between(Prob,0.05,.95)))+
  geom_line(aes(x=Prob,y=value,color=Demo))+
  facet_wrap(.~variable,scales = "free_y")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Percentile of Demographic",y="Large Arrearage Likelihood",color="Demographic")+
  scale_color_brewer(
    type="qual",palette = 2,
    breaks=names(demo.level),
    labels=demo.level)+
  theme(
    text=element_text(family="Gill Sans", size=12))

demo.dist.plot<-ggplot(demo.dist.dat)+
  geom_boxplot(aes(x=variable,y=value,color=variable),size=.5,outlier.size = .2)+
  scale_color_brewer(type="qual",palette = 2)+
  labs(x="Demographic",y="Distribution of Tract-Level Proportions",color="Demographics")+
  guides(color=FALSE)+
  scale_x_discrete(
    breaks=names(demo.level),
    labels=demo.level
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(axis.title.x = element_text(vjust = 0),
    text=element_text(family="Gill Sans", size=12))

