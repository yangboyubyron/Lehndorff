library(ggplot2)

xyz<-as.data.frame(permutations(100,3,v=0:100,repeats.allowed = TRUE)/100)

xyz$rand<-runif(nrow(xyz))

xyz<-xyz %>% arrange(rand) %>% mutate(col=rgb(V1,V2,V3),rank=rank(rand)) %>% filter(rank<=110*180) %>%  data.frame()

xyz$x<-rep(1:180,times=110)
xyz$y<-rep(1:110,each=180)

s<-proc.time()
z<-ggplot(xyz)+
  geom_bin2d(binwidth=c(1,1), aes(x=x,y=y,fill=col))+
  scale_fill_manual(breaks=xyz$col,values=xyz$col)+
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  coord_cartesian(xlim=c(10,171),ylim=c(12,101))
proc.time()-s

ggsave(plot=z,filename = "~/desktop/plot.jpg",device = "jpeg",width = 32,height = 18,units = "in")
