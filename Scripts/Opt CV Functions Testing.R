TheFrame <- read.csv("~/Desktop/Old Sample Frames/SampleFrame_12062016.csv", stringsAsFactors = FALSE)
TheFrame$PrimaryMeasure[TheFrame$CProjectID=="157933"]<-"LEED"

SomeOpts<-OptSampleDesign(Data = TheFrame,Identifier = "CProjectID",SizeVar = "SumKWH",Group = "PrimaryMeasure",which_groups = c("CustomElectric","Motor"),n_strata=7,tolerance=.03,confidence = 1.645,precision = .1,Progress = TRUE,FloatTolerance=TRUE,SmartTolerance=TRUE,OptimalOption=TRUE,Optimize=TRUE)

SomeReOpts<-ReOptimize(StrataData=AllStrata,OptimalOption=TRUE,confidence = 1.284,precision = .2)

SomeSelectedOptions<-PrepDesign(DesignOptions = SomeReOpts,Selection = c(4,8,12,16,21,25,29,30,35))
SomeSelectedOptions<-PrepDesign(DesignOptions = SomeReOpts,Selection = c(3,4,8))

SomeSampleDesign<-DesignSample(PrepedDesign = SomeSelectedOptions,SummaryLevel = "strata")

LameOpts<-OptSampleDesign(Data = TheFrame,Identifier = "CProjectID",SizeVar = "SumKWH",Group = "PrimaryMeasure",which_groups = c("All"),n_strata=5,confidence = 1.284,precision = .2,Optimize = FALSE)

Comp.05<-left_join(LameOpts %>% select(EU,strata,CV,finsamp) %>% mutate(merge=paste(EU,strata,sep="-")),SomeOpts %>% select(EU,strata,CV,finsamp) %>% mutate(merge=paste(EU,strata,sep="-")),by="merge",suffix=c(".LAME",".OPT")) %>% mutate(diff=finsamp.OPT-finsamp.LAME)


# load("~/Desktop/Function_Test_Data.RData")

library(gtools)
combs<-as.data.frame(permutations(2,5,repeats.allowed = TRUE))
combs[combs==2]<-0
combs$time<-100

for(i in 1:nrow(combs)){
  print(i)
  r<-proc.time()
  A<-as.logical(combs$V1[i])
  B<-as.logical(combs$V2[i])
  C<-as.logical(combs$V3[i])
  D<-as.logical(combs$V4[i])
  E<-as.logical(combs$V5[i])
  out<-OptSampleDesign(Data = TheFrame,Identifier = "CProjectID",SizeVar = "SumKWH",Group = "PrimaryMeasure",which_groups = c("!LED"),n_strata=5,tolerance=.03,confidence = 1.645,precision = .1,Progress = A,FloatTolerance=B,SmartTolerance=C,OptimalOption=D,Optimize=E)
  t<-proc.time()
  combs$time[i]<-(t-r)[[3]]
}

