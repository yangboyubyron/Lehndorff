library(dplyr)
library(ggplot2)
library(beepr)

# functions
strataoutfun<-function(){
  counts<-as.data.frame.table(table(Dataopt$Work))
  out<-list(EU=Measure,X1=counts$Freq[counts$Var1==1],X2=sum(counts$Freq[counts$Var1==2]),X3=sum(counts$Freq[counts$Var1==3]),X4=sum(counts$Freq[counts$Var1==4]),X5=sum(counts$Freq[counts$Var1==5]),X6=sum(counts$Freq[counts$Var1==6]),CV=subsetsx(),strata=max(Dataopt$Work),ProbTol=ProbTolfun(),CertTol=CertTolfun())
  return(out)
}
subsetsx<-function(data=Dataopt, strata="Work"){
  str1<-data$SizeVar[data[[strata]]==1]
  str2<-data$SizeVar[data[[strata]]==2]
  str3<-data$SizeVar[data[[strata]]==3]
  str4<-data$SizeVar[data[[strata]]==4]
  str5<-data$SizeVar[data[[strata]]==5]
  str6<-data$SizeVar[data[[strata]]==6]
  
  SD1<-max(sd(str1), 0, na.rm=TRUE)
  SD2<-max(sd(str2), 0, na.rm=TRUE)
  SD3<-max(sd(str3), 0, na.rm=TRUE)
  SD4<-max(sd(str4), 0, na.rm=TRUE)
  SD5<-max(sd(str5), 0, na.rm=TRUE)
  SD6<-max(sd(str6), 0, na.rm=TRUE)
  
  TotalCV<-(
    (SD1/max(mean(str1), 1, na.rm = TRUE))*sum(str1)+ 
      (SD2/max(mean(str2), 1, na.rm = TRUE))*sum(str2)+ 
      (SD3/max(mean(str3), 1, na.rm = TRUE))*sum(str3)+
      (SD4/max(mean(str4), 1, na.rm = TRUE))*sum(str4)+
      (SD5/max(mean(str5), 1, na.rm = TRUE))*sum(str5)+
      (SD6/max(mean(str6), 1, na.rm = TRUE))*sum(str6)
  )/sum(data$SizeVar)
  return(TotalCV)
}
InfSamp<-function(CV){
  round((Critical*as.numeric(CV)/Precision)^2,0)
}
FinSamp<-function(InfSamp,Total){
  ceiling(as.numeric(InfSamp)/(1+as.numeric(InfSamp)/sum(as.numeric(Total))))
}
CertTolfun<-function(){
  tol<-abs(1/max(Dataopt$Work)-sum(Dataopt$Percent[Dataopt$Work==1]))
  return(tol)
}
ProbTolfun<-function(){
  tol1<-c(sum(Dataopt$Percent[Dataopt$Work==min(2,max(Dataopt$Work))]),sum(Dataopt$Percent[Dataopt$Work==min(3,max(Dataopt$Work))]),
    sum(Dataopt$Percent[Dataopt$Work==min(4,max(Dataopt$Work))]),sum(Dataopt$Percent[Dataopt$Work==min(5,max(Dataopt$Work))]),sum(Dataopt$Percent[Dataopt$Work==min(6,max(Dataopt$Work))]))
  # tol2<-max(abs(1/max(Dataopt$Work)-tol1))
  # tol2<-(range(tol1)[2]-range(tol1)[1])
  tol2<-max(abs(tol1-sum(tol1[1:(max(Dataopt$Work)-1)])/(max(Dataopt$Work)-1)))
  return(tol2)
}
cleanup<-function(){
  keep<-c("SmartAdjs","eval","Data","DataIn","OptionsImp","strataoutfun","subsetsx","InfSamp","FinSamp","CertTolfun","ProbTolfun","cleanup")
  rm(list=ls()[!ls()%in%keep])
}

# load data
# DataIn <- read.csv("~/Desktop/Old Sample Frames/ALTSampleFrame_110917.csv", stringsAsFactors = FALSE)
DataIn <- read.csv("~/Desktop/Old Sample Frames/SampleFrame_12062016.csv", stringsAsFactors = FALSE)
DataIn<-as.data.frame(EBPagg2)

# assign program if applicable
DataIn$program<-"ALL"

# assign parameters
# DataIn$Identifier<-DataIn$C.Project.ID
# DataIn$SizeVar<-DataIn$SumMCF
# DataIn$Identifier<-DataIn$CProjectID
# DataIn$SizeVar<-DataIn$SumKWH
# DataIn$Group<-DataIn$PrimaryMeasure
DataIn$Identifier<-DataIn$NMGCUniquePartIdentifier
DataIn$SizeVar<-DataIn$Savings
DataIn$Group<-DataIn$program
# DataIn$Group<-DataIn$program

## multiple groupings
# DataIn$Group1<-DataIn$PrimaryMeasure
# DataIn$Group2<-substr(DataIn$C.Project.ID,1,2)
# DataIn$Group<-paste(DataIn$Group1,DataIn$Group2,sep = "-")

Enduses<-unique(DataIn$Group)
table(DataIn$Group)

Data<-select(DataIn,c(Identifier,Group,SizeVar))

# Optimization inputs; # of Strata (Max 6), which End Uses, size varaiable variation tolerance
Strata<-5
Endusesn<-c(1)
CertTol<-.05
FloatCTMax<-TRUE
ProbTol<-.05
SmartTol<-TRUE
Alert<-FALSE
Evaluate<-TRUE
Plots<-FALSE
OptimalOptions<-TRUE
Critical<-1.645
Precision<-.1
###

# Confirm tolerance adjustments if SmartTol == TRUE
SmartAdjs<-Data%>%group_by(Group)%>%summarise(sd=sd(SizeVar/sum(SizeVar)),n=n(),rat=n/sd)%>%mutate(adj=1.3/(1+exp(1)^(.5*(log(rat)-11))),newTol=adj*ProbTol)
SmartAdjs$adj[SmartAdjs$Group=="CustomElectric"]<-1.5
SmartAdjs$adj[SmartAdjs$Group=="Weatherstripping"]<-1.4
View(SmartAdjs)

for (z in 1:1){
  eval<-NULL
  r<-proc.time()
  ProbTolReset<-ProbTol
  CertTolReset<-CertTol
  for (h in Endusesn){
    ProbTol<-ProbTolReset
    CertTol<-CertTolReset
    Measure<-Enduses[h]
    Dataopt<-subset(Data,Group==Measure)%>%arrange(-SizeVar)%>%mutate(Percent=SizeVar/sum(SizeVar),Percentile=cumsum(Percent),row=1:n())
    if(SmartTol==TRUE){
      ProbTol<-ProbTolReset*subset(SmartAdjs,Group==Measure)$adj
      CertTol<-CertTolReset*subset(SmartAdjs,Group==Measure)$adj
    }
    Length<-length(Dataopt$Percent)
    Pos<-c(1:Length)
    Dataopt$Work<-Pos
    StrataSet<-Strata
    if(Strata>Length){StrataSet<-Length}
    for (n in 1:StrataSet){
      minProbTol<-1
      if(FloatCTMax==TRUE&max(Dataopt$Percent)-1/n>CertTolReset){
        CertTol<-max(Dataopt$Percent)-1/n
      }
      print(paste(Measure,n,sep = "-"))
      Dataopt$Work<-n
      if (n>1){
        start1<-min(Dataopt$row[Dataopt$Percentile>1/n-CertTol])
        Dataopt$Work[1:start1]<-1
        sum1<-sum(Dataopt$Percent[Dataopt$Work==1])
      }else{start1<-1}
      for (a in start1:(Length-n+1)){
        Dataopt$Work[a]<-1
        sum1<-sum(Dataopt$Percent[Dataopt$Work==1])
        Dataopt$Work[Dataopt$Work>1]<-n
        # print(Dataopt$Work)
        if (sum(Dataopt$Percent[Dataopt$Work==1])>(1/n+CertTol)){
          break
        }
        minProbTol<-min(minProbTol,ProbTolfun())
        if (CertTol>=CertTolfun()&(ProbTol>=ProbTolfun()|n<=2)&Evaluate){
          strataout<-strataoutfun()
          eval<-bind_rows(eval,strataout)
        }
        if (sum(Dataopt$Work)==Length){
          break
        }
        if(n<3){next}
        start2<-min(Dataopt$row[cumsum(Dataopt$Percent[Dataopt$Work>1])>(1-sum1)/(n-1)-ProbTol])
        Dataopt$Work[Dataopt$Work>1][1:start2]<-2
        for (b in max(Dataopt$row[Dataopt$Work==2]):(Length-n+2)){
          Dataopt$Work[b]<-2
          Dataopt$Work[Dataopt$Work>2]<-n
          # print(Dataopt$Work)
          if ((sum(Dataopt$Percent[Dataopt$Work==2]))/(1-sum(Dataopt$Percent[Dataopt$Work==1]))>(1/(n-1)+ProbTol)){
            break
          }
          minProbTol<-min(minProbTol,ProbTolfun())
          if ((ProbTol>=ProbTolfun()|n<=2)&Evaluate){
            strataout<-strataoutfun()
            eval<-bind_rows(eval,strataout)
          }
          if(n<4){next}
          start3<-min(Dataopt$row[cumsum(Dataopt$Percent[Dataopt$Work>2])>(1-sum1)/(n-1)-ProbTol])
          Dataopt$Work[Dataopt$Work>2][1:start3]<-3
          for (c in max(Dataopt$row[Dataopt$Work==3]):(Length-n+3)){
            Dataopt$Work[c]<-3
            Dataopt$Work[Dataopt$Work>3]<-n
            # print(Dataopt$Work)
            if ((sum(Dataopt$Percent[Dataopt$Work==3]))/(1-sum(Dataopt$Percent[Dataopt$Work==1]))>(1/(n-1)+ProbTol)){
              break
            }
            minProbTol<-min(minProbTol,ProbTolfun())
            if(ProbTol>=ProbTolfun()&Evaluate){
              strataout<-strataoutfun()
              eval<-bind_rows(eval,strataout)
            }
            if (n<5){next}
            start4<-min(Dataopt$row[cumsum(Dataopt$Percent[Dataopt$Work>3])>(1-sum1)/(n-1)-ProbTol])
            Dataopt$Work[Dataopt$Work>3][1:start4]<-4
            for(d in max(Dataopt$row[Dataopt$Work==4]):(Length-n+4)){
              Dataopt$Work[d]<-4
              Dataopt$Work[Dataopt$Work>4]<-n
              # print(Dataopt$Work)
              if ((sum(Dataopt$Percent[Dataopt$Work==4]))/(1-sum(Dataopt$Percent[Dataopt$Work==1]))>(1/(n-1)+ProbTol)){
                break
              }
              minProbTol<-min(minProbTol,ProbTolfun())
              if (ProbTol>=ProbTolfun()&Evaluate){
                strataout<-strataoutfun()
                eval<-bind_rows(eval,strataout)
              }
              if(n<6){next}
              start5<-min(Dataopt$row[cumsum(Dataopt$Percent[Dataopt$Work>4])>(1-sum1)/(n-1)-ProbTol])
              Dataopt$Work[Dataopt$Work>4][1:start5]<-5
              for(e in max(Dataopt$row[Dataopt$Work==5]):(Length-n+5)){
                Dataopt$Work[e]<-5
                Dataopt$Work[Dataopt$Work>5]<-n
                if ((sum(Dataopt$Percent[Dataopt$Work==5]))/(1-sum(Dataopt$Percent[Dataopt$Work==1]))>(1/(n-1)+ProbTol)){
                  break
                }
                minProbTol<-min(minProbTol,ProbTolfun())
                if (ProbTol>=ProbTolfun()&Evaluate){
                  strataout<-strataoutfun()
                  eval<-bind_rows(eval,strataout)
                }
              }
            }
          }
        }
      }
      # eval<-bind_rows(eval,list(EU=Measure,X1=0,X2=0,X3=0,X4=0,X5=0,X6=0,CV=1000,strata=max(Dataopt$Work),ProbTol=minProbTol,CertTol=NA))
    }
  }
  if(Alert){beep()}
  y<-proc.time()
  print(y-r)
  if (Plots){
    print("Plotting...")
    for (h in Endusesn){
      plotdata<-subset(eval,EU==Enduses[h])
      plot<-ggplot(plotdata)+
        geom_point(aes(x=ProbTol,y=FinSamp(InfSamp(CV),plotdata$X1[1]+plotdata$X2[1]+plotdata$X3[1]+plotdata$X4[1]+plotdata$X5[1]+plotdata$X6[1]),color=as.factor(strata)))+
        labs(title=paste(Enduses[h],"n =",plotdata$X1[1]+plotdata$X2[1]+plotdata$X3[1]+plotdata$X4[1]+plotdata$X5[1]+plotdata$X6[1],sep=" "),y="Sample Size",x="Strata Variance",color="# of Strata")
      print(plot)
    }
  }
  if (OptimalOptions){
    OptionsImp<-eval%>%group_by(EU)%>%mutate(EUN=max(X1),infsamp=InfSamp(CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = unique(EUN)))%>%group_by(EU,strata)%>%arrange(finsamp,ProbTol,CV)%>%ungroup()%>%mutate(row=1:nrow(eval))%>%group_by(EU,strata)%>%mutate(n=n(),min=min(row))%>%filter(row==min(row))%>%arrange(EU,strata)%>%select(c(-EUN,-row,-min))
  }else{
    OptionsImp<-eval%>%group_by(EU,strata)%>%mutate(n=n(),min=min(CV))%>%filter(CV==min)%>%mutate(infsamp=InfSamp(CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = length(Data$Group[Data$Group==unique(EU)])))%>%unique()
  }
  View(OptionsImp)
}

# optional recalculation of finite sample
Critical<-1.44
Precision<-.15

for (z in 1:1){
if (OptimalOptions){
  OptionsImp<-eval%>%group_by(EU)%>%mutate(EUN=max(X1),infsamp=InfSamp(CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = unique(EUN)))%>%group_by(EU,strata)%>%arrange(finsamp,ProbTol,CV)%>%ungroup()%>%mutate(row=1:nrow(eval))%>%group_by(EU,strata)%>%mutate(n=n(),min=min(row))%>%filter(row==min(row))%>%arrange(EU,strata)%>%select(c(-EUN,-row,-min))
}else{
  OptionsImp<-eval%>%group_by(EU,strata)%>%mutate(n=n(),min=min(CV))%>%filter(CV==min)%>%mutate(infsamp=InfSamp(CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = length(Data$Group[Data$Group==unique(EU)])))%>%unique()
}
OptionsImp$selection<-1:nrow(OptionsImp)
}

View(OptionsImp)

#Sample Design inputs; Select rows from Options, Tune Critical Value and Precison

Selection<-c(2,6,9,16,20)
Critical<-1.44
Precision<-.15
###

Results<-subset(OptionsImp,selection%in%Selection)%>%mutate(SitesPerStrata=finsamp/strata)
View(Results)

# Tune results
Results$finsamp[Results$EU=="Custom"]<-4
Results$finsamp[Results$EU=="WaterCon"]<-11
Results<-Results%>%mutate(SitesPerStrata=finsamp/strata)
View(Results)

for (z in 1:1){
Design<-NULL
DataOut<-NULL
for (i in 1:nrow(Results)){
  sel<-Results[i,]
  Dataopt<-subset(Data,Group==sel$EU)%>%arrange(-SizeVar)
  Dataopt$Work<-c(rep(1,sel$X1),rep(2,sel$X2),rep(3,sel$X3),rep(4,sel$X4),rep(5,sel$X5),rep(6,sel$X6))
  DataOut<-bind_rows(DataOut,Dataopt)
  DataoptOut<-Dataopt%>%group_by(Group,Work)%>%summarise(n=n(),mean=mean(SizeVar),sum=sum(SizeVar),sd=sd(SizeVar))
  StrataDef<-Dataopt %>% group_by(Group,Work) %>% summarise(n=n(),min=min(SizeVar),max=max(SizeVar))
  DataoptOut$sample<-0
  while(sum(DataoptOut$sample)<sel$finsamp){
    for(j in 1:nrow(DataoptOut)){
      if(DataoptOut$sample[j]>=DataoptOut$n[j]){
        next
      }
      DataoptOut$sample[j]<-DataoptOut$sample[j]+1
      if (sum(DataoptOut$sample)>=sel$finsamp){break}
    }
  }
  Design<-bind_rows(Design,DataoptOut)
}
}
View(Design)

# Tune design

# compare with previous design
previous<-bind_rows(read.csv("/volumes/Projects/457001 - New Mexico/Other/Sample Design/NMGC_EBP_additional_sites.csv",stringsAsFactors = FALSE),
  read.csv("/volumes/Projects/457001 - New Mexico/Other/Sample Design/NMGC_EBP_IDS.csv",stringsAsFactors = FALSE))

DataOut$previous<-0
DataOut$previous[DataOut$Identifier%in%previous$Rebate.Intake]<-1
previousagg<-DataOut%>%group_by(merge=paste(Group,Work,sep = "-"))%>%summarise(n=n(),complete=sum(previous))

Design$merge<-paste(Design$Group,Design$Work,sep = "-")

compare<-left_join(Design,previousagg,by="merge")%>%select(c(merge,sample,complete))%>%mutate(diff=complete-sample)


for (z in 1:1){
  Results<-cbind(Options2[Selection,1:14], matrix(data=NA, nrow = length(Selection),ncol = 1))
  colnames(Results)<-c(colnames(Results[,1:14]),"Sites/Strat")
  for (i in 1:length(Results[,1])) {
    Results[i,13]<-InfSamp(CV = Results[i,4])
    Results[i,14]<-FinSamp(InfSamp = Results[i,13],Total = Results[i,5:10])
  }
  Results[,15]<-as.numeric(Results[,14])/as.numeric(Results[,2])
  SumVect<-c()
  MeanVect<-c()
  CountVect<-c()
  StratVect<-c()
  EnduseVect<-c()
  SDVect<-c()
  for (x in Selection){
    Measure <- Options2[x,1]
    Dataopt <- Data[EndUseID==Measure ,c(ID,StratVar)]
    Dataopt<-Dataopt[rev(order(Dataopt[StratVar])),]
    Dataopt$Work <- c(rep(1, times = Options2[x,5]),rep(2, times = Options2[x,6]),rep(3, times = Options2[x,7]),rep(4, times = Options2[x,8]),rep(5, times = Options2[x,9]),rep(6, times = Options2[x,10]))
    StratVect<-c(StratVect,1:Options2[x,2])
    EnduseVect<-c(EnduseVect,rep(Options2[x,1],times=Options2[x,2]))
    CountVect<-c(CountVect,Options2[x,5:10])
    SDVectNew<-c()
    # print(Dataopt%>%group_by(Work)%>%summarise(n=n(),MG=Measure,max=max(Savings),min=min(Savings)))
    for (y in 1:StrataMax){
      SumVect<-c(SumVect,sum(Dataopt[Dataopt$Work==y,StratVar]))
      MeanVect<-c(MeanVect,mean(Dataopt[Dataopt$Work==y,StratVar]))
      SDVectNew<-c(SDVectNew, sd(Dataopt[Dataopt$Work==y,StratVar]))
      if (y==StrataMax && is.na(SDVectNew[1])==1){
        SDVectNew[1]<-0
      }
    }
    SDVect<-c(SDVect,SDVectNew)
  }
  SumVect<-SumVect[SumVect>0]
  MeanVect<-MeanVect[is.nan(MeanVect)==0]
  CountVect<-CountVect[CountVect>0]
  SDVect<-SDVect[is.na(SDVect)==0]
  Design<-cbind(EnduseVect,StratVect,CountVect,MeanVect,SumVect,SDVect,matrix(data=0, nrow = length(EnduseVect),ncol = 2))
  colnames(Design)<-c("Enduse","Stratum","Count","MeanKWH",StratVar,"SD","CV","SampTar")
  Design[,7]<-as.numeric(Design[,6])/as.numeric(Design[,4])
  row.names(Design)<-1:length(Design[,1])
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Results[,14])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
  for (y in 1:length(Results[,1])){
    while (as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
      for (x in 1:length(Design[,1])){
        if (Results[y,1]==Design[x,1]){
          if (as.numeric(Design[x,8])<as.numeric(Design[x,3]) && as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
            Design[x,8]<-(as.numeric(Design[x,8])+1)
          }
        }
      }
    }
  }
}
View(Results)

#Fine Tune/Select Sample Sizes
Results[,14]<-c(6,6,8,10,10,1)

for (z in 1:1){
  Design[,8]<-0
  for (y in 1:length(Results[,1])){
    while (as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
      for (x in 1:length(Design[,1])){
        if (Results[y,1]==Design[x,1]){
          if (as.numeric(Design[x,8])<as.numeric(Design[x,3]) && as.numeric(Results[y,14])>as.numeric(sum(as.numeric(Design[(Results[y,1]==Design[,1]),8])))){
            Design[x,8]<-(as.numeric(Design[x,8])+1)
          }
        }
      }
    }
  }
  Results[,15]<-as.numeric(Results[,14])/as.numeric(Results[,2])
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Results[,14])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
}

#Optimized Stratified Sample Design
View(Design)

#Fine Tune Design
# Design[2,8]<-3
# Design[4,8]<-2

for (z in 1:1){
  print.default("Total Sites:",quote = FALSE)
  print(TotalSites<-sum(as.numeric(Design[,8])))
  print.default("Sites per Strata:",quote = FALSE)
  print(SitesperStrat<-TotalSites/length(Design[,1]))
  FinalDesign<-Design[,c(1:5,8)]
}
View(FinalDesign)

DesignOut<-Dataopt%>%group_by(Work)%>%summarise(n=n(),Mean=mean(kwh),Sum=sum(kwh),stddev=sd(kwh))

# xyz<-bind_rows(mget(apropos("zzz")))
