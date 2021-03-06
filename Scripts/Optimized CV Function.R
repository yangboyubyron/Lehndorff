
#' Optimization of stratified sample design
#'
#' @param Data 
#' @param Identifier Character string which identifies column where individuals are identified.
#' @param SizeVar Character string which identifies numeric column that is being stratified.
#' @param Group Character string which identifies column where group membership is identified.
#' @param n_strata Integer from 1 to 6 indicating the maximum number of strata that should be attempted. More strata = more processing time.
#' @param which_groups Character vector which identfies which subset of Group should or should not be included. Use "All" for all groups and "!" to remove groups.
#' @param tolerance Numeric value between 0 and 1 indicating range of acceptable variation between strata. A value of .05 would indicate that the total size of strata of a n strata optimization could vary between 1/n +- .05.
#' @param FloatTolerance Logical indicating if individuals with very high SizeVar values can be exempted from tolerance requirement. 
#' @param SmartTolerance Logical indicating if tolerance can be modifier based on Group population and standard deviation. Allows very large groups to run faster.
#' @param OptimalOption Logical indicating if for the sample sample size the lowest tolerance option should be selected. Else the lowest CV option will be selected.
#' @param confidence Numeric indicating z-score associated with desired confidence level (1.645 = 90%, 1.284 = 80%).
#' @param precision Numeric from 0 to 1 indicating desired precision level.
#' @param Optimize Logical indicating if sample design should be optimized. FALSE produced non-optimized sample design.
#' @param Progress Logical indicating if function progress should be displayed in the console. Helpful for tracking progress of large groups.
#'
#' @return Data frame containing optimal strata based on criteria. Also outputs data used for subsequent steps to enviroment: AllStrata = all strata options pre optimization, OptData = selected data for optimization.
#' @seealso ReOptimize, PrepDesign, DesignSample
#' 
#' @export
#' @import dplyr
#'
#' @examples 
#' # Optimized sample for mtcars data grouped by cyl stratified by disp for 6 and 8 cyl cars.
#' DataforOpt <- mtcars
#' DataforOpt$ids <- row.names(DataforOpt)
#' OptimizedCars <- OptSampleDesign(Data=DataforOpt,Identifier="ids",SizeVar="disp",Group="cyl",n_strata=4,which_groups=c(8,6),tolerance=.05,confidence=1.645,precision=.1)
#' 
#' # Recalculate options at 80/20 level
#' ReOptimizedCars <- ReOptimize(confidence=1.284,precision=.2)
#' 
#' # Select strata to use for sample design
#' SelectedCarsStrata <- PrepDesign(DesignOptions=ReOptimizedCars, Selection=c(2,7))
#' 
#' # Design sample for mtcars
#' CarsSampleDesign <- DesignSample(PrepedDesign=SelectedCarsStrata)
OptSampleDesign<-function(Data,Identifier,SizeVar,Group,n_strata,which_groups="All",tolerance=.05,FloatTolerance=TRUE,SmartTolerance=TRUE,OptimalOption=TRUE,confidence=1.645,precision=.1,Optimize=TRUE,Progress=TRUE){
  require(dplyr,quietly = TRUE)
  
  Evaluate<-TRUE
  
  r<-proc.time()
  if(class(n_strata)!="numeric"|class(n_strata)!="numeric"|n_strata<1|n_strata%%1!=0){
    warning("Invalid n_strata value. Number of strata should be an integer between 1 and 6.")
    return(NULL)
  }else if(n_strata>6){
    message("Maximum number of strata per group is currently 6. Only 6 strata will be run.")
    n_strata<-6
  }
  if(class(FloatTolerance)!="logical"|class(SmartTolerance)!="logical"|class(OptimalOption)!="logical"|class(Optimize)!="logical"|class(Evaluate)!="logical"|class(Progress)!="logical"){
    warning("FloatTolerance, SmartTolerance, OptimalOption, Optimize, Evaluate, and Progress should be logical inputs. Check these inputs.")
    return(NULL)
  }
  if(tolerance>.2){
    message("Tolerance input value is high. Optimization may take a long time to process.")
  }else if (tolerance<0|class(tolerance)!="numeric"){
    warning("Invalid tolerance value. Tolerance should be a positive value between 0 and 1.")
    return(NULL)
  }

  strataoutfun<-function(){
  counts<-as.data.frame.table(table(Dataopt$Work))
  out<-list(group=Measure,X1=counts$Freq[counts$Var1==1],X2=sum(counts$Freq[counts$Var1==2]),X3=sum(counts$Freq[counts$Var1==3]),X4=sum(counts$Freq[counts$Var1==4]),X5=sum(counts$Freq[counts$Var1==5]),X6=sum(counts$Freq[counts$Var1==6]),CV=subsetsx(),strata=max(Dataopt$Work),ProbTol=ProbTolfun(),CertTol=CertTolfun())
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
  
  InfSamp<-function(Critical,Precision,CV){
    round((Critical*CV/Precision)^2,0)
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
    tol2<-max(abs(tol1-sum(tol1[1:(max(Dataopt$Work)-1)])/(max(Dataopt$Work)-1)))
    return(tol2)
  }
  
  optdata<-select(Data,Identifier=Identifier,SizeVar=SizeVar,Group=Group)
  OptData<<-optdata
  
  if(class(optdata$SizeVar)!="numeric"&class(optdata$SizeVar)!="integer"){
    warning("Size variable is not numeric. Use a different variable or convert SizeVar to numeric.")
    return(NULL)
  }
  if(n_distinct(optdata$Identifier)!=nrow(optdata)){
    warning("Not all values of the Identifier variable are unique. Does the input file need to be further aggregated?")
    return(NULL)
  }
  
  SmartAdjs<-optdata%>%group_by(Group)%>%summarise(sd=sd(SizeVar/sum(SizeVar)),n=n(),rat=n/sd)%>%mutate(adj=1.3/(1+exp(1)^(.5*(log(rat)-11))),newTol=adj*tolerance)
  SmartAdjs$newTol[is.na(SmartAdjs$newTol)]<-1
  
  if(min(which_groups=="All")==1|is.null(which_groups)){
    groups=unique(optdata$Group)
  } else if(min(gsub("!","",which_groups)%in%unique(optdata$Group))==0){
      warning(paste("Not all groups recognized. Check 'which_groups'.","Group options are ",paste(unique(optdata$Group),collapse = ", "),sep = ""))
      return(NULL)
  } else if(max(!grepl("!",which_groups))==1){
    groups=which_groups[!grepl("!",which_groups)]
  } else if(max(!grepl("!",which_groups))==0){
    groups=unique(optdata$Group)[!unique(optdata$Group)%in%(gsub("!","",which_groups))]
  }
  
  if(Optimize!=TRUE){
    Options<-NULL
    for (h in 1:n_strata){
      notopt<-optdata %>% filter(Group%in%groups) %>% group_by(Group) %>% arrange(-SizeVar) %>% mutate(Percent=SizeVar/sum(SizeVar),Percentile=cumsum(Percent)) %>% arrange(Group) %>% ungroup() %>% mutate(Work=ceiling(Percentile/(1/h)))
      notopt$Work[notopt$Percent>1/h]<-1
      notoptout<-notopt %>% group_by(group=Group) %>% summarise(X1=sum(Work==1),X2=sum(Work==2),X3=sum(Work==3),X4=sum(Work==4),X5=sum(Work==5),X6=sum(Work==6),CV=subsetsx(data = subset(notopt,Group==unique(group))),strata=max(Work),Tol="Not applicable",infsamp=InfSamp(Critical=confidence,Precision = precision,CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = (X1+X2+X3+X4+X5+X6)))
      Options<-bind_rows(Options,notoptout)
    }
    Options<-Options %>% arrange(group) %>% mutate(selection=1:n())
    exp<-paste(sort(rep(groups,times=n_strata)),1:n_strata,sep="-")
    act<-paste(Options$group,Options$strata,sep="-")
    if(sum(!exp%in%act)>0){
      message(paste("The following strata could not be run:",paste(exp[!exp%in%act],collapse = ", "),". Increase tolerance to make these strata available."))
    } else {
      message("All expected strata were run.")
    }
    return(Options)
  }

  eval<-NULL
  ProbTolReset<-tolerance
  CertTolReset<-tolerance
  
  for (h in groups){
    ProbTol<-ProbTolReset
    CertTol<-CertTolReset
    Measure<-h
    Dataopt<-subset(optdata,Group==Measure)%>%arrange(-SizeVar)%>%mutate(Percent=SizeVar/sum(SizeVar),Percentile=cumsum(Percent),row=1:n())
    if(SmartTolerance==TRUE){
      ProbTol<-subset(SmartAdjs,Group==Measure)$newTol
      CertTol<-subset(SmartAdjs,Group==Measure)$newTol
    }
    Length<-length(Dataopt$Percent)
    Pos<-c(1:Length)
    Dataopt$Work<-Pos
    StrataSet<-n_strata
    if(n_strata>Length){StrataSet<-Length}
    for (n in 1:StrataSet){
      minProbTol<-1
      if(FloatTolerance==TRUE&max(Dataopt$Percent)-1/n>CertTolReset){
        CertTol<-max(Dataopt$Percent)-1/n
      }else if (FloatTolerance==FALSE&max(Dataopt$Percent)-1/n>CertTolReset){
        break
      }
      if(Progress){
        print(paste(Measure,n,sep = "-"),quote=FALSE)
      }
      
      Dataopt$Work<-n
      if (n>1){
        start1<-min(Dataopt$row[Dataopt$Percentile>1/n-CertTol])
        end1<-max(Dataopt$row[Dataopt$Percentile<=1/n+CertTol])+1
        Dataopt$Work[1:start1]<-1
        sum1<-sum(Dataopt$Percent[Dataopt$Work==1])
      }else{
        start1<-1
        end1<-1
      }
      
      prog<-progress_estimated(length(start1:end1))
      
      for (a in start1:(Length-n+1)){
        if(Progress&n>3){
          prog$tick()$print()
        }
        if(Progress&n>3&prog$i==length(start1:end1)){
          print("",quote = FALSE)
        }
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
    }
  }
  y<-proc.time()
  print(y-r)
  if (OptimalOption){
    OptionsImp<-eval%>%group_by(group)%>%mutate(Group_n=max(X1),infsamp=InfSamp(Critical=confidence,Precision = precision,CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = unique(Group_n)))%>%group_by(group,strata)%>%arrange(finsamp,ProbTol,CV)%>%ungroup()%>%mutate(row=1:nrow(eval))%>%group_by(group,strata)%>%mutate(n=n(),min=min(row))%>%filter(row==min(row))%>%arrange(group,strata)%>%select(c(-n,-row,-min))
  }else{
    OptionsImp<-eval%>%group_by(group,strata)%>%mutate(Group_n=n(),min=min(CV))%>%filter(CV==min)%>%mutate(infsamp=InfSamp(Critical=confidence,Precision = precision,CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = length(Data$Group[Data$Group==unique(group)])))%>%unique()
  }
  exp<-paste(sort(rep(groups,times=n_strata)),1:n_strata,sep="-")
  act<-paste(OptionsImp$group,OptionsImp$strata,sep="-")
  if(sum(!exp%in%act)>0){
    message(paste("The following strata could not be run:",paste(exp[!exp%in%act],collapse = ", "),". Either there is not enough projects for these strata or tolerance needs to be increased to make these strata available."))
  } else {
    message("All expected strata were run.")
  }
  AllStrata<<-eval
  OptionsImp$selection<-1:nrow(OptionsImp)
  return(OptionsImp)
}


#' Reoptimze stratification options for different confidence/precision levels
#'
#' @param StrataData List of all possible strata options to be reoptimized. Should be set to AllStrata -- one of the additional outputs from OptSampleDesign.
#' @param OptimalOption Logical indicating if for the sample sample size the lowest tolerance option should be selected. Else the lowest CV option will be selected.
#' @param confidence Logical indicating if sample design should be optimized. FALSE produced non-optimized sample design.
#' @param precision Logical indicating if function progress should be displayed in the console. Helpful for tracking progress of large groups.
#'
#' @return Data frame containing re-optimized strata based on criteria.
#' @seealso OptSampleDesign, PrepDesign, DesignSample
#' 
#' @export 
#' @import dplyr
#'
#' @examples
#' # Optimized sample for mtcars data grouped by cyl stratified by disp for 6 and 8 cyl cars.
#' DataforOpt <- mtcars
#' DataforOpt$ids <- row.names(DataforOpt)
#' OptimizedCars <- OptSampleDesign(Data=DataforOpt,Identifier="ids",SizeVar="disp",Group="cyl",n_strata=4,which_groups=c(8,6),tolerance=.05,confidence=1.645,precision=.1)
#' 
#' # Recalculate options at 80/20 level
#' ReOptimizedCars <- ReOptimize(confidence=1.284,precision=.2)
#' 
#' # Select strata to use for sample design
#' SelectedCarsStrata <- PrepDesign(DesignOptions=ReOptimizedCars, Selection=c(2,7))
#' 
#' # Design sample for mtcars
#' CarsSampleDesign <- DesignSample(PrepedDesign=SelectedCarsStrata)
ReOptimize<-function(StrataData=AllStrata,OptimalOption=TRUE,confidence=1.645,precision=.1){
  require(dplyr,quietly = TRUE)
  
  InfSamp<-function(Critical,Precision,CV){
    round((Critical*CV/Precision)^2,0)
  }
  
  FinSamp<-function(InfSamp,Total){
    ceiling(as.numeric(InfSamp)/(1+as.numeric(InfSamp)/sum(as.numeric(Total))))
  }
  
  eval<-StrataData
  
  if (OptimalOption){
    OptionsImp<-eval%>%group_by(group)%>%mutate(Group_n=max(X1),infsamp=InfSamp(Critical=confidence,Precision = precision,CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = unique(Group_n)))%>%group_by(group,strata)%>%arrange(finsamp,ProbTol,CV)%>%ungroup()%>%mutate(row=1:nrow(eval))%>%group_by(group,strata)%>%mutate(n=n(),min=min(row))%>%filter(row==min(row))%>%arrange(group,strata)%>%select(c(-Group_n,-row,-min))
  }else{
    OptionsImp<-eval%>%group_by(group,strata)%>%mutate(n=n(),min=min(CV))%>%filter(CV==min)%>%mutate(infsamp=InfSamp(Critical=confidence,Precision = precision,CV=CV),finsamp=FinSamp(InfSamp = infsamp,Total = length(Data$Group[Data$Group==unique(group)])))%>%unique()
  }
  OptionsImp$selection<-1:nrow(OptionsImp)
  return(OptionsImp)
}


#' Prepare data for sample design
#'
#' @param DesignOptions Data frame containing strata options as defined by OptSampleDesign or ReOptimize.
#' @param Selection Numeric vector indicating which strata definitions (rows of DesignOptions) should be used for sample design.
#' @param AtLeast2 Logical indicating if at least two sample points should be in each strata (recommended). 
#'
#' @return Selected rows from DesignOptions with sample size that will be used for sample design.
#' @seealso OptSampleDesign, ReOptimize, DesignSample
#' 
#' @export
#' @import dplyr
#'
#' @examples
#' # Optimized sample for mtcars data grouped by cyl stratified by disp for 6 and 8 cyl cars.
#' DataforOpt <- mtcars
#' DataforOpt$ids <- row.names(DataforOpt)
#' OptimizedCars <- OptSampleDesign(Data=DataforOpt,Identifier="ids",SizeVar="disp",Group="cyl",n_strata=4,which_groups=c(8,6),tolerance=.05,confidence=1.645,precision=.1)
#' 
#' # Recalculate options at 80/20 level
#' ReOptimizedCars <- ReOptimize(confidence=1.284,precision=.2)
#' 
#' # Select strata to use for sample design
#' SelectedCarsStrata <- PrepDesign(DesignOptions=ReOptimizedCars, Selection=c(2,7))
#' 
#' # Design sample for mtcars
#' CarsSampleDesign <- DesignSample(PrepedDesign=SelectedCarsStrata)
PrepDesign<-function(DesignOptions,Selection,AtLeast2=TRUE){
  results<-subset(DesignOptions,selection%in%Selection) %>% mutate(SampleSize=finsamp)
  if(AtLeast2){results$SampleSize[results$finsamp<2*results$strata]<-results$strata[results$finsamp<2*results$strata]*2}
  results$SitesPerStrata<-results$SampleSize/results$strata
  return(results)
}


#' Design sample based on optimzed stratification
#'
#' @param PrepedDesign Name of file with output from PrepedDesign function or similarly structure file.
#' @param DataForOpt Data on which sample design is being performed. Should be set to OptData -- one of the additional outputs from OptSampleDesign.
#' @param SummaryLevel Select one of three options. "group" summarizes sample design for each Group. "strata" summarizes sample design for each strata within each group. "identifier" returns strata assignment for each identifier.
#'
#' @return One of three representation of the completed sample design. 
#' @seealso OptSampleDesign, ReOptimize, PrepDesign
#' 
#' @export
#' @import dplyr
#' 
#' @examples 
#' # Optimized sample for mtcars data grouped by cyl stratified by disp for 6 and 8 cyl cars.
#' DataforOpt <- mtcars
#' DataforOpt$ids <- row.names(DataforOpt)
#' OptimizedCars <- OptSampleDesign(Data=DataforOpt,Identifier="ids",SizeVar="disp",Group="cyl",n_strata=4,which_groups=c(8,6),tolerance=.05,confidence=1.645,precision=.1)
#' 
#' # Recalculate options at 80/20 level
#' ReOptimizedCars <- ReOptimize(confidence=1.284,precision=.2)
#' 
#' # Select strata to use for sample design
#' SelectedCarsStrata <- PrepDesign(DesignOptions=ReOptimizedCars, Selection=c(2,7))
#' 
#' # Design sample for mtcars
#' CarsSampleDesign <- DesignSample(PrepedDesign=SelectedCarsStrata)
DesignSample<-function(PrepedDesign,DataForOpt=OptData,SummaryLevel="strata"){
  Data<-DataForOpt
  Results<-PrepedDesign
  if(n_distinct(Results$group)!=nrow(Results)){
    message("Enduses in PrepedDesign are not unique. Unexpected results may occur.")
  }
  Design<-NULL
  DataOut<-NULL
  for (i in 1:nrow(Results)){
    sel<-Results[i,]
    Dataopt<-subset(Data,Group==sel$group)%>%arrange(-SizeVar)
    Dataopt$Work<-c(rep(1,sel$X1),rep(2,sel$X2),rep(3,sel$X3),rep(4,sel$X4),rep(5,sel$X5),rep(6,sel$X6))
    DataOut<-bind_rows(DataOut,Dataopt)
    DataoptOut<-Dataopt%>%group_by(Group,Work)%>%summarise(n=n(),mean=mean(SizeVar),sum=sum(SizeVar),sd=sd(SizeVar),min=min(SizeVar),max=max(SizeVar))
    DataoptOut$sample<-0
    while(sum(DataoptOut$sample)<sel$SampleSize){
      for(j in 1:nrow(DataoptOut)){
        if(DataoptOut$sample[j]>=DataoptOut$n[j]){
          next
        }
        DataoptOut$sample[j]<-DataoptOut$sample[j]+1
        if (sum(DataoptOut$sample)>=sel$SampleSize){break}
      }
    }
    Design<-bind_rows(Design,DataoptOut)
  }
  if(!SummaryLevel%in%c("group","strata","identifier")){
    warning("Unknown summary level. Select 'strata','group', or 'identifier'")
  } else if(sum(SummaryLevel%in%c("group","strata","identifier"))>1){
    warning("Too many summary levels. Select one of 'strata','group', or 'identifier'")
  }else if(SummaryLevel=="strata"){
      return(Design)
  }else if(SummaryLevel=="group"){
      return(DataOut %>% group_by(Group)%>%summarise(n=n(),mean=mean(SizeVar),sum=sum(SizeVar),sd=sd(SizeVar),min=min(SizeVar),max=max(SizeVar),sample=sum(Design$sample[Design$Group==unique(Group)])))
  }else if(SummaryLevel=="identifier"){
      return(DataOut)
  }
}


