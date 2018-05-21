library(wordnet)
WNHOME<-"/users/lehndorff/WordNet-3.0/dict/"
initDict(pathData = WNHOME)
setDict(pathData = WNHOME)
start<-"mark"
list<-c(start)
letters<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
for(i in 1:10){
    inword<-list[1]
    randchar<-letters[sample(1:length(letters),size=length(letters),replace = FALSE)]
    charnums<-sample(1:nchar(start),size=nchar(start),replace = FALSE)
    print(inword)
    newword<-NULL
    nextword<-0
    for (charnum in charnums){
      if(nextword==1){next}
      for(char in randchar){
        if(nextword==1){next}
        if(charnum==1){testword<-paste(char,substr(inword,2,2),substr(inword,3,3),substr(inword,4,4),sep="")}
        if(charnum==2){testword<-paste(substr(inword,1,1),char,substr(inword,3,3),substr(inword,4,4),sep="")}
        if(charnum==3){testword<-paste(substr(inword,1,1),substr(inword,2,2),char,substr(inword,4,4),sep="")}
        if(charnum==4){testword<-paste(substr(inword,1,1),substr(inword,2,2),substr(inword,3,3),char,sep="")}
        if(charnum==5){testword<-paste(substr(inword,1,1),substr(inword,2,2),substr(inword,3,3),char,sep="")}
        filter <- getTermFilter("ExactMatchFilter", testword, TRUE)
        if(!is.null(getIndexTerms(c("NOUN"), 1, filter))){
          newword<-testword
        }else if(!is.null(getIndexTerms(c("VERB"), 1, filter))){
          newword<-testword
        }else if(!is.null(getIndexTerms(c("ADJECTIVE"), 1, filter))){
          newword<-testword
        }else if(!is.null(getIndexTerms(c("ADVERB"), 1, filter))){
          newword<-testword
        }
        if(!is.null(newword)){
          if(!newword%in%list){
            list<-c(newword,list)
            nextword<-1
            next
          }
        }
    }
  }
}

