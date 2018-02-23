file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"

#library(HH)
  Data <- read.csv(file,header=T)
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  do.call(Departs,Data)
  
  
Departs<- function(Data){
  yr=2017
  for (filename in unique(Data$Department))
  {
    sh <- subset(Data, Data$Department == filename)
    sd <- rowSums(sh[,9:180]==1,na.rm=T)
    d <- rowSums(sh[,9:180]==2,na.rm=T)
    u <- rowSums(sh[,9:180]==3,na.rm=T)
    a <- rowSums(sh[,9:180]==4,na.rm=T)
    sa <- rowSums(sh[,9:180]==5,na.rm=T)
    resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Uncertain'=u,'Disagree'=d,'Strongly.Disagree'=sd)
    
    indicators <- c('Course-Content&Organization'=3,'Stud.Contribution'=3,'Environment'=4,'Resources'=4,'Quality.of.Delivery'=3,'Assessment'=3,'Faculty.Eval'=19,'Tutorial'=3,'Practical'=2)
    ind <- c(NULL)
    for (i in 1:9) ind <- c(ind, (rep(names(indicators[i]), indicators[i])))
    
    ind[45]='Full.Time'
    ind[46]='Gender'
    ind[47]='Age'
    row.names(resp) <- rep(ind,nrow(resp)/47)
    
    resp1 <- resp[!row.names(resp) %in% c("Faculty.Eval","Full.Time","Gender","Age"),]
    
    print(likert(resp1,col=c('seagreen','Green','Yellow','Red','darkred'),ylab='Indicators', xlab='Number of Responses',main=paste("Students Course Eval Survey\n",'Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr)))
  }
}
