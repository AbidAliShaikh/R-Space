

analyse2 <- function ( yr,file){
library(HH)

Data <- read.csv(file,header=T)
Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]

#FM <- c(NULL)
#for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 

for (filename in unique(Data$Department))
{
  all <- subset(Data, Data$Department == filename)
  q=1; p=47
  sets = nrow(all)/47
  
 
  pdf(file=paste(filename,".pdf",sep=""),onefile=T)
while (p <= sets*47)
 {

  sh <- all[q:p,]
  q=q+47
  p=p+47;

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
row.names(resp) <- ind


print(likert(resp[c(1:24,40:44),],col=c('seagreen','Green','Yellow','Red','darkred'),ylab='Indicators', xlab='Number of Responses',main=paste("Students Course Eval Survey\n",as.character(sh$Course.Title)[1],'taught by',sh$Faculty.Member[1],'\n','Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr)),)
}
dev.off()
}
}


analyse_depts <- function ( yr,file){
  library(HH)
  
  Data <- read.csv("c:/Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv",header=T)
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  
  #FM <- c(NULL)
  #for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 
  pdf(file="departments.pdf",onefile=T)
  for (filename in unique(Data$Department))
  {
    all <- subset(Data, Data$Department == filename)
    ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""),as.character(all$Qshort[45:47]))
    all <- all[!(all$Qshort %in% ptrn),]
    q=1; p=nrow(all)
    
      sh=all    
      sd <- rowSums(sh[,9:180]==1,na.rm=T)
      d <- rowSums(sh[,9:180]==2,na.rm=T)
      u <- rowSums(sh[,9:180]==3,na.rm=T)
      a <- rowSums(sh[,9:180]==4,na.rm=T)
      sa <- rowSums(sh[,9:180]==5,na.rm=T)
      resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Uncertain'=u,'Disagree'=d,'Strongly.Disagree'=sd)
      resp <- as.data.frame(resp)
      indicators <- c('Course-Content&Organization'=3,'Stud.Contribution'=3,'Environment'=4,'Resources'=4,'Quality.of.Delivery'=3,'Assessment'=3,'Faculty.Eval'=4,'Tutorial'=3,'Practical'=2)
      ind <- c(NULL)
      for (i in 1:9) ind <- c(ind, (rep(names(indicators[i]), indicators[i])))
      
      grp <- rep(ind,nrow(resp)/29) #29 questionnaires after excluding 15 fac eval questions & last 3
      
    aggr_resp=aggregate(resp,by=list(grp),FUN=sum)
      row.names(aggr_resp) <- aggr_resp[,1]; aggr_resp[,1] <- NULL
      tt <- t(aggr_resp)
      
    colnames(tt)[1]<- rep(row.names(tt),tt[,1],each=T)
      print(likert(aggr_resp,col=c('seagreen','Green','Yellow','Red','darkred'),ylab='Indicators', xlab='Number of Responses',main="Students Course Eval Survey\n",group=sh$Indicators))
    
  }
    dev.off()
  }
}
#####################################
summarise <- function(){
  file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
  library(likert);library(dplyr);library(reshape2)
  Data <- read.csv(file,header=T,colClasses=c(rep("factor",8),rep("integer",188)))
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""),as.character(Data$Qshort[45:47]))
  all <- Data[!(Data$Qshort %in% ptrn),];all$Qshort <- factor(all$Qshort);all$Indicators<- factor(all$Indicators)
  rb <- matrix(rep(NA,29),ncol=29)
  
  for (i in seq(from=0,to=nrow(all)-29, by=29)) { rb <-rbind(rb,t(all[(i+1):(i+29),c(2,9:170)]))}
  colnames(rb) <- rb[2,];  rb <- rb[-c(1:2),];rb <- as.data.frame(rb)
  r = sapply(rb2, function(x){factor(x,levels=c(1,2,3,4,5))}); r <- as.data.frame(r);row.names(r)<-NULL
  
  lik<- likert(r);likert.heat.plot(lik)+ggtitle("HEAT PLOT OF STUDENTS' COURSE EVALUATION @UNIVERSITY OF SINDH")
  likert.bar.plot(lik)+ggtitle("BAR PLOT OF STUDENTS' COURSE EVALUATION @UNIVERSITY OF SINDH")

}
