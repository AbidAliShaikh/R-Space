#Faculty Evaluation 2017
file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
analyseFacEval <- function (file ){
library(HH)

Data <- read.csv(file,header=T)
Data <- Data[Data$Indicators=="Faculty.Eval",]
#FM <- c(NULL)
#for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 
#png(width=640,height=480)
for (filename in unique(Data$Department))
{
  all <- subset(Data, Data$Department == filename)
  q=1; p=19
  sets = nrow(all)/19
  
  
  pdf(file=paste(filename,".pdf",sep=""),onefile=T,,width=14,height=11,)
  while (p <= sets*19)
  {
    
    sh <- all[q:p,]
    q=q+19
    p=p+19;
    
sh <- sh[order(rowSums(sh[,9:180],na.rm=T), decreasing=T),]
sd <- rowSums(sh[,9:180]==1,na.rm=T)
d <- rowSums(sh[,9:180]==2,na.rm=T)
u <- rowSums(sh[,9:180]==3,na.rm=T)
a <- rowSums(sh[,9:180]==4,na.rm=T)
sa <- rowSums(sh[,9:180]==5,na.rm=T)
resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Somewhat agree'=u,'Disagree'=d,'Strongly.Disagree'=sd)




 
  row.names(resp) <- as.character(sh$Questions)
#png(paste(fm,".png",collapse=""),width=800,height=600)
  print(plot.likert(resp[c(1:18),],ReferenceZero=3,col=c('blue','royalblue','yellow','red','red2'),ylab='Indicators', xlab='Number of Responses',main=paste("Teacher/Faculty Evaluation\n",as.character(sh$Course.Title)[1],'taught by',sh$Faculty.Member[1],'\n','Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr)))

}
dev.off()
}
}
###############The above function with PIE outputs
FAC_EVAL_TO_PIE() <- function (file ){
  library(plotrix)
  
  Data <- read.csv(file,header=T)
  Data <- Data[Data$Indicators=="Faculty.Eval",]
  #FM <- c(NULL)
  #for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 
  #png(width=640,height=480)
  for (filename in unique(Data$Department))
  {
    all <- subset(Data, Data$Department == filename)
    q=1; p=19
    sets = nrow(all)/19
    
    
    pdf(file=paste(filename,".pdf",sep=""),onefile=T,width=11,bg = "cyan")
    while (p <= sets*19)
    {
      
      sh <- all[q:p,]
      q=q+19
      p=p+19;
      
      sh <- sh[order(rowSums(sh[,9:180],na.rm=T), decreasing=T),]
      sd <- rowSums(sh[,9:180]==1,na.rm=T)
      d <- rowSums(sh[,9:180]==2,na.rm=T)
      u <- rowSums(sh[,9:180]==3,na.rm=T)
      a <- rowSums(sh[,9:180]==4,na.rm=T)
      sa <- rowSums(sh[,9:180]==5,na.rm=T)
      resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Somewhat agree'=u,'Disagree'=d,'Strongly.Disagree'=sd)
      total = colSums(resp[c(1:18),])
      percents = round(prop.table(total)*100,1)
      percents = percents[percents != 0.0]
      lab=paste0(names(percents),":", percents,"%")
      m=paste("\nFaculty Evaluation Survey\n",as.character(sh$Course.Title)[1],'taught by',sh$Faculty.Member[1],'\n','Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr,'\nPercents of Responses')
      cols = c('blue','royalblue','yellow','red','red2')
      
      pie3D(percents,col = cols, labels= lab,main=m,labelcex = 1)
      #legend("bottomright",lab,cex=1,fill=cols,title = "Percents of Responses")
      
    }
    dev.off()
  }
}


summarise <- function(){
  file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
  library(likert);library(dplyr);library(reshape2)
  Data <- read.csv(file,header=T,colClasses=c(rep("factor",8),rep("integer",188)))
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""),as.character(Data$Qshort[45:47]))
  all <- Data[(Data$Qshort %in% ptrn),];all$Qshort <- factor(all$Qshort);all$Indicators<- factor(all$Indicators);
    all$Questions<- factor(all$Questions)
  rb <- matrix(rep(NA,18),ncol=18)
  
  for (i in seq(from=0,to=nrow(all)-18, by=18)) { rb <-rbind(rb,t(all[(i+1):(i+18),c(7,9:170)]))}
  colnames(rb) <- rb[2,];  rb <- rb[-c(1:2),];rb <- as.data.frame(rb)
  r = sapply(rb, function(x){factor(x,levels=c(1,2,3,4,5))}); r <- as.data.frame(r);row.names(r)<-NULL
  
  lik<- likert(r[,-c(16:18)]);likert.heat.plot(lik)+ggtitle("HEAT PLOT OF FACULTY/TEACHER EVALUATION @UNIVERSITY OF SINDH")
  likert.bar.plot(lik)+ggtitle("STACKED-BAR PLOT OF FACULTY/TEACHER EVALUATION @UNIVERSITY OF SINDH")
  
}

data_mining <- function() {
  file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
  library(likert);library(dplyr);library(reshape2)
  Data <- read.csv(file,header=T,colClasses=c(rep("factor",8),rep("integer",188)))
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  #Data <- Data [-which(Data==-9),] 
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""),as.character(Data$Qshort[45:47]))
  all <- Data[(Data$Qshort %in% ptrn),];all$Qshort <- factor(all$Qshort);all$Indicators<- factor(all$Indicators)
    all$Questions <- factor (all$Questions)
  melted <- melt(all,na.rm=T)
  melted <- melted[,-c(1,7:9)]
  melted$Department <- abbreviate(melted$Department)
  melted$Indicators <- abbreviate(melted$Indicators)
  melted <- melted[!(melted$Qshort %in% c("GEND","FPT","AGEGRP")),]; melted$Qshort <- factor(melted$Qshort)
  ag <- aggregate(melted[,6],by=list(melted$Department,melted$Qshort),FUN=mean)
  names(ag)<- c("Dept","Qshort","Mean")
  library(lattice)
  barchart(data=ag,main="Faculty/Teacher Evaluation Response in various Departments", Mean ~ reorder(Qshort, Mean) | Dept, scales= list(x=list(rot=90)))
  bwplot(data=ag,main="Faculty/Teacher Evaluation Response in various Departments", Mean ~ reorder(Dept, Mean), scales= list(x=list(rot=90)))
  
  
  #b <- melted %>% group_by(Department,Course.Title,Faculty.Member,Indicators) %>% summarize(n=n(),mean_value=mean(value,na.rm=T)) %>% rename(Program=Department)
  #b %>% ggplot(aes(Program,mean_value,fill=Faculty.Member)) + geom_bar(stat="identity",show.legend=F) +coord_flip()+facet_wrap(~Indicators,nrow=1)
  data=dcast(data=ag,formula=Dept~ Qshort)
  corr = round(cor(data[,-1]),2)
  library(ggcorrplot)
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("magenta", "white", "cyan"), 
             title="Correlogram of Faculty Evaluation questions in all Surveyed Departments", 
             ggtheme=theme_classic)
  
  
  
  library(gmodels)
  ct1<-CrossTable(melted$Qshort,melted$value)
  ct2<-CrossTable(melted$Department,melted$value)
  mosaicplot(ct1$t,shade=T,las=2,color=c('seagreen','Green','Yellow','Red','darkred'))
  mosaicplot(ct2$t,shade=T,las=2,color=c('seagreen','Green','Yellow','Red','darkred'))
  
  ft1 <- ftable(value ~ Qshort, data=melted)
  ft2 <- ftable(value ~ Department, data=melted)
  library(HH); 
  print(likert(ft1),color=c('seagreen','Green','Yellow','Red','darkred'),ylab='Questions', xlab='Number of Responses',main="")
  print(likert(ft2))
  
  
  
  #m<-dcast(melted,formula=Indicators ~ Department,fun.aggregate=mean)
  library(party)
  mm <- aggregate(melted,by=list(melted$Department,melted$Qshort),FUN=mean,na.rm=T)
  names(mm)[1:2] <- c("Depts","Qshort"); mm$Depts <- factor(mm$Depts);mm$Qshort <- factor(mm$Qshort)
  m.tree <- ctree(value ~ Depts * Qshort, data=mm)
  plot(m.tree)
}
