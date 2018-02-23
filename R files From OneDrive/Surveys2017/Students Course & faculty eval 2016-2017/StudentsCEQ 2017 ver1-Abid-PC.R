
file="c:/Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
yr=2017

file ="~/Documents/Data.csv"
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
#############PIE CHARTS INSTEAD OF STACKED-BAR PLOTS IN ABOVE FUNCTION ... MODIFIED VER BELOW
CEQ_TO_PIE <- function ( yr,file){
  library(plotrix)
  
  Data <- read.csv(file,header=T)
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  
  
  for (filename in unique(Data$Department))
  {
    all <- subset(Data, Data$Department == filename)
    q=1; p=47
    sets = nrow(all)/47
    
    
    pdf(file=paste(filename,".pdf",sep=""),onefile=T,width = 11)
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
      total = colSums(resp[c(1:24,40:44),])
      percents = round(prop.table(total)*100,1)
      percents = percents[percents != 0.0]
      lab=paste0(names(percents),":", percents,"%")
      m=paste("\nStudents Course Eval Survey\n",as.character(sh$Course.Title)[1],'taught by',sh$Faculty.Member[1],'\n','Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr,'\nPercents of Responses')
      cols = c('seagreen','Green','Yellow','Red','darkred')
      pie3D(percents,col = cols, labels= lab,main=m,labelcex = 1)
      
      #pie(percents,col = cols, labels= "",main=m)
      
      #legend(.9,.1,lab,cex=1,fill=cols,title = "Percents of Responses")
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

data_mining <- function() {
  file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
  library(likert);library(dplyr);library(reshape2)
  Data <- read.csv(file,header=T,colClasses=c(rep("factor",8),rep("integer",188)))
  Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
  #Data <- Data [-which(Data==-9),] 
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""),as.character(Data$Qshort[45:47]))
  all <- Data[!(Data$Qshort %in% ptrn),];all$Qshort <- factor(all$Qshort);all$Indicators<- factor(all$Indicators)
  melted <- melt(all,na.rm=T)
  melted <- melted[,-c(1,7:9)]
  melted$Department <- abbreviate(melted$Department)
  melted$Indicators <- abbreviate(melted$Indicators)

  ag <- aggregate(melted[,6],by=list(melted$Department,melted$Indicators),FUN=mean)
  names(ag)<- c("Dept","Ind","Mean")
  library(lattice)
  bwplot(data=ag,main="Students' Course Evaluation Response in various Departments", Mean ~ reorder(Ind, Mean) | Dept, scales= list(x=list(rot=90)))
  bwplot(data=ag,main="Students' Course Evaluation Response in various Departments", Mean ~ reorder(Dept, Mean), scales= list(x=list(rot=90)))
  
  
  #b <- melted %>% group_by(Department,Course.Title,Faculty.Member,Indicators) %>% summarize(n=n(),mean_value=mean(value,na.rm=T)) %>% rename(Program=Department)
    #b %>% ggplot(aes(Program,mean_value,fill=Faculty.Member)) + geom_bar(stat="identity",show.legend=F) +coord_flip()+facet_wrap(~Indicators,nrow=1)
  data=dcast(data=ag,formula=Dept~ Ind)
  corr = round(cor(data[,-1]),2)
  library(ggcorrplot)
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"), 
             title="Correlogram of Indicators in all Surveyed Departments", 
             ggtheme=theme_bw)
  
  
  
  library(gmodels)
  ct1<-CrossTable(melted$Indicators,melted$value)
  ct2<-CrossTable(melted$Department,melted$value)
  mosaicplot(ct1$t,shade=T,las=2,color=c('seagreen','Green','Yellow','Red','darkred'))
  mosaicplot(ct2$t,shade=T,las=2,color=c('seagreen','Green','Yellow','Red','darkred'))
  
  ft1 <- ftable(value ~ Indicators, data=melted)
  ft2 <- ftable(value ~ Department, data=melted)
  library(HH); 
  print(likert(ft1),color=c('seagreen','Green','Yellow','Red','darkred'),ylab='Indicators', xlab='Number of Responses',main="")
  print(likert(ft2))
  
  
  
  #m<-dcast(melted,formula=Indicators ~ Department,fun.aggregate=mean)
  library(party)
  mm <- aggregate(melted,by=list(melted$Department,melted$Indicators),FUN=mean,na.rm=T)
  names(mm)[1:2] <- c("Depts","Ind"); mm$Depts <- factor(mm$Depts);mm$Ind <- factor(mm$Ind)
  m.tree <- ctree(value ~ Depts * Ind, data=mm)
  plot(m.tree)
}

genderpyramid <- function()
{
  data_mining <- function() {
    file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"
    library(likert);library(dplyr);library(reshape2)
    Data <- read.csv(file,header=T,colClasses=c(rep("factor",8),rep("integer",188)))
    Data[Data$Q ==4, 9:180] <- 6-Data[Data$Q==4,9:180]
    #Data <- Data [-which(Data==-9),] 
    all <- Data
    melted <- melt(all,na.rm=T)
    melted <- melted[,-c(1,7:9)]
    melted$Department <- abbreviate(melted$Department)
    melted$Indicators <- abbreviate(melted$Indicators)
    #following is gender-wise pyramid
    melted %>% select(Department, Qshort, value) %>% filter (Qshort == "GEND") %>%
      dcast(formula=Department + Qshort ~ value) %>% select(c(1,2,6,7)) -> A
    names(A)[3:4] <- c("F","M")
    A <- melt(A); A <- A[,-2]
    names(A)[2:3] <- c("Gender","num")
    library(ggthemes)
    brks <- seq(-800,800,200)
    lbls = paste0(as.character(c(seq(800, 0, -200), seq(200, 800, 200))))
    A[A$Gender=="M",3]= -A[A$Gender=="M",3]
    options(scipen = 999)  # turns of scientific notations like 1e+40
    
    ggplot(A, aes(x = reorder(Department,-abs(num),mean), y = num, fill = Gender)) +   # Fill column
      geom_bar(stat = "identity", width = .6) +   # draw the bars
      scale_y_continuous(breaks = brks,   # Breaks
                            labels = lbls) + # Labels
      coord_flip() +  # Flip axes
      labs(title="Gender-wise Respondents") +
      theme_tufte() +  # Tufte theme from ggfortify
      theme(plot.title = element_text(hjust = .5), 
            axis.ticks = element_blank()) +   # Centre plot title
      scale_fill_brewer(palette = "Dark2")  # Color palette
    
    #Population Pyramid With Ggplot
    
}
