file = "c:/Users//Abid/OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data from Google Forms.csv"
Q = "c:/Users//Abid/OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Qshort.csv"
#output = "c:/Users//Abid/OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Stud CEQ2017-Google Forms.csv"
f <- read.csv(file,header=T)

#cleaning
#f$Timestamp <- as.Date(f$Timestamp,"%m/%d/%Y")
#f = f[grep("(2015)|(2016)",f$Timestamp),]
levs = c("Strongly Disagree","Disagree","Uncertain","Agree","Strongly Agree")
for (i in 8:35) f[,i] <- factor(f[,i],levels=levs)
#f$Academic.rank.<- factor(f$Academic.rank.,levels=c("Lecturer","Assistant Professor","Associate Professor","Professor"))
#*********
fheat <- f
Q <- read.csv(Q, header=T)
names(fheat)[8:35] <- as.character(Q$Qshort)

library(likert)
lik <- likert(fheat[,8:35])
likert.heat.plot(lik,centered=F,ordered=T,plot.percent.neutral=F)+ggtitle("Heat Plot of Students Course Eval Questionnaire 2017 @University of Sindh\n Data from Google Forms")


for (i in f$Department.Name) {
  flik <- subset(f, f$Department.Name%in%i)
  lik1 <- likert(flik[8:35])
pdf(file="output.pdf",width=8,height=11*length(levels(f$Department.Name)),onefile=T)
likert.bar.plot(lik1,centered=F,ordered=T,plot.percent.neutral=F)+ggtitle(paste(i,"\nStacked-Bar Likert Chart\nStudents Course Eval Questionnaire 2017 -Google Forms"))
dev.off()
}
jpeg(filename="faculty survey 2015/lbp2.jpg",width=600,height=800)
likert.bar.plot(lik2,centered=F,ordered=T,plot.percent.neutral=F)+ggtitle("Faculty Survey during 2015")
dev.off()


###########Boot Strapping with this limited data
sampleMean <- function (population, indicator,year)
{
  population.mean= mean(population,na.rm=T)
  B=1000
  n= length(population)
  boot.samples = matrix(sample(population, size=B*n, replace=T),nrow=B,ncol=n)
  
  boot.statistics = apply(boot.samples,1,mean,na.rm=T)
  sample.se = sd(boot.statistics,na.rm=T)
  
  me = ceiling(10*2*sample.se)/10
  
  bst= unlist((round(population.mean,1)+ c(1,-1)*me))
  hist(boot.statistics,100, main=paste("Histogram of boot statistic of ",indicator,"Year:",year),xlim=c(2.6,4.4),xlab=paste("Boot statistics: Upper ",bst[1]," Lower ",bst[2],""))
  
}
f <- read.csv(file,header=T,colClasses="character")
ft = f

f=f[,8:35]
f[f=="Strongly Agree"] <- 5
f[f=="Agree"] <- 4
f[f=="Uncertain"] <- 3
f[f=="Disagree"] <- 2
f[f=="Strongly Disagree"] <- 1


par(mfrow=c(4,1))
#pdf(file=auto.pdf,width=11,height=60,onefile=T)
for ( i in names(f)){
  
  sampleMean(as.integer(f[,names(f)==i]),i,2017)
  
}
#dev.off()
