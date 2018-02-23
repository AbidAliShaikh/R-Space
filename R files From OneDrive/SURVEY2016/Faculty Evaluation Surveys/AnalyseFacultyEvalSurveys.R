
analyseFacEval <- function ( yr){
library(HH)

all <- read.csv('All Proforma Data (Faculty Survey).csv',header=T)
FM <- c(NULL)
for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 
#png(width=640,height=480)
for (fm in FM) {

  sh <- subset(all, all$Faculty.Member== fm & all$Year == yr )
print(fm)
sh <- sh[order(rowSums(sh[,9:108],na.rm=T), decreasing=T),]
sd <- rowSums(sh[,9:108]==1,na.rm=T)
d <- rowSums(sh[,9:108]==2,na.rm=T)
u <- rowSums(sh[,9:108]==3,na.rm=T)
a <- rowSums(sh[,9:108]==4,na.rm=T)
sa <- rowSums(sh[,9:108]==5,na.rm=T)
resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Somewhat agree'=u,'Disagree'=d,'Strongly.Disagree'=sd)


 
  row.names(resp) <- as.character(sh$Question)

  print(plot.likert(resp[c(1:18),],ReferenceZero=3,col=c('blue','royalblue','yellow','red','red2'),ylab='Indicators', xlab='Number of Responses',main=paste("Faculty Evaluation Survey\n",fm,'\n','Department:',as.character(sh$Department)[1],'Year:',yr)))
  
}
#dev.off()
}