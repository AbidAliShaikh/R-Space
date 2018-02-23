

analyse2 <- function ( yr,file){
library(HH)

all <- read.csv(file,header=T)
FM <- c(NULL)
for (fm in as.character(unique(all$Faculty.Member))){FM <-c(fm,FM)} 
q=1; p=47
sets = nrow(all)/47
pdf(file="abc.pdf",onefile=T)
while (p <= sets*47)
 {

  sh <- all[q:p,]
  q=q+47
  p=p+47;

sd <- rowSums(sh[,9:108]==1,na.rm=T)
d <- rowSums(sh[,9:108]==2,na.rm=T)
u <- rowSums(sh[,9:108]==3,na.rm=T)
a <- rowSums(sh[,9:108]==4,na.rm=T)
sa <- rowSums(sh[,9:108]==5,na.rm=T)
resp <- cbind ('Strongly.Agree'=sa, 'Agree'=a,'Uncertain'=u,'Disagree'=d,'Strongly.Disagree'=sd)

indicators <- c('Course-Content&Organization'=3,'Stud.Contribution'=3,'Environment'=4,'Resources'=4,'Quality.of.Delivery'=3,'Assessment'=3,'Faculty.Eval'=19,'Tutorial'=3,'Practical'=2)
ind <- c(NULL)
for (i in 1:9) ind <- c(ind, (rep(names(indicators[i]), indicators[i])))

ind[45]='Full.Time'
ind[46]='Gender'
ind[47]='Age'
row.names(resp) <- ind


print(likert(resp[c(1:24,40:44),],col=c('seagreen','Green','Yellow','Red','darkred'),ylab='Indicators', xlab='Number of Responses',main=paste("Students Course Eval Survey\n",as.character(sh$Course.Title)[1],'taught by',sh$Faculty.Member[1],'\n','Institute/Center/Department:',as.character(sh$Department)[1],'Year:',yr)))
}
dev.off()
}