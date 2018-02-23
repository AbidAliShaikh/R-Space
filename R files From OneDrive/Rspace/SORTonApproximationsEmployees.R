f <- read.csv("employee.csv",header=T)
val2=NULL
for (i in f[,5])
{
  val=agrep(i,f[,5])
   val2=c(val2,val)
   
}
val2=unique(val2)

f=f[val2,]
write.csv(f,"e.csv")
