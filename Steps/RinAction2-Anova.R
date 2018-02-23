# Anova -car package

library(multcomp)
attach(cholesterol)
av1 <- aov (response ~ trt) # indicating that tests differ in one or more ways
detach("package:HH") # iff HH is loaded it will interfere with TukeyHSD test
TukeyHSD(av1) # the pairwise distinction
par(mar=c(5,8,4,2))
plot(TukeyHSD(av1),las=2,mar=c(5,8,4,2), col='purple')

#ANCOVA example
data(litter, package='multcomp')
summary(litter)
attach(litter)
aggregate(weight, by =list(dose),FUN=mean)
av2 <- aov(weight ~ gesttime + dose,data=litter)
summary.lm(av2)

















expand_litter <- function(litter){
j = data.frame(NA,NA,NA,NA)
names(j) <- names(litter)
for (i in 1:nrow(litter)) 
{
  for (k in 1:litter[i,4])
  {
    j = rbind(litter[i,],j)
  }
}
j <- j[-nrow(j),-4]
return (j)
}