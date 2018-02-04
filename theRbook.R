library(ggplot2)
library(reshape2)
s <- read.table('crawley/sales.txt',header=T)
s <- melt(s)
names(s)[2:3] <-c('season','sales') 

interaction.plot(s$season,s$name,s$sales)
#qplot( name, sales, data=s,stat='identity')+geom_bar(stat='bin')+geom_point(aes(color=season))
              
s <- read.table('crawley/scatter1.txt',header=T)
smoothScatter(s,nbin = 20,colramp=colorRampPalette(c('white',blues9)))
smoothScatter(s,bandwidth=seq(1,100,.01),colramp=colorRampPalette(c('white',blues9)))
smoothScatter(s,colramp=colorRampPalette(c('orange',blues9)))
smoothScatter(s,bandwidth=seq(1,100,.01),colramp=colorRampPalette(c('white','skyblue','blue','darkblue'),space='Lab'))

###fun with colorRampPallete
d <- colorRampPalette(c('purple','yellow','blue'))
plot(s[order(s$xv),],col=d(2000))

barplot(rep(3,30),col=d(30))
d <- colorRampPalette(c('purple','green','blue'))
barplot(rep(3,50), col=d(50)) #not much here
  
#################
#x11()
g <- ggplot(s, aes(x=s$xv,y=s$ys))+geom_smooth(method='lm',size=3,colour='blue')
g <- g+geom_point(colour='skyblue',alpha=1/2)

s2 <- read.table('crawley/scatter2.txt',header=T)
  
g+geom_point(aes(s2$xv2,s2$ys2))  
### or else using smoothScatter
smoothScatter(s,bandwidth=seq(1,100,.01),colramp=colorRampPalette(c('white','skyblue','blue','darkblue'),space='Lab'))
abline(lm(s$ys~s$xv))
points(s2$xv2, s2$ys2,col='green')
abline(lm(s2$ys~s2$xv))
##############
  
sl <- read.table('crawley/sleep.txt',header=T)
attach(sl)
  
qplot(factor(Days), Reaction )  +geom_boxplot(fill=d(length(unique(Days))))  +geom_smooth(method='lm',aes(group=1))#+geom_point(aes(x=factor(Days),y=mean),colour='red',size=3)  

  plot(factor(Days), Reaction,col=rainbow(10))
  abline(lm(Reaction ~ (Days)))
    
#########################################333
pgr <- read.table('crawley/pgr.txt',header=T)
attach(pgr)
qplot(hay,pH,size=as.integer(FR))+geom_point( color='steelblue',aes(text(hay,pH,labels=FR)))+geom_smooth()
#######################################

w <- read.table('crawley/SilwoodWeather.txt',header=T)
 attach(w)
  qplot( factor(month),lower)+geom_boxplot()  +geom_boxplot(aes(factor(month),upper))
  
  g <- ggplot(w,aes(factor(month),lower))+geom_boxplot()
  g+geom_boxplot(aes(factor(month),lower))+geom_boxplot(aes(factor(month),upper))+geom_abline()
  #windows()
  g+geom_bin2d(aes(factor(month),lower))
    g+geom_jitter(aes(factor(month),lower))
  g+geom_raster(aes(factor(month),lower))
  
  model <- aov(w$lower ~ factor(w$month))
  pdf(file='tukey.pdf',height=50)
  plot(TukeyHSD(model))
  dev.off()
  ############################333
  
  oz <- read.table('crawley/ozone.data.txt',header=T)
  
  #***************
  
  daph <- read.table('crawley/daphnia.txt',header=T)
  interaction.plot(daph$Daphnia,daph$Detergent,daph$Growth.rate)
  interaction.plot(daph$Detergent,daph$Daphnia,daph$Growth.rate)
  library(lattice)
  windows()
  bwplot(daph$Growth.rate ~ daph$Water+daph$Detergent|daph$Daphnia)
  bwplot(daph$Growth.rate ~ daph$Water+daph$Daphnia|daph$Detergent)
#**********************
  d <- read.table('crawley/das.txt',header=T)
  shapiro.test(d$y) #p > .05 hence it is normaly distributed
  
#*********************************
  vartest <- read.table('crawley/f.test.data.txt',header=T)
  boxplot(vartest,notch=T)
  var.test(vartest$gardenB,vartest$gardenC)
  #small p value means variances are significantly different
# hence we can not use student t test to compare sample means
#***********************************
  #TESTING MORE THAN 2 VARIANCES
  r <- read.table('crawley/refuge.txt',header=T)
  plot(r$B, r$T)
  bartlett.test(r$B,r$T)
  # p=.06741 so there is no difference bw variances among all levels
  model <- lm(r$B ~ r$T)
  plot(model)
  # there is slight pattern in residuals; but not significant
  # therefore flinger.test test catches it as p=.04
  # flinger.test is bobust 
  ozone <- read.table('crawley/gardens.txt',header=T)
  boxplot(ozone,notch=T)
  bartlett.test(ozone)  #or
  bartlett.test(list(ozone$gardenA,ozone$gardenB, ozone$gardenC))
  # p value of .0002296 shows there is significant difference may be due to outliers so we can use wilcox test instead of t.test
  wilcox.test(ozone$gardenA,ozone$gardenB)
  #wilcox text is more robust; the warning is not to be wooried about
  
  ############but when data is paired e.g.
  streams <- read.table('crawley/streams.txt',header=T)
  # samples taken from up stream and down streams of the same river
  # no need to do var.test since data is paired
  t.test(streams$down,streams$up,paired=T)
  # p = .0081 shows sewage has impact on bioderdiversity
  # if we ignored paired=T the p is non-significant
  #likewisely
  t.test(streams$up-streams$down) #i.e. one sample t.test
  # p = .0081 here too
  # the mean diff =-0.875 indicates that downstream is greater 0.875 on average
#*************************************#
  #CHISQ TEST
  h= ceiling(runif(100,0,6)) # throw dice 100 times to test if it is a fair coin
  a = table(h)
  chisq.test(a)
  # p value=.3101 means its a fair dice;
  
#*******************
  #******************
  p <- read.table('crawley/paired.txt',header=T)
  cor.test(p$Upstream,p$Downstream)
  # the correlation .882 is highly significant 
  #... since p = .001652
  
  #PROBLEMS WITH COR
  #The following example is of number of species y in forests f of variace
  #... productivities x
  p <- read.table('crawley/productivity.txt',header=T)
  plot(p$x,p$y,ylab='Mammal Species',xlab='Productivity')
  cor(p$x,p$y)
  # cor of .72 means they are positively correlated but wait...
  library(lattice)
  xyplot(y ~ x | f,data=p, panel = function(x,y){
    panel.xyplot(x,y)
    panel.abline(lm(y~x))
  })
  # now they are negatively correlated in various forests
  #... so be careful while using cor in large datasets
  
  
  
  
  
  