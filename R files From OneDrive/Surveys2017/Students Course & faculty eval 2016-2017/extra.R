file="c://Users//Abid//OneDrive/Documents/Surveys2017/Students Course & faculty eval 2016-2017/Data.csv"

  Data <- read.csv(file,header=T,colClasses = c(rep("factor",8),rep("integer",188)))
  Data[Data$Q ==4, 9:196] <- 6-Data[Data$Q==4,9:196]
  
  library (reshape2)
  library (dplyr)

d <- melt(Data[,1:170])
d[grep("44",d[,10]),10] <- NA
d[grep("6",d[,10]),10] <- NA

u <- unique(d$Indicators);u <- as.character(u[1:9])
d$uniq <- paste(d$Course.Title,"Taught by",d$Faculty.Member,"")
d%>% select(uniq,Indicators,value) %>% filter(Indicators %in% u) -> d2
d2$Indicators <- factor(d2$Indicators)
d2 %>% group_by(uniq,Indicators) %>% summarize(n=n(), m=mean(value,na.rm=T)) -> d3
boxplot(d3$m ~ reorder(d3$Indicators,d3$m))