#Faculty
#fac <- read.csv("Ranking2016/Faculty2015.csv",colClasses="character",header=T)
#fac$dupNIC <- duplicated(fac$CNIC.No...e.g..1111122222223..Passport.No...For.Foreigners.)
#fac[fac$Faculty.Type..regular.contractual.visiting. == "Visiting ",14] <- "Visiting"
#fac[fac$Faculty.Type..regular.contractual.visiting. == "Visitor",14] <- "Visiting"
#fac[fac$Faculty.Type..regular.contractual.visiting. == "Visiting/Part-Time",14] <- "Visiting"
#fac[fac$Terminal.Degrees.obtained.from..Name.of.University.== "Brunei Uni UK",19] <- "Brunel Uni UK"

library(chron)
db <- as.Date(chron(format(as.Date(fac$Date.of.Birth,"%d.%m.%Y"),"%m/%d/%Y")))
dj <- as.Date(chron(format(as.Date(fac$Date.of.Joining,"%d.%m.%Y"),"%m/%d/%Y")))
dr <- as.Date(chron(format(as.Date(fac$Date.of.Relieving,"%d.%m.%Y"),"%m/%d/%Y")))
fac$Date.of.Birth <- db
fac$Date.of.Joining <- dj
fac$Date.of.Relieving <- dr
#write.csv(fac,"Ranking2016/CleanFac2015.csv")
#the file is saved as CleanFac2015.csv with other corrections on the fly and Faculty2015.csv may be deleted
#fac <- read.csv("Ranking2016/data/cleanFac2015.csv",header=T,colClasses="character")
#fac <- fac[,-1]
#######################################################################################3
# Data from Regist office during 2016 is in regfac
regfac=read.csv("FacultyFromRegistrarDOCtoCSV.csv",header=T,colClasses="character")
fullfac = read.csv("Full.Fac.Archive.csv",header=T, colClasses="character")

library(chron)

regfac$D.o.J <- as.Date(chron(format(as.Date(regfac$D.o.J,"%m/%d/%Y"),"%m/%d/%Y")))

fullfac$Date.of.Birth  <- as.Date(chron(format(as.Date(fullfac$Date.of.Birth,"%m/%d/%Y"),"%m/%d/%Y")))
fullfac$Date.of.Joining <- as.Date(chron(format(as.Date(fullfac$Date.of.Joining,"%m/%d/%Y"),"%m/%d/%Y")))
fullfac$Date.of.Relieving <- as.Date(chron(format(as.Date(fullfac$Date.of.Relieving,"%m/%d/%Y"),"%m/%d/%Y")))

regfac$merge1 = paste(regfac$Name,regfac$Designation.x)
fullfac$merge2 = paste(fullfac$Full.Name,fullfac$Designation)

matches=partialMatch(regfac$merge1, fullfac$merge2)
a=regfac;b=fullfac
aa=merge(a,matches,by.x="merge1",by.y="raw.x",all.x=T)
aa=merge(aa,b,by.x="raw.y",by.y="merge2",all.x=T)
write.csv(aa,"aa.csv")

a = c(4,5,6,7,9,10,11,13,15,16,19)
f = fac[!complete.cases(fac[,a]),c(a)]
write.csv(f,"Ranking2016/resolve2.csv")

#****************Date Frame****************
library(chron)
fac <- read.csv("Full Faculty 2014-15 13.4.16.csv",header=T,colClasses="character")
db <- as.Date(chron(format(as.Date(fac$Date.of.Birth,"%m/%d/%Y"),"%m/%d/%Y")))
dj <- as.Date(chron(format(as.Date(fac$Date.of.Joining,"%m/%d/%Y"),"%m/%d/%Y")))
dr <- as.Date(chron(format(as.Date(fac$Date.of.Relieving,"%m/%d/%Y"),"%m/%d/%Y")))
fac$Date.of.Birth <- db
fac$Date.of.Joining <- dj
fac$Date.of.Relieving <- dr

retired = fac$Date.of.Birth >= as.Date("1954-07-01") & fac$Date.of.Birth <= as.Date("1955-06-30")
fac = fac[!retired,]
regular = fac$Fac.Type == "Full Time Regular"
fac = fac[regular,];nrow(fac)
nrow(fac[fac$Final.Deg == "University of Sindh",])
recruited <- fac$Date.of.Joining >= as.Date("2014-07-01") & fac$Date.of.Joining <= as.Date("2015-06-30")
nrow(fac[recruited==T,])


write.csv(recruited,"Ranking2016/recruited.csv")
write.csv(retired, "Ranking2016/retired.csv")

#*************************************************************************
#                       ||||||||||||||||||||||||||||||||
#*************************************************************************

fac = read.csv("FacultyFromRegistrarDOCtoCSV.csv",header=T,colClasses="character")
fac = fac[fac$F.Type == "Full Time Regular",]
dj <- as.Date(chron(format(as.Date(fac$Year,"%m/%d/%Y"),"%m/%d/%Y")))
fac$Year <- dj
recruited <- fac$Year >= as.Date("2014-07-01") & fac$Year <= as.Date("2015-06-30")
nrow(fac[recruited==T,])
#**********************Pubs 2015

pub <- read.csv("Ranking2016/pubs/pubs2015.csv",header=T, colClasses="character")
short <- read.csv("Ranking2016/pubs/shortnames.csv",header=T, colClasses="character")

lst <- strsplit(pub$Authors,",")
#**********************************
# convert above list qith unequal columns into a data frame with showing NAs in blank cells
indx <- sapply(lst, length)
#indx <- lengths(lst) 
res <- as.data.frame(do.call(rbind,lapply(lst, `length<-`,max(indx))))
colnames(res) <- names(lst[[which.max(indx)]])
#res
l2 <- res
#**********************************

l2 <- as.character(unlist(l2)); l2 = gsub("^\\s+|\\s+$","",l2)
l2 <- tolower (l2); short$Short <- tolower(short$Short)
l2 = short$Department[ match(l2, short$Short)]
l2 = matrix(l2, ncol=6,byrow=F)
Pub = cbind(pub, l2)
names(Pub) [10:15] <- c(letters[1:6])
Pub <- Pub [ order(Pub[,10],Pub[,11],Pub[,12],Pub[,13],Pub[,14],Pub[,15]),]
N <- Pub[which(rowSums(is.na(Pub[,10:15])) == 6),]
Pub <- Pub[which(rowSums(is.na(Pub[,10:15])) < 6),]

Pub[,10:15]<- sapply(Pub[,10:15],as.character)
Pub[,10:15][is.na(Pub[,10:15])]<-" "
Pub <- as.data.frame(Pub)

write.csv(Pub,"Ranking2016/pubs/PublicationsF2015.csv")
write.csv(N[,-c(10:15)],"Ranking2016/pubs/ExtraPubs2015.csv")

#***********************
#**********************Pubs 2013

pub <- read.csv("Ranking2016/pubs/pubs2013GS.csv",header=T, colClasses="character")
short <- read.csv("Ranking2016/pubs/shortnames.csv",header=T, colClasses="character")

lst <- strsplit(pub$Authors,",")
#**********************************
# convert above list qith unequal columns into a data frame with showing NAs in blank cells
indx <- sapply(lst, length)
#indx <- lengths(lst) 
res <- as.data.frame(do.call(rbind,lapply(lst, `length<-`,max(indx))))
colnames(res) <- names(lst[[which.max(indx)]])
#res
l2 <- res
#**********************************
# doing above We donot need this segment now -->l <- data.frame(do.call(rbind,lst))
#**********************************
l2 <- as.character(unlist(l2)); l2 = gsub("^\\s+|\\s+$","",l2)
l2 <- tolower (l2); short$Short <- tolower(short$Short)
l2 = short$Department[ match(l2, short$Short)]
l2 = matrix(l2, ncol=6,byrow=F)
Pub = cbind(pub, l2)
names(Pub) [11:16] <- c(letters[1:6])
Pub <- Pub [ order(Pub[,11],Pub[,12],Pub[,13],Pub[,14],Pub[,15],Pub[,16]),]
N <- Pub[which(rowSums(is.na(Pub[,11:16])) == 6),]
Pub <- Pub[which(rowSums(is.na(Pub[,11:16])) < 6),]


Pub[,11:16]<- sapply(Pub[,11:16],as.character)
Pub[,11:16][is.na(Pub[,11:16])]<-" "
Pub <- as.data.frame(Pub)
write.csv(Pub,"Ranking2016/pubs/PublicationsF2013.csv")
write.csv(N[,-c(11:16)],"Ranking2016/pubs/ExtraPubs2013.csv")

#***********************pubs 2014

pub <- read.csv("Ranking2016/pubs/pubs2014GS.csv",header=T, colClasses="character")
short <- read.csv("Ranking2016/pubs/shortnames.csv",header=T, colClasses="character")

lst <- strsplit(pub$Authors,",")
#**********************************
# convert above list qith unequal columns into a data frame with showing NAs in blank cells
indx <- sapply(lst, length)
#indx <- lengths(lst) 
res <- as.data.frame(do.call(rbind,lapply(lst, `length<-`,max(indx))))
colnames(res) <- names(lst[[which.max(indx)]])
#res
l2 <- res
#**********************************

l2 <- as.character(unlist(l2)); l2 = gsub("^\\s+|\\s+$","",l2)
l2 <- tolower (l2); short$Short <- tolower(short$Short)
l2 = short$Department[ match(l2, short$Short)]
l2 = matrix(l2, ncol=6,byrow=F)
Pub = cbind(pub, l2)
names(Pub) [10:15] <- c(letters[1:6])
Pub <- Pub [ order(Pub[,10],Pub[,11],Pub[,12],Pub[,13],Pub[,14],Pub[,15]),]
N <- Pub[which(rowSums(is.na(Pub[,10:15])) == 6),]
Pub <- Pub[which(rowSums(is.na(Pub[,10:15])) < 6),]

Pub[,10:15]<- sapply(Pub[,10:15],as.character)
Pub[,10:15][is.na(Pub[,10:15])]<-" "
Pub <- as.data.frame(Pub)

write.csv(Pub,"Ranking2016/pubs/PublicationsF2014.csv")
write.csv(N[,-c(10:15)],"Ranking2016/pubs/ExtraPubs2014.csv")

