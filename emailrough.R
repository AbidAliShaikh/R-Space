e=read.csv("extracted/e2013.csv")
e2 <- read.csv("extracted/e2.txt",header=F,sep=";")
e3 <- melt(e2,id=NULL);
e3$variable <- NULL;
#names(e3) <- NULL;
e2 <- e3$value
e$email <- as.character(e$email)

e2 <- e2[!(e2 %in% e$email)]
e2 <- e2[!(is.na(e2))]
e2 <- gsub("[[:space:]]","",e2)
e2 <- gsub("[[:space:]]","",e2)
e2 <- gsub("[[:space:]]","",e2)
e2 <- e2[!(e2 %in% e$email)] #line above 9
e2[grep("usindh.edu.pk",e2)] <- NA
e2 <- e2[!(is.na(e2))]
e2[grep("usindh.edu.pk",e2)] <- NULL
e$email<-gsub("[[:space:]]","",e$email)
e$email<-gsub(".com.",".com",e$email)
e2[duplicated(e2)] <- NA
e2 <- e2[!(is.na(e2))]
e2 <- tolower(e2)
e2 <- e2[!(e2 %in% tolower(e$email))] #line above 9
et <- gsub("@.+","",e2)
###############################3
e=read.csv("extracted/e2013.csv",colClasses = 'character')
e$email<-gsub("[[:space:]]","",e$email)
e$email<-gsub(".com.",".com",e$email)
