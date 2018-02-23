#Load in the data from the URLs:
#PercentageUsingTheNet=read.csv(url('http://s.telegraph.co.uk/graphics/conrad/PercentageUsingTheNet.csv', encoding='MACROMAN'))
#ccode=read.csv(url('http://www.iso.org/iso/country_names_and_code_elements_txt'),sep=';')
                                   
                                   ##Here's where the algorithm starts...
                                   ##I'm going to generate a signature from country names to reduce some of the minor differences between strings
                                   ##In this case:
                                   ### convert all characters to lower case (tolower())
                                   ### split the string into a vector (unlist()) of separate words (strsplit())
                                   ### sort the words alphabetically (sort())
                                   ### and then concatenate them with no spaces (paste(y,collapse='')).
                                   ##So for example, United Kingdom would become kingdomunited
                                   ##To extend this function, we might also remove stopwords such as 'the' and 'of', for example (not shown).
                                   signature=function(x){
                                     sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
                                     return(sig)
                                   }
                                   
                                   #The partialMatch function takes two wordlists as vectors (x,y) and an optional distance threshold (levDist)
                                   #The aim is to find words in the second list (y) that match or partially match words in the first (x)
                                   partialMatch=function(x,y,levDist=0.1){
                                     #Create a data framecontainind the signature for each word
                                     xx=data.frame(sig=sapply(x, signature),row.names=NULL)
                                     yy=data.frame(sig=sapply(y, signature),row.names=NULL)
                                     #Add the original words to the data frame too...
                                     xx$raw=x
                                     yy$raw=y
                                     #We only want words that have a signature...
                                     xx=subset(xx,subset=(sig!=''))
                                     
                                     #The first matching pass - are there any rows in the two lists that have exactly the same signature?
                                     xy=merge(xx,yy,by='sig',all=T)
                                     matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
                                     #?I think matched=xy[ complete.cases(raw.x,raw.y) ] might also work here?
                                     #Label the items with identical signatures as being 'Duplicate' matches
                                     matched$pass="Duplicate"
                                     
                                     #Grab the rows from the first list that were unmatched - that is, no matching item from the second list appears
                                     todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
                                     #We've grabbed the signature and original raw text from the first list that haven't been matched up yet
                                     #Name the columns so we know what's what
                                     colnames(todo)=c('sig','raw')
                                     
                                     #This is the partial matching magic - agrep finds items in the second list that are within a 
                                     ## certain Levenshtein distance of items in the first list.
                                     ##Note that I'm finding the distance between signatures.
                                     todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
                                     
                                     #Bring the original text into the partial match list based on the sig key.
                                     todo=merge(todo,yy,by.x='partials',by.y='sig')
                                     
                                     #Find the items that were actually partially matched, and pull out the columns relating to signatures and raw text
                                     partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
                                     #Label these rows as partial match items
                                     partial.matched$pass="Partial"
                                     #Add the set of partially matched items to the set of duplicate matched items
                                     matched=rbind(matched,partial.matched)
                                     
                                     #Find the rows that still haven't been matched
                                     un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
                                     
                                     #If there are any unmatched rows, add them to the list of matched rows, but labelled as such
                                     if (nrow(un.matched)>0){
                                       un.matched$pass="Unmatched"
                                       matched=rbind(matched,un.matched)
                                     }
                                     
                                     #Grab the columns of raw text from x and y from the matched list, along with how they were matched/are unmatched
                                      matched=subset(matched,select=c("raw.x","raw.y","pass"))
                                     #Ideally, the length of this should be the same as the length of valid rows in the original first list (x)
                                     
                                     return(matched)
                                   }
                                   
                                   #A rogue character in @coneee's data file borked things for me, so I originally needed to do a character code conversion first
                                   #PercentageUsingTheNet$ECONOMY=iconv(PercentageUsingTheNet$ECONOMY)
                                   #Loading the CSV with the encoding attribute set (h/t Kent Johnson) seemed to work properly though...
                                   
                                   #Call the partial match function using two vectors
                                   #The aim is to find items in the second vector that partially match items in the first
                                   #The function returns the first list annotated with partial match items from the second and a match type
                                   matches=partialMatch(PercentageUsingTheNet$ECONOMY,ccode$Country.Name)
                                   
                                   
                                   
                                   a=PercentageUsingTheNet
                                   b=ccode
                                   #Merge the original data set with the ISO country code country name keys
                                   aa=merge(a,matches,by.x='ECONOMY',by.y='raw.x',all.x=T)
                                   #Merge in the ISO country codes
                                   aa=merge(aa,b,by.x='raw.y',by.y='Country.Name',all.x=T)
                                   aa=subset(aa,select=c('ECONOMY','RANK','PERCENTAGE.OF.INDIVIDUALS.USING.THE.INTERNET.2011','ISO.3166.1.alpha.2.code'))
                                   