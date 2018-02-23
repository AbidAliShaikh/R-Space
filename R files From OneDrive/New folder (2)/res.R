 gsub("\\,",".\n",j2[,2])

 y=(sapply(k,function(x) {unlist(strsplit(x,"\\,"))}))
 
 
 
 b=apply(j2[,2],1,function(x){ gsub("\\,","\\.\n",x)})



z=unlist(stri_reverse (unlist(strsplit(j[3,2]," "))))