

file.list <- list.files(path="f:/Rspace/From Excel to R/",pattern='*.xlsx',recursive = T)
df.list <- lapply(file.list, read_excel)

library(data.table)
df <- rbindlist(df.list, idcol = "id")
d <- df [rowSums(is.na(df[,3:188])) != ncol(df[,3:188]),]
