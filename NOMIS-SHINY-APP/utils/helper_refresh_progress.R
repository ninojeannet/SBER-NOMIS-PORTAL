

updateExpeditionTable <- function(df,ranges,params_list){
  print(df)
  newdf <- df %>% select(-min) %>% select(-max) %>% select(-name) %>% select(-abreviation)
  for (i in 1:nrow(df)) {
    row <- df[i,]
    expedRanges <- ranges[[row[["abreviation"]]]]
    newdf <- updateExpedition(row,expedRanges,colnames(newdf),newdf)
  }
  saveData(newdf,"expedition",pool)
}


updateExpedition(row,expedRanges,params_list,newdf){
  
  ids <- generateGlacierIDs(expedRanges)
  for (i in 2:length(params_list)){
    param <- params_list[i]
    # table <- getTableFrom
    # nbEntryForOneGlacier <- NbOfEntryByGlacier[[param]] * length(ids)
    # actualNbOfEntry <- getNbOfNotNULLEntries(table,fields,ids,pool)
    
    #TODO wrong index, get table name, create index list for nbofentrybygalcier
    # newdf[[i,]] <- paste0(actualNbOfEntry," / ",nbEntryForOneGlacier)
    print(newdf)
  }
  return(newdf)
}

generateGlacierIDs <- function(ranges){
  ids <- vector()
  for (range in ranges) {
    ids <- c(ids,paste0("GL",as.character(range[1]:range[2])))
  }
}
