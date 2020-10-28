
source('./utils/helper_dataframe.R')


updateExpeditionTable <- function(df,ranges,pool){
  
  newdf <- df %>% select(-min) %>% select(-max) %>% select(-abreviation)
  # print(newdf)
  
  for (i in 1:nrow(df)) {
    row <- df[i,]
    expedRanges <- ranges[[row[["abreviation"]]]]
    newdf <- updateExpedition(row,expedRanges,colnames(newdf),newdf,pool)
  }
  # print(newdf)
  saveData(newdf,"expedition",pool)
}


updateExpedition <- function(row,expedRanges,params_list,newdf,pool){
  
  ids <- generateGlacierIDs(expedRanges)
  nbOfGlacier <- length(ids)
  # print(nbOfGlacier)
  for (i in 3:length(params_list)){
    param <- params_list[i]
    table <- getTableNameFromValue(param)
    fields <- getFieldsFromValue(param)

    nbEntryForOneGlacier <- nbOfEntryByGlacier[[param]] 
    nbOfValidGlacier <- getNbOfNotNULLEntries(table,fields,ids,nbEntryForOneGlacier,pool)
    
    newdf[[rownames(row),i]] <- paste0(nbOfValidGlacier," / ",nbOfGlacier)
  }
  return(newdf)
}

generateGlacierIDs <- function(ranges){
  ids <- vector()
  for (range in ranges) {
    ids <- c(ids,paste0("GL",as.character(range[1]:range[2])))
  }
  # print(ids)
  return(ids)
}
