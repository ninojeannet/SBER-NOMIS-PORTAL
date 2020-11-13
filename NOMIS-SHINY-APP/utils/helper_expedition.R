
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

buildProgressTable <- function(pool){
  dataframe <- getProgressTable(pool)
  dataframe <- dataframe[order(dataframe$abreviation),]
  headers <- dataframe[["name"]]
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max) %>% select(-name) %>% select(-id_expedition)
  rowTypes <- colnames(df)
  df[is.na(df)] <- ""
  df <- aggregate(df["range"], by=list(abreviation=df$abreviation,doc=df$doc,dom=df$dom,
                                       ion=df$ion,nutrient=df$nutrient,eps=df$eps,ba=df$ba
                                       ,bp=df$bp,respiration=df$respiration,chla=df$chla,enzyme =df$enzyme), paste)
  df <- df[order(df$abreviation),]
  print(df)
  df <- as.data.frame(t(df))
  print(df)
  df <- cbind(summaryFullNameFields,df)
  
  colnames(df) <- c(" ",unique(headers))
  df <- df[c(1,nrow(df),2:(nrow(df)-1)),]
  rownames(df) <- NULL
  
  print(df)
  return(df)
}

buildExpeditionTable <- function(pool){
  dataframe <- getProgressTable(pool)
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max)
  df <- aggregate(df["range"], by=list(id_expedition=df$id_expedition,name=df$name,abreviation=df$abreviation), paste)
  return(df)
}
