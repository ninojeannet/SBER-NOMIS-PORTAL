
source('./utils/helper_dataframe.R')

# Update the expedition table by fetching the "entire" database to give a summary of what's in the database
# For each fields and for each expedition, checks if the data has been uploaded or not.
# PArameters : 
# - df : the dataframe with the current summary
# - ranges : list of ranges of glacier for each expedition
# - pool : the database connection pool
# Return the updated table
updateExpeditionTable <- function(df,ranges,pool){
  
  newdf <- df %>% select(-min) %>% select(-max) %>% select(-abreviation)
  for (i in 1:nrow(df)) {
    row <- df[i,]
    expedRanges <- ranges[[row[["abreviation"]]]]
    newdf <- updateExpedition(row,expedRanges,colnames(newdf),newdf,pool)
  }
  saveData(newdf,"expedition",pool)
}

# Update a single expedition' summary.
# Parameters : 
# - row : the actual values from the current sumary
# - expedRanges : list of the expedition ranges
# - params : list of parameters to process the new values for
# - newdf : the data frame to append the new values to
# - pool : the database connection pool
# Return the data frame with the expedition updated
updateExpedition <- function(row,expedRanges,params_list,newdf,pool){
  
  ids <- generateGlacierIDs(expedRanges)
  nbOfGlacier <- length(ids)
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

# Generate a list of valid glacier ids from given ranges
generateGlacierIDs <- function(ranges){
  ids <- vector()
  for (range in ranges) {
    ids <- c(ids,paste0("GL",as.character(range[1]:range[2])))
  }
  return(ids)
}

# Build the progress table to be displayed on the website.
# the function retrieve the basic progress tabée then adapt / transform it to have a user firendly table to display
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
  df <- as.data.frame(t(df))
  df <- cbind(summaryFullNameFields,df)
  
  colnames(df) <- c(" ",unique(headers))
  df <- df[c(1,nrow(df),2:(nrow(df)-1)),]
  rownames(df) <- NULL
  return(df)
}

# Build a simple expedition table with their own ranges
buildExpeditionTable <- function(pool){
  dataframe <- getProgressTable(pool)
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max)
  df <- aggregate(df["range"], by=list(id_expedition=df$id_expedition,name=df$name,abreviation=df$abreviation), paste)
  return(df)
}
