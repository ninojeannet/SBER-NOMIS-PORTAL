
source('./utils/helper_dataframe.R')

# Update the expedition table by fetching the "entire" database to give a summary of what's in the database
# For each fields and for each expedition, checks if the data has been uploaded or not.
# PArameters : 
# - df : the dataframe with the current summary
# - ranges : list of ranges of glacier for each expedition
# - pool : the database connection pool
# Return the updated table
updateExpeditionTable <- function(df,ranges,pool){
  tryCatch({
    withProgress(message = "Refreshing progress table",value = 0,{
      newdf <- df %>% select(-min) %>% select(-max) %>% select(-abbreviation)
      n <- nrow(df)
      for (i in 1:n) {
        row <- df[i,]
        expedRanges <- ranges[[row[["abbreviation"]]]]
        newdf <- updateExpedition(row,expedRanges,colnames(newdf),newdf,pool)
        incProgress(1/n, detail = paste("Processing..."))
      }
    })
    saveData(newdf,"expedition",FALSE,pool)
    showNotification("Progress table succesfully updated", type = "message")
  },
  warning = function(war){
    print(war)
    showNotification(war$message, type = "warning")
  },
  error = function(err){
    print(err)
    showNotification(err$message,type = "error",duration = NULL)
  },
  finally = function(f){
  })
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
    if(param=="16s" || param=="18s" || param=="trace_el" || param=="ft_icr_ms"){
      newdf[[rownames(row),i]] <- row[[param]]
    }
    else{
      if(table=="glacier"){
        fields <- unique(unlist(templateFieldNames[subCategoriesOfTable[["glacier"]]]))
        nbEntryForOneGlacier <- nbOfEntryByGlacier[[param]]
        nbOfValidGlacier1 <- getNbOfNotNULLEntries(table,fields,ids,nbEntryForOneGlacier,pool)
        fields <- unique(unlist(templateFieldNames[subCategoriesOfTable[["glacier_ud"]]]))
        nbEntryForOneGlacier <- nbOfEntryByGlacier[["glacier_ud"]]
        nbOfValidGlacier2 <- getNbOfNotNULLEntries("glacier_ud",fields,ids,nbEntryForOneGlacier,pool)
        nbOfValidGlacier <- min(nbOfValidGlacier1,nbOfValidGlacier2)
      }
      else{
        fields <- getFieldsFromValue(param)
        nbEntryForOneGlacier <- nbOfEntryByGlacier[[param]]
        nbOfValidGlacier <- getNbOfNotNULLEntries(table,fields,ids,nbEntryForOneGlacier,pool)
      }
      newdf[[rownames(row),i]] <- paste0(nbOfValidGlacier," / ",nbOfGlacier)
    }
    
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

  if (nrow(dataframe)==0)
    return(dataframe)
  # print(dataframe)
  # dataframe <- data.frame("16s"=c(1,2,4),"18s"=c(1,2,4),"trace_el"=c(1,NA,4),"ft_icr_ms"=c(1,2,4))
  files <- c("16s","18s","trace_el","ft_icr_ms")
  dataframe[files] <- lapply(dataframe[files], function(x) replace(x,!is.na(x), "1 / 1"))
  dataframe[files] <- lapply(dataframe[files], function(x) replace(x,is.na(x), "0 / 1"))
  dataframe <- dataframe[order(dataframe$abbreviation),]
  
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max) %>% select(-id_expedition)
  # print(df)
  rowTypes <- colnames(df)
  df[is.na(df)] <- ""
  df <- aggregate(df["range"], by=list(name=df$name,abbreviation=df$abbreviation,location=df$location,glacier=df$glacier,
                                       doc=df$doc,dom=df$dom,indices=df$indices,ft_icr_ms=df$ft_icr_ms,mineral=df$mineral,
                                       ion=df$ion,nutrient=df$nutrient,isotope=df$isotope,trace_el=df$trace_el,"16s"=df[["16s"]],
                                       "18s"=df[["18s"]],chla=df$chla,eps=df$eps,enzyme =df$enzyme,
                                       bp=df$bp,ba=df$ba,respiration=df$respiration),
                  function(x){ paste(unlist(x),collapse = ', ')})
  
  df <- df[order(as.numeric(gsub(" -.+","", df$range)),df$range),]
  headers <- df[["name"]]
  df <- df %>% select(-name)
  df <- as.data.frame(t(df))
  df <- as.data.frame(df[c(1,nrow(df),2:(nrow(df)-1)),])
  df <- cbind(summaryFullNameFields,df)
  colnames(df) <- c(" ",unique(headers))
  
  rownames(df) <- NULL
  return(df)
}

# Build a simple expedition table with their own ranges
buildExpeditionTable <- function(pool){
  dataframe <- getProgressTable(pool)
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max)
  if (nrow(df)==0){
    df <- df %>% select(id_expedition,name, abbreviation, range)
    return(df)
  }
  df <- aggregate(df["range"], by=list(id_expedition=df$id_expedition,name=df$name,abbreviation=df$abbreviation),function(x){ paste(x,sep = ",", collapse = ',')})
  return(df)
}


formatExpedList <- function(pool){
  exped <-getTable("expedition",pool)
  return(setNames(as.character(exped$id_expedition), exped$name))
}