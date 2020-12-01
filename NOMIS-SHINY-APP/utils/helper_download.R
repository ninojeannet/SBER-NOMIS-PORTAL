# Generate a dataframe of fields from the same table
# the dataframe contains detailed value from the database such as replicates
# Parameters : 
# - field : the field / category of field to put in the data frame
# - ids : lists of glaciers ids
# - pool : the database connection pool
# Return the generated data frame
generateSimpleDownloadDF <- function(field,ids,pool){
  table <- getTableNameFromValue(field)
  fields <- unique(c(mandatoryFields[[table]], getFieldsFromValue(field)))
  df <- getFieldsFromGlacier(pool,table,fields,ids)
  df <- formatDFforDownload(df)
  newnames <- c()
  for (field in fields) {
    newName <- str_replace_all(convertToDLName(field),"\n"," ")
    newnames <- c(newnames,newName)
  }
  colnames(df) <- newnames
  return(df)
}

# Generate a dataframe which can contains fields from any table.
# The function merge, reduce and scale data to obtains one only data frame containing all the desired parameters
# Parameters : 
# - fields : list of the selected fields. Those fields will be in the dataframe
# - ids : Lists of selecteed glacier ids
# - pool : The database connection pool
# Return the generated data frame
generateMergedDownloadDF <- function(fields,ids,pool){
  tables <- unlist(lapply(fields,getTableNameFromValue))
  # print(tables)
  nbEntries <- max(levels[tables])
  nbRow <- nbEntries*length(ids)
  
  df <- data.frame(matrix(ncol = 0, nrow = nbRow))
  if(nbEntries == 6)
    df[["patch"]] <- unlist(getFieldsFromGlacier(pool,tableName = "patch" ,fields = c("id_patch"),ids = ids))
  else if(nbEntries == 2)
    df[["location"]] <- unlist(getFieldsFromGlacier(pool,tableName = "location" ,fields = c("id_location"),ids = ids))
  else
    df[["glacier"]] <-  unlist(ids)
  
  for (field in fields) {
    table <- getTableNameFromValue(field)
    fieldsToRetrieve <- setdiff(getFieldsFromValue(field),mandatoryFields[[table]])
    nbReplicates <- length(tableOptions[[table]][["replicates"]])
    
    values <-getFieldsWithFKFromGlacier(pool,tablename = table ,fields = fieldsToRetrieve,ids = ids)
    fk <- tableOptions[[table]][["FK"]]
    colToSummarise <-names(values %>% select(-all_of(fk)))
    
    if(nbReplicates > 1)
      values <- reduce(values,table,fk)
    else
      values <- formatDFforDownload(values)
    
    if(isUpOnly(table))
      values <- insertEmptyDownEntries(values,fk)
    values <- removeFK(values)

    column <- scale(values,table,nbEntries)
    
    if(is.null(ncol(column))){
      name <- convertToDLName(field)
      df[[name]] <- unlist(column)
      
    }
    else{
      for(i in 1:ncol(column)){
        name <- convertToDLName(names(column[i]))
        df[[name]] <- unlist(column[i])
      }
    }
  }
  return(df)
}

# This function merges all the replicate of a parameters to eliminate the replicates 
# and replace it with an average value.
# Parameters : 
# - values : the data frame to reduce 
# - tablename : the table name of the input values
# Return the reduced data frame
reduce <- function(values,tablename,fk){
  # values <- data.frame(id_patch=c(1,1,1,1),chla=c(-9999,NA,0.42,0.43))
  # tablename <- "microbial_3"
  colToSummarise <-names(values %>% select(-all_of(fk)))
  values <- formatDFforDownload(values)
  # complex complex
  # print(values)
  values <- values %>% 
    group_by(.dots=fk) %>%                         
    summarise_at(vars(colToSummarise),mean,na.rm = TRUE) 
  values[is.na(values)] <- NA
  

  # print(values)
  return(values)
}

# This function scale the given data frame to the desired final size.
# It duplicates all values to have a data frame of the right size
# Parameters :
# - values : the data frame to scale
# - tablename : the table name of the data frame 
# - nbFinalEntries : the final number of entries to have per glacier
# Returned the scaled data frame
scale <- function(values,tablename,nbFinalEntries){
  nbEntries <- levels[tablename]
  factor <- nbFinalEntries / nbEntries
  newdf <- values[rep(row.names(values), each=factor), 1:ncol(values) ]
  newdf <- as.data.frame(newdf)
  colnames(newdf) <- names(values)
  return(newdf)
}

# Get the display name of the given field
# Parameter :
# - field : field to convert 
# Return the converted name
convertToDLName <- function(field){
  category <- getCategoryFromValue(field)
  index <- which(templateFieldNames[[category]] == field)[[1]]
  displayField <- fullnameFields[[category]][[index]]
  unit <-str_extract(displayField,"\\[.+\\]")
  if(!is.na(unit))
    name <- paste(field,unit)
  else
    name <-field
  # print(paste(field,unit))
  return(name)
}

isUpOnly <- function(table){
  return(!is.null(isOnlyUP[[table]]) && isOnlyUP[[table]])
}

insertEmptyDownEntries <- function(df,fk){
  df <- df %>% mutate(rownum = row_number()) %>% 
    bind_rows(., filter(., !is.na(fk)) %>% 
                mutate(across(all_of(colToSummarise),function(.){NA}), rownum = rownum-.5)) %>% 
    arrange(rownum) %>%
    select(-rownum)
  return(df)
}

