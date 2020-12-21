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
  nbEntries <- max(levels[tables])
  nbRow <- nbEntries*length(ids)
  
  df <- data.frame(matrix(ncol = 0, nrow = nbRow))
  if(nbEntries == 6){
      lis <- getFieldsFromGlacier(pool,tableName = "patch" ,fields = c("id_patch"),ids = ids)
      df[["patch"]] <- reg_sort(lis[[1]],"^GL\\d+",-"UP|DN","_\\d")
  }
  else if(nbEntries == 2){
    lis <- getFieldsFromGlacier(pool,tableName = "location" ,fields = c("id_location"),ids = ids)
    df[["location"]] <- reg_sort(lis[[1]],"^GL\\d+",-"UP|DN")
  }
  else{
    lis <- unlist(ids)
    df[["glacier"]] <-  reg_sort(lis,"^GL\\d+")
  }

  for (field in fields) {
    table <- getTableNameFromValue(field)
    fieldsToRetrieve <- setdiff(getFieldsFromValue(field),mandatoryFields[[table]])
    nbReplicates <- length(tableOptions[[table]][["replicates"]])
    
    values <-getFieldsWithFKFromGlacier(pool,tablename = table ,fields = fieldsToRetrieve,ids = ids)
    fk <- tableOptions[[table]][["FK"]]
    if(table != "glacier")
      colToSummarise <-names(values %>% select(-all_of(fk)))

    if(nbReplicates > 1)
      values <- reduce(values,table,fk,colToSummarise)
    else
      values <- formatDFforDownload(values)
    
    if(levels[table] == 6){
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))),
                                   desc(gsub("[^(UP|DN)]", "\\1", values[[1]])), gsub("^.+_", "\\1", values[[1]]))
    }
    else if(levels[table] == 2){
      for (i in 1:length(values[[1]])) {
        if(i %% 2 == 1)
          values[i,1] <- paste0(values[i,1],"_DN")
        else
          values[i,1] <- paste0(values[i,1],"_UP")
      }
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))),
                                   desc(gsub("[^(UP|DN)]", "\\1", values[[1]])))
      values[[1]] <- unlist(lapply(values[[1]],function(x){str_remove(x,"_UP|_DN")}))
    }
    else{
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))))
    }

    if(isUpOnly(table))
      values <- insertEmptyDownEntries(values,fk,colToSummarise)
    
    if(table != "glacier")
      values <- removeFK(values,table)
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
reduce <- function(values,tablename,fk,colToSummarise){
  values <- formatDFforDownload(values)
  # complex complex
  values <- values %>% 
    group_by(.dots=fk) %>%                         
    summarise_at(vars(all_of(colToSummarise)),mean,na.rm = TRUE) 
  values[is.na(values)] <- NA
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
  return(name)
}

isUpOnly <- function(table){
  return(!is.null(isOnlyUP[[table]]) && isOnlyUP[[table]])
}

insertEmptyDownEntries <- function(df,fk,colToSummarise){
  df <- df %>% mutate(rownum = row_number()) %>% 
    bind_rows(., filter(., !is.na(fk)) %>% 
                mutate(across(all_of(colToSummarise),function(.){NA}), rownum = rownum+.5)) %>% 
    arrange(rownum) %>%
    select(-rownum)
  return(df)
}

