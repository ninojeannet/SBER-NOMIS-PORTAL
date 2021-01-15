source('./utils/template_config.R')

# Copy dataframe value to another dataframe
# Parameters :
# - dftoCopy : the dataframe to copy
# - newdf : the new df to copy the values to
# - primary : the primary key of the table
# Return the updated dataframe
copyDFValuesTo <- function(dfToCopy,newdf,primary){
  # Insert existing values into the new data frame
  ids <- newdf[[primary]]
  for (i in 1:nrow(dfToCopy)) {
    row <-dfToCopy[i,]
    id <- row[[primary]]
    if(id %in% ids)
    {
      se <- newdf[[primary]]
      rownumber <- which(se == id,arr.ind = TRUE)
      newdf[rownumber,] <- row
    }
  }
  return(newdf)
}

# Add rows filled with NA to a given dataframe
# Parameters :
# - df : the dataframe to apppend rows to
# - start : the row number where to start adding rows
# - stop : the row number where to stop adding rows
# - nbcol : the number of column to add for each row
# Return the updated dataframe
addRows <- function(df,start,stop,nbCol){
  for (i in (start+1):stop) {
    row <- c(rep(NA, nbCol))
    df[i,] <- row
  }
  return(df)
}

# Check for cells with existing data in the database and return a list of index of these cells
# Parameters : 
# - dataframe : the dataframe to check for not empty cells
# - tablename : the name of the table of the current dataframe
# Return a dataframe of  indexes of not empty cells
getReadOnlyCells <- function(dataframe,tablename){
  colNames <- setdiff(colnames(dataframe),unlist(mandatoryFields[tablename]))
  dataframe <- dataframe[colNames]
  if(tablename == "location")
    dataframe$rdna <- NULL
  onlyExistingCells <- which(!is.na(dataframe),arr.ind = TRUE)
  onlyExistingCells[,"col"] <- onlyExistingCells[,"col"]+length(unlist(mandatoryFields[tablename]))
  rownames(onlyExistingCells) <- NULL
  return(onlyExistingCells)
}
# Return the name of the table containing a given field
# Check in the config file the tablename
# Parameters :
# - value : name of the field to find its table for
# return the name of the table
getTableNameFromValue <- function(value){
  l <- list.search(templateFieldNames,value %in% .)
  if(length(l) >0){
    # tablename <- names(l)[1]
    l1 <- list.search(subCategoriesOfTable,names(l)[1] %in% .)
    if(length(l1) >0)
      tablename <- names(l1)[1]
    else
      tablename <- names(l)[1]
  }
  else{
    l1 <- list.search(subCategoriesOfTable,value %in% .)
    if(length(l1) >0)
      tablename <- names(l1)[1]
    else
      tablename <- value
  }
  return(tablename)
}

# Return the category containing a given field
# Check in the config file the category
# Parameters :
# - value : name of the field to find its category for
# return the category
getCategoryFromValue <- function(value){
  l <- list.search(templateFieldNames,value %in% .)
  if(length(l) >0){
    tmp <- names(l)
    tablename <- tmp[[length(tmp)]]
  }
  else
    tablename <- value
  return(tablename)
}

# Return the fields from a given value
# Check in the config file 
# Parameters :
# - value : nvalue to find fields for
# return the fields
getFieldsFromValue <- function(value){
  
  if (value %in% names(templateFieldNames))
  {
    return(templateFieldNames[[value]])
  }
  else{
    if(value %in% names(specificFields))
       return(specificFields[[value]])
    else
      return(value)
    }
}

# Generate an handsonTable according to the given table
# set the validator, renderer and type for each columns. Values taken from config file
# Parameters :
# - df : the dataframe containing the data to display
# - dimension : Dimension of the window to adapt the table
# - readOnlyrows : list of rows to display as ReadOnly
# - name : name of the data to display
# - tablename : name of the database table (can be the same as table)
# Return the generated handsontable
generateHandsonTable <- function(df,dimension,readOnlyCells,name,tablename){
  specificTypeColumns <- c()
  if(name == "location"){
    df[["rdna"]] <- as.logical(df[["rdna"]])
    df$rdna[is.na(df$rdna)] <- FALSE
    specificTypeColumns <- c("rdna")
  }
  df[df=="-9999"] <- "NA"
  df[!(colnames(df) %in% specificTypeColumns)] <- lapply(df[!(colnames(df) %in% specificTypeColumns)], as.character)
  df <- setCompleteColumnName(df,name)
  
  
  handsonTable <- rhandsontable(df,overflow='visible',stretchH = "all",height = dimension()[2]/100*70)%>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(fixedColumnsLeft = length(mandatoryFields[[tablename]]),renderer = 
    "function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.getRenderer('text')(instance, td, row, col, prop, value, cellProperties);

          if(value =='na' || value == 'NA')
          {
            value = 'NA';
            Handsontable.renderers.getRenderer('text')(instance, td, row, col, prop, value, cellProperties);
          }
          return td;}") %>%
    hot_col(col = mandatoryColumns[[tablename]], readOnly = TRUE)

  if(length(readOnlyCells) != 0){
    for (i in 1:length(readOnlyCells)/2) {
      handsonTable <-  do.call(hot_cell,list(hot=handsonTable,row=readOnlyCells[[i,1]],col=readOnlyCells[[i,2]],readOnly=TRUE))
    }
  }
  if (name %in% names(colConfig)){
    for (params in colConfig[[name]]) {
      handsonTable <-  do.call(hot_col,c(list(hot=handsonTable),params))
    }
  }
  
  return(handsonTable)
}

formatDFforUpload <- function(df){
  df[df==""] <- NA
  df[df==" "] <- NA
  df[df=="na" | df=="NA" | df=="nan"] <- "-9999"
  df <- data.frame(lapply(df, function(x){
    if(is.character(x))
      gsub("^\\s",NA,x)
    else
      x
  }))
  return(df)
}

formatDFforDownload <- function(df){
  df[is.na(df)] <- NA
  df[df == -9999 | df =="-9999"] <- NA
  return(df)
}

removeFK <- function(df,table){
  fk <- tableOptions[[table]][["FK"]]
  df <- df %>%
    select(-all_of(fk))
  return(df)
}