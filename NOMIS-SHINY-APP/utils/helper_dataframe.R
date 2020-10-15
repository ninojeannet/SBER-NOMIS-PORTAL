source('./utils/template_config.R')


copyDFValuesTo <- function(dfToCopy,newdf,primary,shift){
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

addRows <- function(df,start,stop,nbCol){
  for (i in (start+1):stop) {
    row <- c(rep(NA, nbCol))
    df[i,] <- row
    # df <- rbind(df,row)
    # df <- rbindlist(list(df,as.list(row)))
    # df %>% add_row(tribble_row())
  }
  return(df)
}

# Check for rows with existing data in the database and return a list of index of these rows
# Parameters : 
# - dataframe : the dataframe to check for not empty rows
# - tablename : the name of the table of the current dataframe
# Return a list of row indexes of not empty rows
getReadOnlyRows <- function(dataframe,tablename){
  colNames <- setdiff(colnames(dataframe),unlist(mandatoryFields[tablename]))
  dataframe <- dataframe[colNames]
  onlyExistingRows <- dataframe[rowSums(is.na(dataframe)) != ncol(dataframe),,drop = FALSE]
  rows <- rownames(onlyExistingRows)
  return(rows)
}

# Return the name of the table containing a given field
# Check in the config file the tablename
# Parameters :
# - value : name of the field to find its table for
# return the name of the table
getTableNameFromValue <- function(value){
  l <- list.search(templateFieldNames,value %in% .)
  if(length(l) >0)
    tablename <- names(l)[1]
  else
    tablename <- value
  return(tablename)
}

generateHandsonTable <- function(df,dimension,readOnlyRows,table){
  if(table == "location")
    df[["rdna"]] <- as.logical(df[["rdna"]])
  df <- setCompleteColumnName(df,table)
  
  handsonTable <- rhandsontable(df,overflow='visible',stretchH = "all", height = dimension()[2]/100*70)%>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
    hot_col(mandatoryFields[[table]], readOnly = TRUE) %>%
    hot_row(readOnlyRows, readOnly = TRUE)
  #     hot_cols(format = tableOptions[[table]][["format"]]) %>%
  

  # switch (table,
  #       
  #         "glacier" = {
  #         },
  #         "location" = {
  for (params in colConfig[[table]]) {
    handsonTable <-  do.call(hot_col,c(list(hot=handsonTable),params))
  }
            # handsonTable <-  do.call(hot_col,v) %>%
          #   #   hot_col(5, validator = "function (value, callback) {
          #   #   if (/^\\d{1,2}:\\d{2}($|:\\d{2}$)/.test(value)) {callback(true)} else {callback(false)}}") %>%
          #   #   hot_col(c(6,7), validator = "function (value, callback) {
          #   #   if (/^-?([1-8]?[1-9]|[1-9]0)\\.{1}\\d{1,6}/.test(value)) {callback(true)} else {callback(false)}}") %>%
          #   #   hot_col(c(9,10,11,12,13,14,15,16), validator = "function (value, callback) {
          #   #   if (/^[-]?\\d*\\.?\\d*$/.test(value)) {callback(true)} else {callback(false)}}") %>%
          #   #   hot_col(8, validator = "function (value, callback) {
          #   #   if (/\\d+/.test(value)) {callback(true)} else {callback(false)}}")%>%
          #   #   hot_col(17, type = "checkbox",default = FALSE, renderer = "function(instance, td, row, col, prop, value, cellProperties) {
          #   #     td.style.textAlign = 'center';
          #   #     Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          #   #     return td;}")
          # },
          # "patch" = {},
          # "enzyme" = {
          #   handsonTable <- handsonTable  %>%
          #     hot_col(c(4,5,6,7,8), type = "numeric")
          # }
  # )
  return(handsonTable)
}