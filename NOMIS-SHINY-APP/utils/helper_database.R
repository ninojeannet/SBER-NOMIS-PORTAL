source('./utils/template_config.R')


getProgressTable <- function(pool){
  query <- "SELECT a.id_expedition,a.name,a.abreviation,a.enzyme,a.doc,a.dom,b.min,b.max
  FROM expedition as a,glacier_range as b 
  WHERE a.id_expedition = b.id_expedition"
  return(sendQuery(query,pool,FALSE))
}

# Retrieve entire table from the database for specific glaciers
# Parameters :
# pool : the connection pool to access the database
# tablename : the name of the table to query in the database
# ids : list of glacier's id to query the data from
# isRange : specify if there is multiple glacier to query
# Return the query result as dataframe
getTableFromGlacier <- function(pool,tableName,ids){
  pk <- tableOptions[[tableName]][["primary"]]
  if (tableName == "glacier")
  {
    query <- paste0("SELECT * FROM ",tableName," WHERE ")
    for (id in ids) {
      query <- paste0(query,pk," = '",id,"'")
      query <- paste0(query," OR ")
    }
  }
  else
  {
    query <- paste0("SELECT * FROM ",tableName," WHERE ")
    for (id in ids) {
      query <- paste0(query,pk," LIKE '",id,"\\_%'")
      query <- paste0(query," OR ")
    }
  }
  query <- substr(query,1,nchar(query)-4)

  dataframe <- sendQuery(query,pool,FALSE)
  
  return(dataframe)
}

# Retrieve specific field from a table in the database for specific glaciers
# Parameters : 
# - pool : the connection pool to access the database
# - tableName : the name of the database table to retrieve the field from
# - fields : list of fieldnames to retrieve
# - ids : list of ids to retrieve the field from
# Return the query result as dataframe
getFieldsFromGlacier <- function(pool,tableName,fields,ids){
  AllFields <- mandatoryFields[[tableName]]
  AllFields <- c(AllFields,fields)
  AllFields <- unique(AllFields)

  fieldnames <- ""
  for (field in AllFields) {
    fieldnames <- paste0(fieldnames,field,",")
  }
  fieldnames <- substr(fieldnames,1,nchar(fieldnames)-1)
  query <- paste0("SELECT ",fieldnames," FROM ",tableName," WHERE ")
  pk <- tableOptions[[tableName]][["primary"]]
  if (tableName == "glacier")
  {
    for (id in ids) {
      query <- paste0(query,pk," = '",id,"'")
      query <- paste0(query," OR ")
    }
  }
  else
  {
    for (id in ids) {
      query <- paste0(query,pk," LIKE '",id,"\\_%'")
      query <- paste0(query," OR ")
    }
  }

  query <- substr(query,1,nchar(query)-4)
  dataframe <- sendQuery(query,pool,FALSE)
  # 
  return(dataframe)
}

# Save the given data into the database
# Parameters : 
# data : the data as dataframe to save into db
# tablename : the name of the table to save the data into
# pool : connection pool to access the database
saveData <- function(data,tableName,pool){
  query <- buildInsertQuery(data,tableName,pool)
  if(is.atomic(sendQuery(query,pool,TRUE)))
    return(FALSE)
  else
    return(TRUE)
}

# Send a given query to the database and return the result
# Parameters : 
# - query : the sql query as a string
# - pool : the database connection pool
# - flag : flag to show success message as boolean
# return the query result
sendQuery <- function(query,pool,flag){
  check <- tryCatch({
    print(query)
    dataframe <-dbGetQuery(pool,query)
    
    if(flag)
      showNotification("Data successfully inserted into database",type = "message")
    return(dataframe)
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
      print(f) 
    })
  return(FALSE)
}


# Build an insert sql query
# Params :
# - data : dataframe of data to insert into the database
# - tableName : name of the table in which the data will be inserted
# Return the built request in a string
buildInsertQuery <- function(data,tableName,pool){
  request <- paste0(c('INSERT INTO ',tableName,' ('))
  headers <-colnames(data)
  for(x in headers){request <- paste0(c(request,"`",x,"`, "))}
  request <- paste(request,collapse = '')
  request <- substr(request,1,nchar(request)-2)
  request <- paste0(request,") VALUES ")
  for(row in 1:nrow(data)){
    request <- paste0(request,"(")
    for(value in data[row,])
    {
      if(is.na(value) | value == '')
        request <- paste0(request,"NULL,")
      else{
        if(is.character(value))
          request <- paste0(request,dbQuoteString(pool,value),",")
        else
          request <- paste0(request,value,",")
      }
    }
    request <- substr(request,1,nchar(request)-1)
    request <- paste0(request,"),")
  }
  request <- substr(request,1,nchar(request)-1)
  request <- paste0(request," AS new_values ON DUPLICATE KEY UPDATE ")
  for(x in headers[-1]){request <- paste0(request,x,"=new_values.",x,", ")}
  request <- substr(request,1,nchar(request)-2)

  return(request)
}

# Save a specific field from a table in the database
# Parameters : 
# - tablename : name of the table to save the field value into
# - field : the name of the field to save
# - pkvalue : the primary key of the table
# - fkValue : the foreign key of the table
# - uniqueValue : the unique value of the table
# - value : the value to insert into the database
# - pool : the database connection pool
saveFieldInDB <- function(tablename,field,pkValue,fkValue,uniqueValue,value,pool){
  pk <- tableOptions[[tablename]][["primary"]]
  fk <- tableOptions[[tablename]][["FK"]]
  unique <- tableOptions[[tablename]][["name"]]

  df <- setNames(data.frame(matrix(ncol =4, nrow = 1)), c(pk, fk, unique,field))
  df[[pk]] <- pkValue
  df[[fk]] <- fkValue
  df[[unique]] <- uniqueValue
  df[[field]] <- value
  query <- buildInsertQuery(df,tablename,pool)
  sendQuery(query,pool,FALSE)
}




# Save a new expedition row in the database
# Parameters :
# - name : name of the expedition
# - abr : expedition's abreviation
# - pool: the database connection pool
saveExpeditionInDB <- function(name,abr,pool)
{
  request <- 'INSERT INTO expedition (`name`,`abreviation`) VALUES (?name,?abr)'
  query <- sqlInterpolate(pool,request,name=name,abr=abr)
  sendQuery(query,pool,TRUE)
}

# Save a new range row in the database
# Parameters : 
# - exp_name : expedition name linked to the new range
# - min : minimum value of the range
# - max : maximum value of the range
# - pool : the database connection pool
saveRangeInDB <- function(exp_name,min,max,pool)
{
  request <- 'INSERT INTO glacier_range (`id_expedition`,`min`,`max`) VALUES (?exp,?min,?max)'
  exp_id <-dbGetQuery(pool,paste0("SELECT id_expedition FROM expedition WHERE name =",dbQuoteString(pool,exp_name)))
  exp_id <-exp_id[['id_expedition']]
  query <- sqlInterpolate(pool,request,exp=exp_id,min=min,max=max)
  sendQuery(query,pool,FALSE)
}


getNbOfNotNULLEntries <- function(table,fields,ids,nbOfEntry,pool){
  result <- 0
  for (id in ids) {
    query <- buildCountQuery(table,fields,id,nbOfEntry)
    nb <- as.numeric(sendQuery(query,pool,FALSE))
    result <- result + nb 
    
  }
  # print(result)
  return(result)
}

buildCountQuery <- function(table,fields,id,nbOfEntry){
  query <- 'SELECT if( COUNT(  IF('
  for (field in fields) {
    query <- paste0(query,field," IS NOT NULL AND ")
  }
  query <- substr(query,1,nchar(query)-5)
  query <- paste0(query,", 1, NULL)) = ",nbOfEntry,",1,0 ) AS result FROM ",table," WHERE ")
  pk <- tableOptions[[table]][["primary"]]
  query <- paste0(query,pk," LIKE '",id,"\\_%'")
  return(query)
}
