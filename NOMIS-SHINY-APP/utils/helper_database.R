source('./utils/template_config.R')


# entryExistInTable <- function(pool,tablename,id){
#   request <- paste0("SELECT EXISTS( SELECT * FROM ",tablename," WHERE ",tableOptions[[tablename]][["primary"]],"=",id,")")
#   #Send query to the database using pool
#   check <- tryCatch({
#     conn <- poolCheckout(pool)
#     queryStatus <- dbWithTransaction(conn,{
#       result <- dbGetQuery(conn,request)
#     })
#     poolReturn(conn)
#     print("Data successfully inserted into database")
#     showNotification("Data successfully inserted into database",type = "message")
#   },
#   warning = function(war){
#     print(war)
#     showNotification(war$message, type = "warning")
#   },
#   error = function(err){
#     print(err)
#     showNotification(err$message,type = "error",duration = NULL)
#   },
#   finally = function(f){
#     print(e) 
#   })
#   
#   if (result == 1)
#     return(TRUE)
#   else
#     return(FALSE)
# }


# Retrieve data from the database for speicific glaciers
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

getFieldFromGlacier <- function(pool,tableName,field,ids){
  fields <- mandatoryFields[[tableName]]
  fields <- c(fields,field)
  fieldnames <- ""
  for (field in fields) {
    fieldnames <- paste0(fieldnames,field,",")
  }
  fieldnames <- substr(fieldnames,1,nchar(fieldnames)-1)
  query <- paste0("SELECT ",fieldnames," FROM ",tableName," WHERE ")
  pk <- tableOptions[[tableName]][["primary"]]
  for (id in ids) {
    query <- paste0(query,pk," LIKE '",id,"\\_%'")
    query <- paste0(query," OR ")
  }
  query <- substr(query,1,nchar(query)-4)
  print(query)
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
  print(query)
  if(is.atomic(sendQuery(query,pool,TRUE)))
    return(FALSE)
  else
    return(TRUE)
  # return(TRUE)
}


sendQuery <- function(query,pool,flag){
  check <- tryCatch({
    
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
      else
        request <- paste0(request,dbQuoteLiteral(pool,value),",")
      # request <- paste0(request,"?:,")
    }
    request <- substr(request,1,nchar(request)-1)
    request <- paste0(request,"),")
  }
  request <- substr(request,1,nchar(request)-1)
  request <- paste0(request," AS new_values ON DUPLICATE KEY UPDATE ")
  for(x in headers[-1]){request <- paste0(request,x,"=new_values.",x,", ")}
  request <- substr(request,1,nchar(request)-2)
  print(request)
  return(request)
}

saveFieldInDB <- function(tablename,field,pkValue,fkValue,uniqueValue,value,pool){
  pk <- tableOptions[[tablename]][["primary"]]
  fk <- tableOptions[[tablename]][["FK"]]
  unique <- tableOptions[[tablename]][["name"]]
  
    # request <- paste0('INSERT INTO ',tablename,' (',"`",pk,"`,","`",field,"`) VALUES ('",pkValue,"','",value,"')")
  # request <- paste0(request," AS new_values ON DUPLICATE KEY UPDATE ",field,"=new_values.",field)
  df <- setNames(data.frame(matrix(ncol =4, nrow = 1)), c(pk, fk, unique,field))
  df[[pk]] <- pkValue
  df[[fk]] <- fkValue
  df[[unique]] <- uniqueValue
  df[[field]] <- value
  print(df)
  query <- buildInsertQuery(df,tablename,pool)
  sendQuery(query,pool,FALSE)
}