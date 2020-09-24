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
getDataFromGlacier <- function(pool,tableName,ids,isRange){
  if(isRange)
  {
    if (tableName == "glacier")
    {
      query <- paste0("SELECT * FROM ",tableName," WHERE ")
      for (id in ids) {
        query <- paste0(query,"id_",tableName," = '",id,"'")
        query <- paste0(query," OR ")
      }
    }
    else
    {
      query <- paste0("SELECT * FROM ",tableName," WHERE ")
      for (id in ids) {
        query <- paste0(query,"id_",tableName," LIKE '",id,"\\_%'")
        query <- paste0(query," OR ")
      }
    }
    query <- substr(query,1,nchar(query)-4)
  }
  else
  {
    if (tableName == "glacier")
      query <- paste0("SELECT * FROM ",tableName," WHERE id_",tableName," = '",ids,"'")
    else
      query <- paste0("SELECT * FROM ",tableName," WHERE id_",tableName," LIKE '",ids,"\\_%'")
  }
  
  check <- tryCatch({
    conn <- poolCheckout(pool)
    queryStatus <- dbWithTransaction(conn,{
      print(query)
      dataframe <-dbGetQuery(conn,query)
      print(dataframe)
    })
    poolReturn(conn)},
    warning = function(war){
      print(war)
      showNotification(war$message, type = "warning")
    },
    error = function(err){
      print(err)
      showNotification(err$message,type = "error",duration = NULL)
    },
    finally = function(f){
      print(e) 
    })
  return(dataframe)
}

# Save the given data into the database
# Parameters : 
# data : the data as dataframe to save into db
# tablename : the name of the table to save the data into
# pool : connection pool to access the database
saveData <- function(data,tableName,pool){
  request <- buildInsertQuery(data,tableName)
  
  #Send query to the database using pool
  check <- tryCatch({
    conn <- poolCheckout(pool)
    queryStatus <- dbWithTransaction(conn,{
      dbGetQuery(conn,request)
    })
    poolReturn(conn)
    print("Data successfully inserted into database")
    showNotification("Data successfully inserted into database",type = "message")
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
    print(e) 
  })
  
}


# Build an insert sql query
# Params :
# - data : dataframe of data to insert into the database
# - tableName : name of the table in which the data will be inserted
# Return the built request in a string
buildInsertQuery <- function(data,tableName){
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
      if(is.na(value))
        request <- paste0(request,"NULL,")
      else
        request <- paste0(request,"'",value,"',")
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