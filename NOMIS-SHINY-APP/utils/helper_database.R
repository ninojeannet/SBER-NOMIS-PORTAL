

getDataFromGlacier <- function(pool,tableName,glacierID){
  check <- tryCatch({
    conn <- poolCheckout(pool)
    queryStatus <- dbWithTransaction(conn,{
      if (tableName == "glacier")
        query <- paste0("SELECT * FROM ",tableName," WHERE id_",tableName," = '",glacierID,"'")
      else
        query <- paste0("SELECT * FROM ",tableName," WHERE id_",tableName," LIKE '",glacierID,"_%'")
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