source('./utils/template_config.R')


getProgressTable <- function(pool){
  query <- "SELECT a.*,b.min,b.max
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

getFieldsWithFKFromGlacier <- function(pool,tablename,fields,ids){
  if(tableOptions[[tablename]][["FK"]] != "")
  {
    fk <- tableOptions[[tablename]][["FK"]]
    fields <- c(fk,fields)
  }
  return(getFieldsFromGlacier(pool,tablename,fields,ids))
}

# Retrieve specific field from a table in the database for specific glaciers
# Parameters : 
# - pool : the connection pool to access the database
# - tableName : the name of the database table to retrieve the field from
# - fields : list of fieldnames to retrieve
# - ids : list of ids to retrieve the field from
# Return the query result as dataframe
getFieldsFromGlacier <- function(pool,tableName,fields,ids){
  AllFields <- c()
  AllFields <- c(AllFields,fields)
  AllFields <- unique(AllFields)

  fieldnames <- ""
  for (field in fields) {
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
    # print(query)
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

## Parameters validation and parsing #######################################################

validInputString <- function(input) {
  if (!is.character(input)){
    return(SQL('NULL'))
  }
  
  if (input == '' | is.na(input) | length(input) != 1) {
    return(SQL('NULL'))
  }
  
  return(input)
}

## User queries ###################################################################

# Get user for login
loginUser <- function(pool, username) {
  pool %>% tbl('user') %>% filter(name == username, active == 1) %>% select(name, password, role) %>% head(1) %>% collect()
}



# Get all users
getUsers <- function(pool, columns = NULL) {
  # Initiate query with users table
  query <- pool %>% tbl('user')
  
  # Select columns if needed
  if (!is.null(columns)) {
    query %<>% select(all_of(columns), -password)
  } else {
    query %<>% select(-password)
  }
  
  # Perform query
  query %>% collect()
}



# Create a new user
createUser <- function(pool, username, password, role = 'sber', active = TRUE) {
  # Check for valid input string
  username <- validInputString(username)
  password <- validInputString(password)
  role <- validInputString(role)
  
  # Hash password before saving
  if (password != SQL('NULL')) {
    hashedPassword <- sodium::password_store(password)
  } else {
    hashedPassword <- SQL('NULL')
  }
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO user (name, password, role, active) values(?username, ?hashedPassword, ?role, ?active);',
    username = username, hashedPassword = hashedPassword, role = role, active = active
  )
  
  # Send Query and catch errors
  sendQuery(query,pool,TRUE)
}



updateUser <- function(pool, user, username = '', password = '', role = '', active = TRUE) {
  # Check for valid input string
  username <- validInputString(username)
  password <- validInputString(password)
  role <- validInputString(role)
  print("ww")
  print(user)
  print("ici")
  # Use previous values if not defined
  if (username == SQL('NULL')) username <- user$name
  if (role == SQL('NULL')) role <- user$role
  if (!is.logical(active) | is.na(active)) active <- user$active
  
  # UPdate password only if a new one is provided
  if (password == SQL('NULL')) {
    # Create query without password
    query <- sqlInterpolate(
      pool,
      "UPDATE user SET name = ?name, role = ?role, active = ?active WHERE id_user = ?id;",
      id = user$id_user, name = username, role = role, active = active
    )
  } else {
    # Hash the new password
    hashedPassword <- sodium::password_store(password)
    
    # Create query with password
    query <- sqlInterpolate(
      pool,
      "UPDATE user SET name = ?name, password = ?password, role = ?role, active = ?active WHERE id_user = ?id;",
      id = user$id_user, name = username, password = hashedPassword, role = role, active = active
    )
  }
  
  # Send Query and catch errors
  sendQuery(query,pool,TRUE)
}

## General queries ##############################################################

getEnumValues <- function(pool, table, column) {
  # Create query
  query <- sqlInterpolate(
    pool,
    "SELECT COLUMN_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ?table AND COLUMN_NAME = ?column AND DATA_TYPE = 'enum';",
    table = table, column = column
  )

  # Run query
  result <- dbGetQuery(pool, query)

  # If not empty, parse the info
  if (nrow(result) == 1) {
    result[1, 1] %>% str_extract_all("(?<=')[^,]+(?=')") %>% unlist()
  } else {
    NULL
  }
}


deleteRows <- function(pool, table, ids) {
  # Perform deletion only if ids is not NULL, numeric, not empty and does not contains NA
  if (is.null(ids)) {
    error <- 'Error: IDs cannot be NULL.'
  } else if (!is.numeric(ids)) {
    error <- 'Error: IDs should be a numeric vector.'
  } else if (length(ids) == 0) {
    error <- 'Error: IDs cannot be empty.'
  } else if (any(is.na(ids))) {
    error <- 'Error: IDs cannot be NA.'
  } else {
    # If ids is of length 1, make a specific query,
    # Otherwise, make a IN query
    if (length(ids) == 1) {
      query <- sqlInterpolate(
        pool,
        paste0("DELETE FROM ?table WHERE id_",table," = ?id;"),
        table = dbQuoteIdentifier(pool, table), id = ids
      )
    } else {
      query <- sqlInterpolateList(
        pool,
        paste0("DELETE FROM ?table WHERE id_",table," IN (?ids);"),
        vars = list(table = dbQuoteIdentifier(pool, table)),
        list_vars = list(ids = ids)
      )
    }
    # Send Query and catch errors
    return(sendQuery(query,pool,FALSE))
  }
  
  # If error return the error message
  return(error)
}

sqlInterpolateList <- function(conn, sql, vars=list(), list_vars=list()) {
  if (length(list_vars) > 0) {
    for (name in names(list_vars)) {
      sql <- sub(paste0("\\?", name), paste("?", name, "_list_var", 1:length(list_vars[[name]]), sep="", collapse=","), sql)
    }
    list_vars <- lapply(list_vars, function(sublist) {
      names(sublist) <- paste0("list_var", 1:length(sublist))
      sublist
    }) %>% unlist()
    # unlist gives names as "outer.inner" but DBI doesn't like names with periods
    names(list_vars) <- sub("\\.", "_", names(list_vars))
    vars <- c(vars, list_vars)
  }
  DBI::sqlInterpolate(conn, sql, .dots=vars)
}


updateExpeditionInDB <- function(pool, expedition, name = '', abreviation = '', ranges = list()) {
  # Check for valid input string
  print("yess")
  name <- validInputString(name)
  abreviation <- validInputString(abreviation)
  # print(expedition)
  # Use previous values if not defined
  if (name == SQL('NULL')) name <- expedition$name
  if (abreviation == SQL('NULL')) abreviation <- expedition$abreviation
  if (length(ranges) ==0) ranges <- expedition$range

  # Create query 
  query <- sqlInterpolate(
    pool,
    "UPDATE expedition SET name = ?name, abreviation = ?abreviation WHERE id_expedition = ?id_expedition;",
    id_expedition = expedition$id_expedition, name = name, abreviation = abreviation)
  # Send Query and catch errors
  sendQuery(query,pool,TRUE)
  
  oldRanges <- unlist(expedition$range)
  newRanges <- unlist(ranges)
  rangesToCreate <- setdiff(newRanges,oldRanges)
  rangesToDelete <- setdiff(oldRanges,newRanges)

  
  updateRanges(pool,expedition$id_expedition,name,rangesToCreate,rangesToDelete)
}

updateRanges <- function(pool,id_expedition,name_expedition,rangesToCreate,rangesToDelete){
  
  for (oldrange in rangesToDelete) {
    range <- str_split(oldrange," - ")
    query <- sqlInterpolate(
      pool,
      "DELETE FROM glacier_range WHERE id_expedition = ?id_expedition AND min=?min AND max=?max;",
      id_expedition = id_expedition,min=range[[1]][1],max=range[[1]][2])
    sendQuery(query,pool,FALSE)
  }
  print(rangesToCreate)
  for (range in rangesToCreate) {
    range <- str_split(range," - ")
    saveRangeInDB(name_expedition,as.numeric(range[[1]][1]),as.numeric(range[[1]][2]),pool)
  }
  
}

