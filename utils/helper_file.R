# Generate all filename from a range of ids and a tablename
# Parameters :
# - ids : list of ids to generate the filenames from
# - tablename : name of the table of the given ids
# Return a vector of filenames as string
generateFilenames <- function(ids,tablename){
  filenames <- vector()
  for (id in ids) {
    f1 <- paste0(id,"_UP_A_",tablename,".dat")
    f2 <- paste0(id,"_UP_B_",tablename,".dat")
    f3 <- paste0(id,"_UP_C_",tablename,".dat")
    
    filenames<-c(filenames,f1)
    filenames<-c(filenames,f2)
    filenames<-c(filenames,f3)
    
  }
  return(filenames)
}

# Generate a list containing three vector : valid, wrong and missing each representing 
# a vector of valid file. wrong file and missing files.
# These vectors are genrerated from a given list of files and a list of valid filenames
# Parameters : 
# - filenames : vector of all the valid filenames. Used to check the validity of each files
# - files : vector of files to check
# - existingFiles : 
# - isUploadOnly : 
# Return a list containing the three vectors
generateFileTables <- function(filenames,files,existingFiles,isUploadOnly){
  tables <- list()
  tables[["valid"]] <- vector()
  tables[["wrong"]] <- vector()
  tables[["missing"]] <- vector()
  tables[["existing"]] <- vector()
  for (filename in filenames) {
    if (filename %in% files[["name"]])
      tables[["valid"]] <- c(tables[["valid"]],filename)
    else
      tables[["missing"]] <- c(tables[["missing"]],filename)
  }
  for (val in files[["name"]]) {
    if(!(val %in% tables[["valid"]]) & !(val %in% tables[["missing"]]))
      tables[["wrong"]] <- c(tables[["wrong"]],val)
    
  }
  tables[["existing"]] <- existingFiles
  if(isUploadOnly)
    tables[["valid"]] <- setdiff(tables[["valid"]],tables[["existing"]])
  
  return(tables)
}

# Return a list of existing filenames in the database
# Parameters :
# - pool : the database connection pool
# - table : name of the table to query
# - field : name of the field to check its files
# - ids : list of glacier ids
getExistingFilenamesInDB <- function(pool,table,field,ids){
  filenames <- getFieldsFromGlacier(pool,table,paste0("filename_",field),ids)[[paste0("filename_",field)]]
  filenames <- filenames[!is.na(filenames)]
  if(length(filenames)==0)
    return(FALSE)
  return(filenames)
}

getExistingExpedFilenameInDB <- function(pool,expedition,fileType){
  t <- getTable("expedition",pool)
  t <- t[t$id_expedition == as.numeric(expedition),fileType]
  if(is.na(t))
    return(FALSE)
  return(t)
}

# Save file on the server
# Parameters :
# - name : name of the file to save
# - path : path of the file to save
# - pathTo : path of the destination directory
# - type : name of the type of the file to save
saveFile <- function(name,path,pathTo,type){
  tryCatch({
    destPath <- paste0(pathTo,type,"/",name)
    if (!dir.exists(paste0(pathTo,type,"/")))
      dir.create(paste0(pathTo,type,"/"),recursive = TRUE)
    file.copy(path,destPath,overwrite = TRUE)
    saveLog("upload","Nino",paste0("Upload file ",name," on the server"))
  },
  warning = function(war){
    print(war)
  },
  error = function(err){
    print(err)
    showNotification(err$message,type = "error",duration = NULL)
  },
  finally = function(f){
    print(f) 
  })

}

deleteFile <- function(path,filename){
  if (file.exists(paste0(path,filename))) {
    #Delete file if it exists
    file.remove(paste0(path,filename))
  }
}

getFilePath <- function(type,filename){
  return(paste0("data/",type,"/",filename))
}

# Given a set of filename and validfiles, process and store them in the server with a specific hierarchy
# show and update a progress bar indicating the upload status
# Parameters : 
# - validFilename : List of all valid filename
# - validFiles : dataframe of the valid selected files with name and datapath
# - tablename : the name of the table to save the filename to
# - type : the type of file (eem,abs1,abs10,..)
# - pool : the database connection pool
processDOMFiles <- function(validFilename,validFiles,tablename,type,pool){
  tryCatch({
    withProgress(message = "Saving valid files",value = 0,{
      nbOfFiles <- nrow(validFiles)
      for (row in 1:nrow(validFiles)) {
        name <- validFiles[row,"name"]
        saveFile(name,validFiles[row,"datapath"],"data/",type)
        pkValue <- str_remove(name,"_[^_]+\\..+")
        fkValue <- str_remove(name,"_[^_]+_[^_]+\\..+")
        replicate <- str_extract(str_extract(name,"_[ABC]_"),"[ABC]")
        saveFieldInDB(tablename,paste0("filename_",type),pkValue,fkValue,replicate,name,pool)
        incProgress(1/nbOfFiles, detail = paste("Saving ", name))
      }
    })
    showNotification("Files successfully inserted into database",type = "message")
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
}


isExtensionValid <- function(file,fileType){
  exts <- fileExtension[[fileType]]
  ext <- file_ext(file)
  if(ext %in% exts)
    return(TRUE)
  else
    return(FALSE)
}

saveMapURL <- function(url){
  if (!dir.exists("data/url/"))
    dir.create("data/url/",recursive = TRUE)
  if(!file.exists("data/url/map.txt"))
    file.create("data/url/map.txt")
  fileConn<-file("data/url/map.txt")
  writeLines(c(url), fileConn)
  close(fileConn)
}

getMapURL <- function(){
  if(!file.exists("data/url/map.txt"))
    return("")
  fileConn<-file("data/url/map.txt")
  value <- readLines(fileConn)
  close(fileConn)
  return(value)
}