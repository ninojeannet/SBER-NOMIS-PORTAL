# Generate all filename from a range of ids and a tablename
# Parameters :
# - ids : list of ids to generate the filenames from
# - tablename : name of the table of the given ids
# Return a vector of filenames as string
generateFilenames <- function(ids,tablename){
  filenames <- vector()
  for (id in ids) {
    f1 <- paste0(id,"_UP_1_",tablename,".dat")
    f2 <- paste0(id,"_UP_2_",tablename,".dat")
    f3 <- paste0(id,"_UP_3_",tablename,".dat")
    
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
  filenames <- getFieldFromGlacier(pool,table,paste0("filename_",field),ids)[[paste0("filename_",field)]]
  filenames <- filenames[!is.na(filenames)]
  return(filenames)
}


# Save file on the server
# Parameters :
# - name : name of the file to save
# - path : path of the file to save
# - tablename : name of the type of the file to save
saveFile <- function(name,path,tablename){
  tryCatch({
    destPath <- paste0("data/",tablename,"/",name)
    if (!dir.exists(paste0("data/",tablename,"/")))
      dir.create(paste0("data/",tablename,"/"),recursive = TRUE)
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

# Given a set of filename and validfiles, process and store them in the server with a specific hierarchy
# show and update a progress bar indicating the upload status
# Parameters : 
# - validFilename : List of all valid filename
# - validFiles : dataframe of the valid selected files with name and datapath
# - tablename : the name of the table to save the filename to
# - type : the type of file (eem,abs1,abs10,..)
# - pool : the database connection pool
processFiles <- function(validFilename,validFiles,tablename,type,pool){
  tryCatch({
    withProgress(message = "Saving valid files",value = 0,{
      print(validFiles)
      nbOfFiles <- nrow(validFiles)
      for (row in 1:nrow(validFiles)) {
        name <- validFiles[row,"name"]
        saveFile(name,validFiles[row,"datapath"],type)
        pkValue <- str_remove(name,"_[^_]+\\..+")
        fkValue <- str_remove(name,"_[^_]+_[^_]+\\..+")
        replicate <- str_extract(str_extract(name,"_\\d_"),"\\d")
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