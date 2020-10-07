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
# Return a list containing the three vectors
generateFileTables <- function(filenames,files){
  tables <- list()
  tables[["valid"]] <- vector()
  tables[["wrong"]] <- vector()
  tables[["missing"]] <- vector()
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
  
  return(tables)
}


# Save file on the server
# Parameters :
# - name : name of the file to save
# - path : path of the file to save
# - tablename : name of the type of the file to save
saveFile <- function(name,path,tablename){
  # destPath <- paste0("data\\",tablename,"\\",name)
  tryCatch({
    destPath <- paste0("data/",tablename,"/",name)
    if (!dir.exists(paste0("data/",tablename,"/")))
      dir.create(paste0("data/",tablename,"/"),recursive = TRUE)
    file.copy(path,destPath,overwrite = TRUE)
    saveLog("upload","Nino",paste0("Upload file ",name," on the server"))
  },
  warning = function(war){
    print(war)
    # showNotification(war$message, type = "warning")
  },
  error = function(err){
    print(err)
    showNotification(err$message,type = "error",duration = NULL)
  },
  finally = function(f){
    print(f) 
  })

}