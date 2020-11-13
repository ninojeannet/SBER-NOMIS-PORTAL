
# Save a log value in the log file on the server
# Parameters : 
# - type : the type of log (UPDATE, CREATE,..)
# - user: user that made the action
# - text : additional text
saveLog <- function(type,user,text){
  log <- paste0(now(),", ",type," by ",user," : ",text)
  
  destPath <- paste0("log/",type,"/")
  if (!dir.exists(destPath))
    dir.create(destPath,recursive = TRUE)
  filename <- paste0(type,"_log.txt")
  filepath <- paste0(destPath,filename)
  if(!file.exists(filepath))
     file.create(filepath)
  write(log,file=filepath,append=TRUE)
}