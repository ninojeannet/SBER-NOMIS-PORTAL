
# This function generate a complete dataframe for a specific table and existing data
# Generate a dataframe for each glacier in ids and then merge them together to have one dataframe
# parameters :
# - dataf : the existing data as a data frame
# - tablename : the name of data's table
# - ids : the list of dis of the data's glaciers
# Return the generated data frame of the given table
generateFilledDF <- function(dataf,tablename,ids){
  finaldf <- dataf[FALSE,]
  
  for (glacierID in ids) {
    primaryKey <- tableOptions[[tablename]][["primary"]]
    if(tablename == "glacier")
      subsetdf <- subset(dataf,dataf[primaryKey] == glacierID)
    else
      subsetdf <- subset(dataf,grepl(paste0(glacierID,"_"), dataf[primaryKey], fixed = TRUE))
    if(ncol(subsetdf) == 0)
      subsetdf <- dataf[FALSE,]
    print(subsetdf)
    df <- switch (tablename,
                  'glacier' = generateGlacierDF(subsetdf,glacierID),
                  'location' = generateLocationDF(subsetdf,glacierID),
                  'patch' = generatePatchDF(subsetdf,glacierID),
                  generateParametersDF(subsetdf,tablename,glacierID)
    )
    finaldf <- rbind(finaldf,df)
  }
  return(finaldf)
}

# Generate a specific dataframe from glacier data
# parameters :
# dataf : the dataframe of a glacier
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier
generateGlacierDF <- function(dataf,glacierID){
  primary <- tableOptions[["glacier"]][["primary"]]
  id <- glacierID
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  print(nrow(newdataf))
  # Add new rows to the df if necessary
  if(nbRow < 1)
  {
    print("add rows")
    newdataf <- addRows(newdataf,-1,1,nbCol)
  }
  # print(newdataf)
  # Fill the df with generated columns
  newdataf[[primary]] <- id
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
}

# Generate a specific dataframe from location data
# parameters :
# dataf : the dataframe glacier's locations
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier's locations
generateLocationDF <- function(dataf,glacierID){
  primary <- tableOptions[["location"]][["primary"]]
  fk_column <- tableOptions[["location"]][["FK"]]
  idUP <- paste0(glacierID,"_UP")
  idDN <- paste0(glacierID,"_DN")
  ids <- c(idDN,idUP)
  fk <- glacierID
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }
  
  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk_column]] <- fk
  newdataf[["type"]] <- c("Down","Up")
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
  
}

# Generate a specific dataframe from Patch data
# parameters :
# dataf : the dataframe glacier's patches
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier's patches
generatePatchDF <- function(dataf,glacierID){
  primary <- tableOptions[["patch"]][["primary"]]
  fk <- tableOptions[["patch"]][["FK"]]
  idUP <- paste0(glacierID,"_UP")
  idDN <- paste0(glacierID,"_DN")
  ids <- vector()
  idsFk <- vector()
  patches <- c(1,2,3)
  for (patch in patches) {
    idsFk <- c(idsFk,idUP)
    idsFk <- c(idsFk,idDN)
    ids <- c(ids,paste0(idUP,"_",patch))
    ids <- c(ids,paste0(idDN,"_",patch))
  }
  ids <- sort(ids)
  idsFk <- sort(idsFk)
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }
  
  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk]] <- idsFk
  newdataf[["name"]] <- patches
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
}

# Generate a specific dataframe from parameters data
# parameters :
# dataf : the dataframe glacier's parameters
# tablename : specify for which parameters the dataframe has to be generated
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier's parameters
generateParametersDF <- function(dataf,tablename,glacierID){
  # Get the column names from config file
  primary <- tableOptions[[tablename]][["primary"]]
  fk <- tableOptions[[tablename]][["FK"]]
  replicates <- tableOptions[[tablename]]["replicates"][[1]]
  
  # Generate all the ids
  idUP <- paste0(glacierID,"_UP_")
  idDN <- paste0(glacierID,"_DN_")
  ids <- vector()
  idsFk <- vector()
  for (patch in 1:3) {
    for (replicate in replicates) {
      idsFk <- c(idsFk,paste0(idUP,patch))
      idsFk <- c(idsFk,paste0(idDN,patch))
      ids <- c(ids,paste0(idUP,patch,"_",replicate))
      ids <- c(ids,paste0(idDN,patch,"_",replicate))
    }
  }
  
  ids <- sort(ids)
  print(length(idsFk))
  idsFk <- sort(idsFk)
  print(idsFk)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }
  
  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk]] <- idsFk
  newdataf[["replicate"]] <- replicates
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
}