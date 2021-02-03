
# This function generate a complete dataframe for a specific table and existing data
# Generate a dataframe for each glacier in ids and then merge them together to have one dataframe
# parameters :
# - dataf : the existing data as a data frame
# - tablename : the name of data's table
# - ids : the list of dis of the data's glaciers
# Return the generated data frame of the given table
generateFilledDF <- function(dataf,tablename,ids){
  # print(dataf)
  finaldf <- dataf[FALSE,]
  
  for (glacierID in ids) {
    primaryKey <- tableOptions[[tablename]][["primary"]]
    if(tablename == "glacier")
      subsetdataf <- subset(dataf,dataf[[primaryKey]] == glacierID)
    else
      subsetdataf <- subset(dataf,grepl(paste0(glacierID,"_"), dataf[[primaryKey]], fixed = TRUE))
    
    if(ncol(subsetdataf) == 0)
      subsetdataf <- dataf[FALSE,]
    
    df <- switch (tablename,
                  'glacier' = generateGlacierDF(subsetdataf,glacierID),
                  'glacier_ud' = generateGlacierUDDF(subsetdataf,glacierID),
                  'location' = generateLocationDF(subsetdataf,glacierID),
                  'patch' = generatePatchDF(subsetdataf,glacierID),
                  'biogeo_3' = generateBiogeoDF(subsetdataf,tablename,glacierID),
                  'biogeo_1' = generateBiogeoDF(subsetdataf,tablename,glacierID),
                  'biogeo_3u' = generateBiogeoDF(subsetdataf,tablename,glacierID),
                  'biogeo_1u' = generateBiogeoDF(subsetdataf,tablename,glacierID),
                  generateParametersDF(subsetdataf,tablename,glacierID)
    )
    finaldf <- rbind(finaldf,df)
  }
  rownames(finaldf) = seq(length=nrow(finaldf))
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
  # print(dataf)
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < 1)
  {
    newdataf <- addRows(newdataf,-1,1,nbCol)
  }

  newdataf[[primary]] <- id
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)

  return(newdataf)

}

# Generate a specific dataframe from glacier ud data
# parameters :
# dataf : the dataframe of a glacier
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier
generateGlacierUDDF <- function(dataf,glacierID){
  primary <- tableOptions[["glacier_ud"]][["primary"]]
  fk_column <- tableOptions[["glacier_ud"]][["FK"]]
  idUP <- paste0(glacierID,"_UP")
  idDN <- paste0(glacierID,"_DN")
  ids <- c(idUP,idDN)
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
  
  newdataf[[primary]] <- ids
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  newdataf[[fk_column]] <- fk
  newdataf[["site"]] <- c("Up","Down")
  
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
  ids <- c(idUP,idDN)
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
  
  newdataf[[primary]] <- ids
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)

  newdataf[[fk_column]] <- fk
  newdataf[["type"]] <- c("Up","Down")
  
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
  ids <- reg_sort(ids,"GL[\\d+]_",-"_UP_|_DN_","_\\d")
  idsFk <- reg_sort(idsFk,"GL[\\d+]_",-"_UP|_DN")
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
  
  newdataf[[primary]] <- ids
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)

  newdataf[[fk]] <- idsFk
  newdataf[["name"]] <- patches
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
  
  ids <- reg_sort(ids,"GL[\\d+]_",-"_UP_|_DN_","_\\d_","_[ABC]")
  idsFk <- reg_sort(idsFk,"GL[\\d+]_",-"_UP|_DN","_\\d")
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
  
  newdataf[[primary]] <- ids
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)

  newdataf[[fk]] <- idsFk
  newdataf[["replicate"]] <- replicates
  return(newdataf)
}


# Generate a specific dataframe from biogeo data
# parameters :
# dataf : the dataframe glacier's biogeo data
# tablename : specify for which data the dataframe has to be generated
# galcierId : the ID of the given glacier
# Return a generated dataframe of glacier's biogeo values
generateBiogeoDF <- function(dataf,tablename,glacierID){
  # Get the column names from config file
  primary <- tableOptions[[tablename]][["primary"]]
  fk <- tableOptions[[tablename]][["FK"]]
  replicates <- tableOptions[[tablename]]["replicates"][[1]]
  
  # Generate all the ids
  idUP <- paste0(glacierID,"_UP")
  if(!isOnlyUP[[tablename]])
    idDN <- paste0(glacierID,"_DN")
  ids <- vector()
  idsFk <- vector()

  for (replicate in replicates) {
    idsFk <- c(idsFk,idUP)
    ids <- c(ids,paste0(idUP,"_",replicate))
    if(!isOnlyUP[[tablename]]){
      idsFk <- c(idsFk,idDN)
      ids <- c(ids,paste0(idDN,"_",replicate))
    }
  }
  
  ids <- reg_sort(ids,"GL[\\d+]_",-"_UP_|_DN_","_[ABC]")
  idsFk <- reg_sort(idsFk,"GL[\\d+]_",-"_UP|_DN")
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

  newdataf[[primary]] <- ids
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)

  newdataf[[fk]] <- idsFk
  newdataf[["replicate"]] <- replicates
  
  return(newdataf)
}

# Set the default column names of a specific dataframe for database communication
# Column names are taken from the config file
# Parameters : 
# - dataframe : the dataframe to update the column names to
# - tablename : the name of the table 
# Return the updated dataframe
setDefaultColumnName <- function(dataframe,tablename){
  if(tablename %in% names(templateFieldNames))
    colnames(dataframe) <- templateFieldNames[[tablename]]
  return(dataframe)
}

# Set the complete column names of a specific dataframe for display
# Column names are taken from the config file
# Parameters : 
# - dataframe : the dataframe to update the column names to
# - tablename : the name of the table 
# Return the updated dataframe
setCompleteColumnName <- function(dataframe,tablename){
  if(tablename %in% names(fullnameFields))
    colnames(dataframe) <- fullnameFields[[tablename]]
  return(dataframe)
}