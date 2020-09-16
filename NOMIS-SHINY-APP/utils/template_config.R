
templateTypes <- c("Glacier"="glacier","Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

templateFieldNames <- list()
templateFieldNames[["glacier"]] <- c('id','name','country','type','area')
templateFieldNames[["location"]] <- c('idlocation','glacierid','type','altitude')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","AG","AP","BG","LAP","NAG")

# tableNameIndex <- list()
# tableNameIndex[["gl"]] <- "glacier"
# tableNameIndex[["loc"]] <- "location"
# tableNameIndex[["patch"]] <- "patch"
# tableNameIndex[["enzyme"]] <- "enzyme"