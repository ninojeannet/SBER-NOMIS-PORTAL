
templateTypes <- c("Glacier"="gl","Location"="loc","Patch metrics"="patch","Enzyme"="enzyme")

templateFieldNames <- list()
templateFieldNames[["gl"]] <- c('id','name','country','type','area')
templateFieldNames[["loc"]] <- c('idlocation','glacierid','type','altitude')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","AG","AP","BG","LAP","NAG")

tableNameIndex <- list()
tableNameIndex[["gl"]] <- "glacier"
tableNameIndex[["loc"]] <- "location"
tableNameIndex[["patch"]] <- "patch"
tableNameIndex[["enzyme"]] <- "enzyme"