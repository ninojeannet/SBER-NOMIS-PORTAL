
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
uniqueTogetherFields <- list()
uniqueTogetherFields[["location"]] <- c('glacierid','type')
uniqueTogetherFields[["patch"]] <- c('id_location','name')
uniqueTogetherFields[["enzyme"]] <- c('id_patch','replicate')

tableOptions <- list()
tableOptions[["enzyme"]] <- c("format"="0.000000000","replicate"=3,"primary"="id_enzyme", "FK"="id_patch", "name"="replicate")

