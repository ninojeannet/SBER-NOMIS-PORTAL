
templateTypes <- c("Glacier"="glacier","Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

parametersList <- c("enzyme")

templateFieldNames <- list()
templateFieldNames[["glacier"]] <- c('id_glacier','name','country','type','area')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','altitude')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","AG","AP","BG","LAP","NAG")

readOnlyFields <- list()
readOnlyFields[["glacier"]] <- c('id_glacier')
readOnlyFields[["location"]] <- c('id_location','id_glacier','type')
readOnlyFields[["patch"]] <- c('id_patch','id_location','name')
readOnlyFields[["enzyme"]] <- c('id_enzyme','id_patch','replicate')
readOnlyFields[["biogeo"]] <- c('id_biogeo','id_location','replicate')



tableOptions <- list()
tableOptions[["enzyme"]] <- c("isParameter"=TRUE,"format"="0.000000000","primary"="id_enzyme", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["patch"]] <- c("isParameter"=FALSE,"format"="","primary"="id_patch", "FK"="id_location", "name"="name")
tableOptions[["location"]] <- c("isParameter"=FALSE,"format"="","primary"="id_location", "FK"="id_glacier", "name"="type")
tableOptions[["glacier"]] <- c("isParameter"=FALSE,"format"="","primary"="id_glacier", "FK"="", "name"="name")
tableOptions[["biogeo"]] <- c("isParameter"=TRUE,"format"="","primary"="id_biogeo", "FK"="id_location", "name"="replicate", "replicates"= list(c('1','2','3')))


