
templateTypes <- c("Glacier"="glacier","Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

parametersList <- c("enzyme")

templateFieldNames <- list()
templateFieldNames[["glacier"]] <- c('id_glacier','name','country','type','area')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','altitude')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","AG","AP","BG","LAP","NAG")
templateFieldNames[["biogeo"]] <- c("id_biogeo","id_location","replicate","filename_eem","filename_abs1","filename_abs10","doc","bix","fi")

mandatoryFields <- list()
mandatoryFields[["glacier"]] <- c('id_glacier')
mandatoryFields[["location"]] <- c('id_location','id_glacier','type')
mandatoryFields[["patch"]] <- c('id_patch','id_location','name')
mandatoryFields[["enzyme"]] <- c('id_enzyme','id_patch','replicate')
mandatoryFields[["biogeo"]] <- c('id_biogeo','id_location','replicate')

tableOptions <- list()
tableOptions[["enzyme"]] <- c("format"="0.000000000","primary"="id_enzyme", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["patch"]] <- c("format"="","primary"="id_patch", "FK"="id_location", "name"="name")
tableOptions[["location"]] <- c("format"="","primary"="id_location", "FK"="id_glacier", "name"="type")
tableOptions[["glacier"]] <- c("format"="","primary"="id_glacier", "FK"="", "name"="name")
tableOptions[["biogeo"]] <- c("format"="","primary"="id_biogeo", "FK"="id_location", "name"="replicate", "replicates"= list(c('1','2','3')))


isOnlyUP <- list()
isOnlyUP[["biogeo"]] <- TRUE