
templateTypes <- c("Glacier"="glacier","Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

parametersList <- c("enzyme")

# List of all field name contained in each table
templateFieldNames <- list()
templateFieldNames[["glacier"]] <- c('id_glacier','name','country','type','area')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','date','time','latitude','longitude','altitude','water_temp','do','do_sat','w_co2','ph','potential','conductivity','turb')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","ag","ap","bg","lap","nag")
templateFieldNames[["biogeo"]] <- c("id_biogeo","id_location","replicate","filename_eem","filename_abs1","filename_abs10","doc","bix","fi")

# List of mandatory field for each table
mandatoryFields <- list()
mandatoryFields[["glacier"]] <- c('id_glacier')
mandatoryFields[["location"]] <- c('id_location','id_glacier','type')
mandatoryFields[["patch"]] <- c('id_patch','id_location','name')
mandatoryFields[["enzyme"]] <- c('id_enzyme','id_patch','replicate')
mandatoryFields[["biogeo"]] <- c('id_biogeo','id_location','replicate')

# List of options for each table to display in the upload section as "excel sheet"
tableOptions <- list()
tableOptions[["enzyme"]] <- c("format"="0.000000000","primary"="id_enzyme", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["patch"]] <- c("format"="","primary"="id_patch", "FK"="id_location", "name"="name")
tableOptions[["location"]] <- c("format"="","primary"="id_location", "FK"="id_glacier", "name"="type")
tableOptions[["glacier"]] <- c("format"="","primary"="id_glacier", "FK"="", "name"="name")
tableOptions[["biogeo"]] <- c("format"="","primary"="id_biogeo", "FK"="id_location", "name"="replicate", "replicates"= list(c('1','2','3')))

# List of table that are only on UP location
isOnlyUP <- list()
isOnlyUP[["biogeo"]] <- TRUE

# The list of all the data types that can be uploaded from the upload section
uploadDataTypes <- list("Glacier" = "glacier","Location"="location","Patch"="patch",
                        `Microbial metrics` = c("Enzyme"="enzyme"),
                        `Biogeochemical metrics` = c("Dissolved organic matter"="biogeo","Dissolved organic carbon"="doc"))

# The sublist of all the "DOM type" that can be uploaded
uploadDOMTypes <- c("EEM" = "eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10")