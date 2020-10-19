
templateTypes <- c("Glacier"="glacier","Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

parametersList <- c("enzyme")

# List of all field name contained in each table
templateFieldNames <- list()
templateFieldNames[["glacier"]] <- c('id_glacier','name','country','type','area')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','date','time','latitude','longitude','altitude','water_temp','ph','potential','do','do_sat','w_co2','conductivity','turb','rdna')
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

fullnameFields <- list()
fullnameFields[["glacier"]] <- c('id_glacier','name','Country','Type','Area')
fullnameFields[["location"]] <- c('id_location','id_glacier','type','Date\n[DD/MM/YYYY]','Time\n[HH:MM]','Latitude\n[dd]','Longitude\n[dd]',
                                  'Altitude\n[m.a.s.l.]','Water Temp\n[\u00B0C]','pH\n[pH]','Potential\n[mV]','Dissolved Oxygen\n[mg l-1]','Dissolved Oxygen\n[saturation]','Water Co2\n[mATM]',
                                  'Conductivity\n[uS cm -1]','Turbidity\n[NTU]','Rock DNA\npresent\n[boolean]')
fullnameFields[["patch"]] <- c('id_patch','id_location','name')
fullnameFields[["enzyme"]] <- c("id_enzyme","id_patch","replicate","AG\n[nmol g-1 h-1]","AP\n[nmol g-1 h-1]","BG\n[nmol g-1 h-1]","LAP\n[nmol g-1 h-1]","NAG\n[nmol g-1 h-1]")
fullnameFields[["biogeo"]] <- c("id_biogeo","id_location","replicate","EEM filename","Abs 1cm filename","Abs 10cm filename","DOC","BIX","FI")

# List of options for each table to display in the upload section as "excel sheet"
tableOptions <- list()
tableOptions[["enzyme"]] <- c("primary"="id_enzyme", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["patch"]] <- c("primary"="id_patch", "FK"="id_location", "name"="name")
tableOptions[["location"]] <- c("primary"="id_location", "FK"="id_glacier", "name"="type")
tableOptions[["glacier"]] <- c("primary"="id_glacier", "FK"="", "name"="name")
tableOptions[["biogeo"]] <- c("primary"="id_biogeo", "FK"="id_location", "name"="replicate", "replicates"= list(c('1','2','3')))

# List of table that are only on UP location
isOnlyUP <- list()
isOnlyUP[["biogeo"]] <- TRUE

# The list of all the data types that can be uploaded from the upload section
uploadDataTypes <- list("Glacier" = "glacier","Location"="location","Patch"="patch",
                        `Microbial metrics` = c("Enzyme"="enzyme"),
                        `Biogeochemical metrics` = c("Dissolved organic matter"="biogeo","Dissolved organic carbon"="doc"))

# The sublist of all the "DOM type" that can be uploaded
uploadDOMTypes <- c("EEM" = "eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10")


colConfig <- list()
colConfig[["location"]] <- list(list(col=4,type = "date",dateFormat = "YYYY-MM-DD"),
                                list(col= 5, validator = "function (value, callback) {
              if (/^\\d{1,2}:\\d{2}($|:\\d{2}$)/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= c(6,7), validator = "function (value, callback) {
              if (/(\\-?|\\+?)?\\d+(\\.\\d+)?/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= c(9,10,11,12,13,14,15,16), validator = "function (value, callback) {
              if (/^[-]?\\d*\\.?\\d*$/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= 8, validator = "function (value, callback) {
              if (/\\d+/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= 17, type = "checkbox",default = FALSE, renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              td.style.textAlign = 'center';
              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              return td;}"))
colConfig[["enzyme"]] <- list(list(col=c(4,5,6,7,8), type = "numeric"))