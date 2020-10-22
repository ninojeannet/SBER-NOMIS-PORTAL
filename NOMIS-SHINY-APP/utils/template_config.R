
templateTypes <- c("Location"="location","Patch metrics"="patch","Enzyme"="enzyme")

tableList <- c("glacier","location","patch","enzyme","biogeo")

# List of all field name contained in each table
templateFieldNames <- list()
templateFieldNames[["gl_global"]] <- c('id_glacier','gl_name')
templateFieldNames[["gl_point"]] <- c('id_glacier','lat_sn','lon_sn','ele_sn','lat_up','lon_up','ele_up','lat_dn','lon_dn','ele_dn','max_el','mean_el')
templateFieldNames[["gl_line"]] <- c('id_glacier','sn_dist','sn_ele_diff','up_dn_dist','up_dn_ele_diff','abl_asp')
templateFieldNames[["gl_area"]] <- c('id_glacier','sa','cov_up','a_up')
templateFieldNames[["gl_invent"]] <- c('id_glacier','rgi_v6','glims_id','wgms_id','m_balance','mb_obs','fv_variation','fv_obs','sp_event')
templateFieldNames[["gl_source"]] <- c('id_glacier','data_s','data_ts','sn_data','sn_data_ts','dem_data','dem_data_ts','mb_data_s','fv_data_s','sp_event_s')
templateFieldNames[["gl_other"]] <- c('id_glacier','','','','')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','date','time','latitude','longitude','altitude','water_temp','ph','potential','do','do_sat','w_co2','conductivity','turb','rdna')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["enzyme"]] <- c("id_enzyme","id_patch","replicate","ag","ap","bg","lap","nag")
templateFieldNames[["biogeo"]] <- c("id_biogeo","id_location","replicate","filename_eem","filename_abs1","filename_abs10","doc","bix","fi")

subCategoriesOfTable <- list()
subCategoriesOfTable[["glacier"]] <- c('gl_global','gl_point','gl_line','gl_area','gl_invent','gl_source','gl_other')
# List of mandatory field for each table
mandatoryFields <- list()
mandatoryFields[["glacier"]] <- c('id_glacier')
mandatoryFields[["location"]] <- c('id_location','id_glacier','type')
mandatoryFields[["patch"]] <- c('id_patch','id_location','name')
mandatoryFields[["enzyme"]] <- c('id_enzyme','id_patch','replicate')
mandatoryFields[["biogeo"]] <- c('id_biogeo','id_location','replicate')

fullnameFields <- list()
fullnameFields[["gl_global"]] <- c('id_glacier','Glacier name')
fullnameFields[["gl_point"]] <- c('id_glacier','Snout latitude\n[DD]','Snout longitude\n[DD]','Snout elevation\n[m]','Up latitude\n[DD]','Up longitude\n[DD]','Up elevation\n[m]'
                                  ,'Down latitude\n[DD]','Down longitude\n[DD]','Down elevation\n[m]','Max elevation\n[m]','Mean elevation\n[m]')
fullnameFields[["gl_line"]] <- c('id_glacier','Up to snout\ndist\n[m]','Up to snout\nelevation diff\n[m]','Up to down\ndist\n[m]','Up to down\nelevation diff\n[m]','Aspect of ablation zone\n[\u00B0]')
fullnameFields[["gl_area"]] <- c('id_glacier','Glacier surface area\n[km2]','Glacier coverage Up\n[%]','Catchment area Up\n[km2]')
fullnameFields[["gl_invent"]] <- c('id_glacier','RGI v.6','GLIMS ID','WGMS ID','Mass balance\ndata availability','Mass balance\nobservations','Frontal variation\ndata availability','Frontal variation\nobservations','Special event')
fullnameFields[["gl_source"]] <- c('id_glacier','Glacier outline\ndata source','Glacier outline\ntimestamp','Snout position\ndata source','Snout position\ntime stamp','DEM data\nsource','DEM data\ntime stamp','Mass balance\ndata source','Frontal variation\ndata source','Special event\ndata source')
fullnameFields[["gl_other"]] <- c('id_glacier','','','','')
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
uploadDataTypes <- list("Location"="location","Patch"="patch",
                        `Glaciological metrics` = c("Global metrics"="gl_global","Point metrics"="gl_point","Line metrics"="gl_line","Area metrics"="gl_area","Inventory data"="gl_invent","Data sources"="gl_source","Other metrics"="gl_other"),
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
