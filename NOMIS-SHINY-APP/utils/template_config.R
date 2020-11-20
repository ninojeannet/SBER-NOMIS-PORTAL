
tableList <- c("glacier","location","patch","microbial_1","microbial_2","microbial_3","biogeo_3","biogeo_1")

# List of all field name contained in each table
templateFieldNames <- list()
templateFieldNames[["gl_global"]] <- c('id_glacier','gl_name')
templateFieldNames[["gl_point"]] <- c('id_glacier','lat_sn','lon_sn','ele_sn','lat_up','lon_up','ele_up','lat_dn','lon_dn','ele_dn','max_el','mean_el')
templateFieldNames[["gl_line"]] <- c('id_glacier','sn_dist','sn_ele_diff','up_dn_dist','up_dn_ele_diff','abl_asp')
templateFieldNames[["gl_area"]] <- c('id_glacier','sa','cov_up','a_up')
templateFieldNames[["gl_invent"]] <- c('id_glacier','rgi_v6','glims_id','wgms_id','m_balance','mb_obs','fv_variation','fv_obs','sp_event')
templateFieldNames[["gl_source"]] <- c('id_glacier','data_s','data_ts','sn_data','sn_data_ts','dem_data','dem_data_ts','mb_data_s','fv_data_s','sp_event_s')
templateFieldNames[["gl_other"]] <- c('id_glacier','','','','')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','date','time','water_temp','ph','potential','do','do_sat','w_co2','conductivity','turb','rdna')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
# templateFieldNames[["biogeo_3"]] <- c("id_biogeo_3","id_location","replicate","filename_eem","filename_abs1","filename_abs10",
#                                     "doc","abs254","abs300","suva","e2e3","e4e6","s275295","s350400","s300700",
#                                     "sr","bix","fi","hix","coble_b","coble_t","coble_a","coble_m","coble_c","coble_r")
# templateFieldNames[["coble"]] <- c("id_biogeo_3","id_location","replicate","coble_b","coble_t","coble_a","coble_m","coble_c","coble_r")
templateFieldNames[["indices"]] <- c("id_biogeo_3","id_location","replicate","abs254","abs300",
                                        "suva","e2e3","e4e6","s275295","s350400","s300700","sr","bix","fi","hix",
                                     "coble_b","coble_t","coble_a","coble_m","coble_c","coble_r")
templateFieldNames[["ion"]] <- c("id_biogeo_1","id_location","replicate","i1_na","i2_k","i3_mg","i4_ca","i5_cl","i6_so4")
templateFieldNames[["nutrient"]] <- c("id_biogeo_1","id_location","replicate","n1_tn","n2_tp","n3_srp","n4_nh4","n5_no3","n6_no2")
# templateFieldNames[["microbial_1"]] <- c('id_microbial_1',"id_patch",'replicate',"eps","wba","sba")
# templateFieldNames[["microbial_2"]] <- c('id_microbial_2',"id_patch",'replicate',"bp","respiration")
# templateFieldNames[["microbial_3"]] <- c('id_microbial_3',"id_patch",'replicate',"chla","ag","ap","bg","lap","nag")
templateFieldNames[["enzyme"]] <- c("id_microbial_3","id_patch","replicate","ag","ap","bg","lap","nag")
templateFieldNames[["chla"]] <- c("id_microbial_3","id_patch","replicate","chla")
templateFieldNames[["eps"]] <- c("id_microbial_1","id_patch","replicate","eps")
templateFieldNames[["bp"]] <- c("id_microbial_2","id_patch","replicate","bp")
templateFieldNames[["ba"]] <- c("id_microbial_1","id_patch","replicate","wba","sba")
templateFieldNames[["respiration"]] <- c("id_microbial_2","id_patch","replicate","respiration")
templateFieldNames[["doc"]] <- c("id_biogeo_3","id_location","replicate","doc")

subCategoriesOfTable <- list()
subCategoriesOfTable[["glacier"]] <- c('gl_global','gl_point','gl_line','gl_area','gl_invent','gl_source')#,'gl_other')
subCategoriesOfTable[["biogeo_3"]] <- c("dom","indices","doc", "eem","abs1","abs10")
subCategoriesOfTable[["biogeo_1"]] <- c("ion","nutrient")
subCategoriesOfTable[["microbial_1"]] <- c("ba","eps")
subCategoriesOfTable[["microbial_2"]] <- c("respiration","bp")
subCategoriesOfTable[["microbial_3"]] <- c("enzyme","chla")

specificFields <- list()
specificFields[["dom"]] <- c("filename_eem","filename_abs1","filename_abs10")

# List of mandatory field for each table
mandatoryFields <- list()
mandatoryFields[["glacier"]] <- c('id_glacier')
mandatoryFields[["location"]] <- c('id_location','id_glacier','type')
mandatoryFields[["patch"]] <- c('id_patch','id_location','name')
mandatoryFields[["biogeo_3"]] <- c('id_biogeo_3','id_location','replicate')
mandatoryFields[["biogeo_1"]] <- c('id_biogeo_1','id_location','replicate')
mandatoryFields[["microbial_1"]] <- c('id_microbial_1','id_patch','replicate')
mandatoryFields[["microbial_2"]] <- c('id_microbial_2','id_patch','replicate')
mandatoryFields[["microbial_3"]] <- c('id_microbial_3','id_patch','replicate')

mandatoryColumns <- list()
mandatoryColumns[["glacier"]] <- c(1)
mandatoryColumns[["location"]] <- c(1,2,3)
mandatoryColumns[["patch"]] <- c(1,2,3)
mandatoryColumns[["biogeo_3"]] <- c(1,2,3)
mandatoryColumns[["biogeo_1"]] <- c(1,2,3)
mandatoryColumns[["microbial_1"]] <- c(1,2,3)
mandatoryColumns[["microbial_2"]] <- c(1,2,3)
mandatoryColumns[["microbial_3"]] <- c(1,2,3)

fullnameFields <- list()
fullnameFields[["gl_global"]] <- c('ID','Glacier name')
fullnameFields[["gl_point"]] <- c('ID','Snout latitude\n[DD]','Snout longitude\n[DD]','Snout elevation\n[m]','Up latitude\n[DD]','Up longitude\n[DD]','Up elevation\n[m]'
                                  ,'Down latitude\n[DD]','Down longitude\n[DD]','Down elevation\n[m]','Max elevation\n[m]','Mean elevation\n[m]')
fullnameFields[["gl_line"]] <- c('ID','Up to snout\ndist\n[m]','Up to snout\nelevation diff\n[m]','Up to down\ndist\n[m]','Up to down\nelevation diff\n[m]','Aspect of ablation zone\n[\u00B0]')
fullnameFields[["gl_area"]] <- c('ID','Glacier surface area\n[km2]','Glacier coverage Up\n[%]','Catchment area Up\n[km2]')
fullnameFields[["gl_invent"]] <- c('ID','RGI v.6','GLIMS ID','WGMS ID','Mass balance\ndata availability','Mass balance\nobservations','Frontal variation\ndata availability','Frontal variation\nobservations','Special event')
fullnameFields[["gl_source"]] <- c('ID','Glacier outline\ndata source','Glacier outline\ntimestamp','Snout position\ndata source','Snout position\ntime stamp','DEM data\nsource','DEM data\ntime stamp','Mass balance\ndata source','Frontal variation\ndata source','Special event\ndata source')
fullnameFields[["gl_other"]] <- c('ID','','','','')
fullnameFields[["location"]] <- c('ID','Glacier','Type','Date\n[DD.MM.YYYY]','Time\n[HH:MM]',
                                  'Water Temp\n[\u00B0C]','pH\n[pH]','Potential\n[mV]','Dissolved Oxygen\n[mg l-1]','Dissolved Oxygen\n[saturation]','Water Co2\n[mATM]',
                                  'Conductivity\n[uS cm -1]','Turbidity\n[NTU]','Rock DNA\npresent\n[boolean]')
fullnameFields[["patch"]] <- c("ID","Location",'name')
# fullnameFields[["coble"]] <- c("ID","Location","Replicate","Coble peak B\n[RU]","Coble peak T\n[RU]","Coble peak A\n[RU]","Coble peak M\n[RU]","Coble peak C\n[RU]","Coble peak R\n[RU]")
fullnameFields[["indices"]] <- c("ID","Location","Replicate","Absorbance\nat 254nm\n[m-1]","Absorbance\nat 300nm\n[m-1]",
                                    "Specific UV\nAbsorbance\n[mg C l-1 m-1]","E2:E3\n(Abs 250:365nm)\n[proportion]","E4:E6\n(Abs 465:665nm)\n[proportion]",
                                    "Slope Absorbance\nrange 275-295nm\n[nm-1]","Slope Absorbance\nrange 350-400nm\n[nm-1]",
                                    "Slope Absorbance\nrange 300-700nm\n[nm-1]","Slope ratio\n(275-295:350-400nm)\n[proportion]",
                                    "Biological index\n[proportion]","Fluorescence index\n[proportion]","Humification index\n[proportion]",
                                    "Coble peak B\n[RU]","Coble peak T\n[RU]","Coble peak A\n[RU]","Coble peak M\n[RU]","Coble peak C\n[RU]","Coble peak R\n[RU]")
fullnameFields[["ion"]] <- c("ID","Location","Replicate","Ion 1\nSodium\n[ug l-1]","Ion 2\nPotassium\n[ug l-1]","Ion 3\nMagnesium\n[ug l-1]",
                             "Ion 4\nCalcium\n[ug l-1]","Ion 5\nChloride\n[ug l-1]","Ion 6\nSulfate\n[ug l-1]")
fullnameFields[["nutrient"]] <- c("ID","Location","Replicate","Nutrients 1\nTotal Nitrogen\n[ug l-1]","Nutrients 2\nTotal Phosphorus\n[ug l-1]",
                                  "Nutrients 3\nSoluble reactive\nphosphorus\n[ug l-1]","Nutrients 4\nAmmonium\n[ug l-1]",
                                  "Nutrients 5\nNitrate\n[ug l-1]","Nutrients 6\nNitrite\n[ug l-1]")
fullnameFields[["doc"]] <- c("ID","Location","Replicate","Dissolved organic carbon")

fullnameFields[["chla"]] <- c("ID","Patch","Replicate","CHLA\n[ug g-1]")
fullnameFields[["enzyme"]] <- c("ID","Patch","Replicate","AG\n[nmol g-1 h-1]","AP\n[nmol g-1 h-1]","BG\n[nmol g-1 h-1]","LAP\n[nmol g-1 h-1]","NAG\n[nmol g-1 h-1]")
fullnameFields[["eps"]] <- c("ID","Patch","Replicate","EPS\n[ugC g-1]")
fullnameFields[["bp"]] <- c("ID","Patch","Replicate","Bacterial\nProduction\n[ngC g-1 h-1]")
fullnameFields[["ba"]] <- c("ID","Patch","Replicate","Water bacterial\nabundance\n[cells ml-1]","Sediment bacterial\nabundance\n[cells g-1]")
fullnameFields[["respiration"]] <- c("ID","Patch","Replicate","Respiration\n[mg 02 g-1 h-1]")

# List of options for each table to display in the upload section as "excel sheet"
tableOptions <- list()
tableOptions[["enzyme"]] <- c("primary"="id_enzyme", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["patch"]] <- c("primary"="id_patch", "FK"="id_location", "name"="name", "replicates"= list(c()))
tableOptions[["location"]] <- c("primary"="id_location", "FK"="id_glacier", "name"="type", "replicates"= list(c()))
tableOptions[["glacier"]] <- c("primary"="id_glacier", "FK"="", "name"="name", "replicates"= list(c()))
tableOptions[["biogeo_3"]] <- c("primary"="id_biogeo_3", "FK"="id_location", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["biogeo_1"]] <- c("primary"="id_biogeo_1", "FK"="id_location", "name"="replicate", "replicates"= list(c('A')))
tableOptions[["microbial_1"]] <- c("primary"="id_microbial_1", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A')))
tableOptions[["microbial_2"]] <- c("primary"="id_microbial_2", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B')))
tableOptions[["microbial_3"]] <- c("primary"="id_microbial_3", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))


# List of table that are only on UP location
isOnlyUP <- list()
isOnlyUP[["biogeo_3"]] <- TRUE
isOnlyUP[["biogeo_1"]] <- FALSE

nbOfEntryByGlacier <- c("location"=2,"glacier"=1, "enzyme"=18,"chla"=18,"respiration"=12,"bp"=12,"ba"=6,"eps"=6,"nutrient"=2,"ion"=2,"doc"=3,"dom"=3)
nbOfEntryByTable <- c("glacier"=1,"location"=2,"patch"=6,"microbial_1"=6,"microbial_2"=12,"microbial_3"=18,"biogeo_3"=6,"biogeo_1"=2)
levels <- c("glacier"=1,"location"=2,"patch"=6,"microbial_1"=6,"microbial_2"=6,"microbial_3"=6,"biogeo_3"=2,"biogeo_1"=2)
# The list of all the data types that can be uploaded from the upload section
uploadDataTypes <- list("Field metrics"="location","Patch"="patch",
                        `Glaciological metrics` = c("Identification metrics"="gl_global","Point metrics"="gl_point","Line metrics"="gl_line","Area metrics"="gl_area","Inventory data"="gl_invent","Data sources"="gl_source","Other metrics"="gl_other"),
                        `Microbial metrics` = c("Chlorophyll-A"="chla","Extracelullar polymeric substances"="eps","Enzyme"="enzyme","Bacterial production"="bp","Bacterial abundance"="ba","Respiration"="respiration"),
                        `Biogeochemical metrics` = c("Dissolved organic matter"="biogeo_3",
                                                     "Dissolved organic matter indices"="indices",
                                                     "Dissolved organic carbon"="doc",
                                                     "Ions" = "ion",
                                                     "Nutrients"="nutrient"))

downloadFileTypes <- c("EEM"="eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10")

# The sublist of all the "DOM type" that can be uploaded
uploadDOMTypes <- c("EEM" = "eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10")


downloadDataTypes <- list(`Field metrics` = c("Field metrics - All"="location-all","Date"="date",
                                              "Time"="time","Water temperature"="water_temp","Ph"="ph",
                                              "Potential"="potential","Dissolved oxygen"="do","Dissolved oxygen saturation"="do_sat","Water CO2"="w_co2",
                                              "Conductivity"="conductivity","Turbidity"="turb","Rock DNA present"="rdna"),
                        `Glaciological metrics` = c("Glaciological metrics - All" ="glacio-all","Identification metrics"="gl_global","Point metrics"="gl_point","Line metrics"="gl_line","Area metrics"="gl_area","Inventory data"="gl_invent","Data sources"="gl_source"
                                                    # ,"Other metrics"="gl_other"
                                                    ),
                        `Microbial metrics` = c("Microbial metrics - All" ="micro-all","Chlorophyll-A"="chla","Extracelullar polymeric substances"="eps","Enzymes"="enzyme","Bacterial production"="bp","Bacterial abundance"="ba","Respiration"="respiration"),
                        `Biogeochemical metrics` = c("Biogeochemical metrics - All" ="biogeo-all",
                                                     "Dissolved organic matter indices"="indices",
                                                     "Dissolved organic carbon"="doc",
                                                     "Ions" = "ion",
                                                     "Nutrients"="nutrient"))
typeList <- function(){
  l <- downloadDataTypes
  i <- 1
  for (metrics in l) {
    l[[i]] <- names(metrics)
    # print(metrics)
    i <- i+1
  }
  l    
}
colConfig <- list()
colConfig[["location"]] <- list(list(col=4,type = "date",dateFormat = "DD.MM.YYYY"),
                                list(col= 5, validator = "function (value, callback) {
              if (/(^\\d{1,2}:\\d{2}($|:\\d{2}$))|(^(?![\\s\\S]))|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= c(6,7,8,9,10,11,12,13), validator = "function (value, callback) {
              if (/^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$|(^(?![\\s\\S]))|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= 14, type = "checkbox",default = FALSE, renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              td.style.textAlign = 'center';
              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              return td;}")
          )
colConfig[["gl_global"]] <- list()
# 'id_glacier','lat_sn','lon_sn','ele_sn','lat_up','lon_up','ele_up','lat_dn','lon_dn','ele_dn','max_el','mean_el'
colConfig[["gl_point"]] <- list(list(col= 2:11, validator = "function (value, callback) {
              if (/^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"))
colConfig[["gl_line"]] <- list(list(col= 2:6, validator = "function (value, callback) {
              if (/^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"))
colConfig[["gl_area"]] <- list(list(col= 2:4, validator = "function (value, callback) {
              if (/^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"))
colConfig[["gl_invent"]] <- list()
colConfig[["gl_source"]] <- list()
colConfig[["indices"]] <- list()
colConfig[["ion"]] <- list()
colConfig[["nutrient"]] <- list()
colConfig[["enzyme"]] <- list()
colConfig[["eps"]] <- list()
colConfig[["bp"]] <- list()
colConfig[["ba"]] <- list()
colConfig[["chla"]] <- list(list(col=4,type="numeric", format="0.00"))
colConfig[["respiration"]] <- list()
colConfig[["doc"]] <- list()


summaryFullNameFields <- c("Abbreviation","Range","Field metrics","Glaciological metrics","DOC","DOM","Ions","Nutrients","EPS","Ba","Bp","Respiration","Chlorophyll-A","Enzymes")

