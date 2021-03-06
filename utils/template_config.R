MIN <- 1
MAX <- 250

tableList <- c("glacier","glacier_ud","location","patch","microbial_1","microbial_2","microbial_3","biogeo_3u","biogeo_1u","biogeo_1")

# List of all field name contained in each table
templateFieldNames <- list()
templateFieldNames[["gl_global"]] <- c('id_glacier','gl_name')
templateFieldNames[["gl_point"]] <- c('id_glacier_ud','id_glacier','site','lat_sn','lon_sn','ele_sn','lat_sp','lon_sp','ele_sp','max_el','mean_el')
templateFieldNames[["gl_line"]] <- c('id_glacier_ud','id_glacier','site','sn_sp_dist','sn_sp_ele','abl_asp')
templateFieldNames[["gl_area"]] <- c('id_glacier_ud','id_glacier','site','gl_sa','gl_cov','gl_a')
templateFieldNames[["gl_invent"]] <- c('id_glacier','rgi_v6','glims_id','wgms_id','m_balance','mb_obs','fv_variation','fv_obs','sp_event')
templateFieldNames[["gl_source"]] <- c('id_glacier','data_s','data_ts','sn_data','sn_data_ts','dem_data','dem_data_ts','mb_data_s','fv_data_s','sp_event_s')
templateFieldNames[["gl_other"]] <- c('id_glacier','','','','')
templateFieldNames[["location"]] <- c('id_location','id_glacier','type','date','time','water_temp','ph','do','do_sat','w_co2','conductivity','turb','rdna')
templateFieldNames[["patch"]] <- c("id_patch","id_location","name")
templateFieldNames[["indices"]] <- c("id_biogeo_3u","id_location","replicate","abs254","abs300",
                                        "suva","e2e3","e4e6","s275295","s350400","s300700","sr","bix","fi","hix",
                                     "coble_t","coble_a","coble_m","coble_c")
templateFieldNames[["ion"]] <- c("id_biogeo_1","id_location","replicate","i1_na","i2_k","i3_mg","i4_ca","i5_cl","i6_so4")
templateFieldNames[["mineral"]] <- c("id_biogeo_1u","id_location","replicate","tss_1","tss_2","tss_3","tss_4","tss_5","tss_5a","tss_5b","tss_5c","tss_5d")
templateFieldNames[["nutrient"]] <- c("id_biogeo_1","id_location","replicate","n1_tn","n2_tp","n3_srp","n4_nh4","n5_no3","n6_no2")
templateFieldNames[["isotope"]] <- c("id_biogeo_1","id_location","replicate","hydro_isotope","oxy_isotope")
templateFieldNames[["enzyme"]] <- c("id_microbial_3","id_patch","replicate","ag","ap","bg","lap","nag")
templateFieldNames[["chla"]] <- c("id_microbial_3","id_patch","replicate","chla")
templateFieldNames[["eps"]] <- c("id_microbial_1","id_patch","replicate","eps")
templateFieldNames[["bp"]] <- c("id_microbial_2","id_patch","replicate","bp")
templateFieldNames[["ba"]] <- c("id_microbial_1","id_patch","replicate","wba","sba")
templateFieldNames[["respiration"]] <- c("id_microbial_2","id_patch","replicate","respiration")
templateFieldNames[["doc"]] <- c("id_biogeo_3u","id_location","replicate","doc")

subCategoriesOfTable <- list()
subCategoriesOfTable[["glacier"]] <- c('gl_global','gl_invent','gl_source')#,'gl_other')
subCategoriesOfTable[["glacier_ud"]] <- c('gl_point','gl_line','gl_area')
subCategoriesOfTable[["biogeo_3u"]] <- c("dom","indices","doc", "eem","abs1","abs10")
subCategoriesOfTable[["biogeo_1"]] <- c("ion","nutrient","isotope")
subCategoriesOfTable[["biogeo_1u"]] <- c("mineral")
subCategoriesOfTable[["microbial_1"]] <- c("ba","eps")
subCategoriesOfTable[["microbial_2"]] <- c("respiration","bp")
subCategoriesOfTable[["microbial_3"]] <- c("enzyme","chla")

specificFields <- list()
specificFields[["dom"]] <- c("filename_eem","filename_abs1","filename_abs10")

# List of mandatory field for each table
mandatoryFields <- list()
mandatoryFields[["glacier"]] <- c('id_glacier')
mandatoryFields[["glacier_ud"]] <- c('id_glacier_ud','id_glacier','site')
mandatoryFields[["location"]] <- c('id_location','id_glacier','type')
mandatoryFields[["patch"]] <- c('id_patch','id_location','name')
mandatoryFields[["biogeo_3u"]] <- c('id_biogeo_3u','id_location','replicate')
mandatoryFields[["biogeo_1"]] <- c('id_biogeo_1','id_location','replicate')
mandatoryFields[["biogeo_1u"]] <- c("id_biogeo_1u","id_location","replicate")
mandatoryFields[["microbial_1"]] <- c('id_microbial_1','id_patch','replicate')
mandatoryFields[["microbial_2"]] <- c('id_microbial_2','id_patch','replicate')
mandatoryFields[["microbial_3"]] <- c('id_microbial_3','id_patch','replicate')

mandatoryColumns <- list()
mandatoryColumns[["glacier"]] <- c(1)
mandatoryColumns[["glacier_ud"]] <- c(1,2,3)
mandatoryColumns[["location"]] <- c(1,2,3)
mandatoryColumns[["patch"]] <- c(1,2,3)
mandatoryColumns[["biogeo_3u"]] <- c(1,2,3)
mandatoryColumns[["biogeo_1"]] <- c(1,2,3)
mandatoryColumns[["biogeo_1u"]] <- c(1,2,3)
mandatoryColumns[["microbial_1"]] <- c(1,2,3)
mandatoryColumns[["microbial_2"]] <- c(1,2,3)
mandatoryColumns[["microbial_3"]] <- c(1,2,3)

fullnameFields <- list()
fullnameFields[["gl_global"]] <- c('ID','Glacier name')
fullnameFields[["gl_point"]] <- c('ID','Glacier','Site','Snout latitude\n[DD]','Snout longitude\n[DD]','Snout elevation\n[m]','Sampling point\nlatitude\n[DD]','Sampling point\nlongitude\n[DD]','Sampling point\nelevation\n[m]'
                                  ,'Max elevation\n[m]','Mean elevation\n[m]')
fullnameFields[["gl_line"]] <- c('ID','Glacier','Site','Snout to\nsampling point\ndist\n[m]','Snout to\nsampling point\nelevation diff\n[m]','Aspect of ablation zone\n[\u00B0]')
fullnameFields[["gl_area"]] <- c('ID','Glacier','Site','Glacier surface area\n[km2]','Glacier coverage\n[%]','Catchment area\n[km2]')
fullnameFields[["gl_invent"]] <- c('ID','RGI v.6','GLIMS ID','WGMS ID','Mass balance\ndata availability','Mass balance\nobservations','Frontal variation\ndata availability','Frontal variation\nobservations','Special event')
fullnameFields[["gl_source"]] <- c('ID','Glacier outline\ndata source','Glacier outline\ntimestamp','Snout position\ndata source','Snout position\ntime stamp','DEM data\nsource','DEM data\ntime stamp','Mass balance\ndata source','Frontal variation\ndata source','Special event\ndata source')
fullnameFields[["gl_other"]] <- c('ID','','','','')
fullnameFields[["location"]] <- c('ID','Glacier','Type','Date\n[DD.MM.YYYY]','Time\n[HH:MM]',
                                  'Water Temp\n[\u00B0C]','pH\n[pH]','Dissolved Oxygen\n[mg l-1]','Dissolved Oxygen\n[saturation]','Water Co2\n[mATM]',
                                  'Conductivity\n[uS cm -1]','Turbidity\n[NTU]','Rock DNA\npresent\n[boolean]')
fullnameFields[["patch"]] <- c("ID","Location",'name')
fullnameFields[["indices"]] <- c("ID","Location","Replicate","Absorbance\nat 254nm\n[m-1]","Absorbance\nat 300nm\n[m-1]",
                                    "Specific UV\nAbsorbance\n[mg C l-1 m-1]","E2:E3\n(Abs 250:365nm)\n[proportion]","E4:E6\n(Abs 465:665nm)\n[proportion]",
                                    "Slope Absorbance\nrange 275-295nm\n[nm-1]","Slope Absorbance\nrange 350-400nm\n[nm-1]",
                                    "Slope Absorbance\nrange 300-700nm\n[nm-1]","Slope ratio\n(275-295:350-400nm)\n[proportion]",
                                    "Biological index\n[proportion]","Fluorescence index\n[proportion]","Humification index\n[proportion]",
                                    "Coble peak T\n[RU]","Coble peak A\n[RU]","Coble peak M\n[RU]","Coble peak C\n[RU]")
fullnameFields[["ion"]] <- c("ID","Location","Replicate","Ion 1\nSodium\n[ug l-1]","Ion 2\nPotassium\n[ug l-1]","Ion 3\nMagnesium\n[ug l-1]",
                             "Ion 4\nCalcium\n[ug l-1]","Ion 5\nChloride\n[ug l-1]","Ion 6\nSulfate\n[ug l-1]")
fullnameFields[["mineral"]] <- c("ID","Location","Replicate","TSS Mineral\nQuartz\n[%]","TSS Mineral\nFeldspar\n[%]","TSS Mineral\nMicas\n[%]",
                                 "TSS Mineral\nCalcite\n[%]","TSS Mineral\nClay\n[%]","TSS Mineral\nChlorite\n[%]","TSS Mineral\nIllite\n[%]",
                                 "TSS Mineral\nKaolinite\n[%]","TSS Mineral\nSmectite\n[%]")
fullnameFields[["nutrient"]] <- c("ID","Location","Replicate","Nutrients 1\nTotal Nitrogen\n[ug l-1]","Nutrients 2\nTotal Phosphorus\n[ug l-1]",
                                  "Nutrients 3\nSoluble reactive\nphosphorus\n[ug l-1]","Nutrients 4\nAmmonium\n[ug l-1]",
                                  "Nutrients 5\nNitrate\n[ug l-1]","Nutrients 6\nNitrite\n[ug l-1]")
fullnameFields[["isotope"]] <- c("ID","Location","Replicate","Hydrogen stable\nisotope\n[per_mil]","Oxygen stable\nisotope\n[per_mil]")
fullnameFields[["doc"]] <- c("ID","Location","Replicate","Dissolved organic carbon\n[ug l-1]")
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
tableOptions[["glacier"]] <- c("primary"="id_glacier", "FK"="id_glacier", "name"="name", "replicates"= list(c()))
tableOptions[["glacier_ud"]] <- c("primary"="id_glacier_ud", "FK"="id_glacier", "name"="site", "replicates"= list(c()))
tableOptions[["biogeo_3u"]] <- c("primary"="id_biogeo_3u", "FK"="id_location", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["biogeo_1"]] <- c("primary"="id_biogeo_1", "FK"="id_location", "name"="replicate", "replicates"= list(c('A')))
tableOptions[["biogeo_1u"]] <- c("primary"="id_biogeo_1u", "FK"="id_location", "name"="replicate", "replicates"= list(c('A')))
tableOptions[["microbial_1"]] <- c("primary"="id_microbial_1", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A')))
tableOptions[["microbial_2"]] <- c("primary"="id_microbial_2", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B')))
tableOptions[["microbial_3"]] <- c("primary"="id_microbial_3", "FK"="id_patch", "name"="replicate", "replicates"= list(c('A','B','C')))
tableOptions[["expedition"]] <- c("primary"="id_expedition", "name"="name")

# List of table that are only on UP location
isOnlyUP <- list()
isOnlyUP[["biogeo_3u"]] <- TRUE
isOnlyUP[["biogeo_1"]] <- FALSE
isOnlyUP[["biogeo_1u"]] <- TRUE

nbOfEntryByGlacier <- c("location"=2,"glacier"=1,"glacier_ud"=2, "enzyme"=18,"chla"=18,"respiration"=12,"bp"=12,"ba"=6,"eps"=6,"nutrient"=2,"isotope"=2,"ion"=2,"doc"=3,"dom"=3,"indices"=3,"mineral"=1)
nbOfEntryByTable <- c("glacier"=1,"glacier_ud"=2,"location"=2,"patch"=6,"microbial_1"=6,"microbial_2"=12,"microbial_3"=18,"biogeo_3u"=6,"biogeo_1"=2,"biogeo_1u"=2)
levels <- c("glacier"=1,"glacier_ud"=2,"location"=2,"patch"=6,"microbial_1"=6,"microbial_2"=6,"microbial_3"=6,"biogeo_3u"=2,"biogeo_1"=2,"biogeo_1u"=2)
# The list of all the data types that can be uploaded from the upload section
uploadDataTypes <- list("Field metrics"="location","Patch"="patch",
                        `Glaciological metrics` = c("Identification metrics"="gl_global","Point metrics"="gl_point","Line metrics"="gl_line","Area metrics"="gl_area","Inventory data"="gl_invent","Data sources"="gl_source" ),#,"Other metrics"="gl_other"),
                        `Microbial metrics` = c("DNA"="expedition","Chlorophyll-A"="chla","Extracelullar polymeric substances"="eps","Enzyme"="enzyme","Bacterial production"="bp","Bacterial abundance"="ba","Respiration"="respiration"),
                        `Biogeochemical metrics` = c("Dissolved organic matter"="biogeo_3u",
                                                     "Dissolved organic matter indices"="indices",
                                                     "FT-ICR MS"="ft_icr_ms",
                                                     "Dissolved organic carbon"="doc",
                                                     "Minerals" = "mineral",
                                                     "Ions" = "ion",
                                                     "Nutrients"="nutrient",
                                                     "Isotopes" = "isotope",
                                                     "Trace elements"="trace_el"))

plotDataTypes <- list(`Glaciological metrics` = c("Snout latitude"="lat_sn","Snout longitude"="lon_sn","Snout elevation"="ele_sn",
                                                    "Sampling point latitude"="lat_sp","Sampling point longitude"="lon_sp","Sampling point elevation"="ele_sp",
                                                    "Glacier surface area"="gl_sa", "Aspect of ablation zone"="abl_asp","Glacier maximal elevation"="max_el",
                                                    "Glacier mean elevation"="mean_el","Catchment area "="gl_a","Snout to sampling point distance"="sn_sp_dist",
                                                    "Snout to sampling point elevation difference"="sn_sp_ele","Glacier coverage"="gl_cov"),
                        `Field metrics` = c("Water temperature"="water_temp","pH"="ph",
                                            "Dissolved oxygen"="do","Dissolved oxygen saturation"="do_sat",
                                            "Water CO2"="w_co2","Conductivity"="conductivity","Turbidity"="turb",
                                            "Rock DNA present"="rdna"),
                        `Microbial metrics` = c("Chlorophyll-A"="chla",
                                                "Enzyme 1: α-1,4-glucosidase"="ag","Enzyme 2: β-1,4-glucosidase"="bg",
                                                "Enzyme 3: leucine aminopeptidase"="lap","Enzyme 4: β-1,4-n-acetylglucosaminidase"="nag",
                                                "Enzyme 5: Acid (alkaline) phosphatase"="ap","Extracelullar polymeric substances"="eps",
                                                "Water bacterial abundance"="wba","Sediment bacterial abundance"="sba",
                                                "Bacterial production"="bp","Respiration"="respiration"),
                        `Biogeochemical metrics` = c("Absorbance at 254 nm"="abs254","Absorbance at 300 nm"="abs300","Specific UV Absorbance"="suva",
                                                     "E2:E3 (Abs 250:365 nm)"="e2e3","E4:E6 (Abs 465:665 nm)"="e4e6","Slope Absorbance range 275-295 nm"="s275295",
                                                     "Slope Absorbance range 350-400 nm"="s350400","Slope Absorbance range 300-700 nm"="s300700",
                                                     "Slope ratio (275-295:350-400nm)"="sr","Biological index"="bix","Fluorescence index"="fi",
                                                     "Humification index"="hix","Coble peak t"="coble_t",
                                                     "Coble peak a"="coble_a","Coble peak m"="coble_m","Coble peak c"="coble_c",
                                                     "Dissolved organic carbon"="doc","Total Suspended Solids Mineral Quartz" = "tss_1",
                                                     "Total Suspended Solids Mineral Feldspar" = "tss_2","Total Suspended Solids Mineral Micas" = "tss_3",
                                                     "Total Suspended Solids Mineral Calcite" = "tss_4","Total Suspended Solids Mineral Clay" = "tss_5",
                                                     "Total Suspended Solids Mineral Chlorite" = "tss_5a","Total Suspended Solids Mineral Illite" = "tss_5b",
                                                     "Total Suspended Solids Mineral Kaolinite" = "tss_5c","Total Suspended Solids Mineral Smectite" = "TSS_5d",
                                                     "Ion 1:Sodium" = "i1_na", "Ion 2:Potassium" = "i2_k","Ion 3:Magnesium" = "i3_mg",
                                                     "Ion 4:Calcium" = "i4_ca","Ion 5:Chloride" = "i5_cl","Ion 6:Sulfate" = "i6_so4",
                                                     "Nutrients 1:Total Nitrogen"="n1_tn","Nutrients 2:Total phosphorus"="n2_tp","Nutrients 3:Soluble reactive phosphorus"="n3_srp",
                                                     "Nutrients 4:Ammonium"="n4_nh4","Nutrients 5:Nitrate"="n5_no3","Nutrients 6:Nitrite"="n6_no2",
                                                     "Hydrogen stable isotope" = "hydro_isotope","Oxygen stable isotope" = "oxy_isotope"))

downloadFileTypes <- c("EEM"="eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10","16S table"="16s","16S taxonomy"="16s_tax","16S phylogenetic trees"="16s_phyl","18S table"="18s","18S taxonomy"="18s_tax","18s phylogenetic trees"="18s_phyl","Trace elements"="trace_el","FT-ICR MS"="ft_icr_ms")

# The sublist of all the "DOM type" that can be uploaded
uploadDOMTypes <- c("EEM" = "eem","Absorbance 1cm"="abs1","Absorbance 10cm"="abs10")
uploadDNAFilesType <- c("16S table"="16s","16S taxonomy"="16s_tax","16S phylogenetic tree"="16s_phyl","18S table"="18s","18S taxonomy"="18s_tax","18S phylogenetic tree"="18s_phyl")

fileExtension <- list()
fileExtension[["16s"]] <- c("txt")
fileExtension[["18s"]] <- c("txt")
fileExtension[["16s_tax"]] <- c("txt")
fileExtension[["18s_tax"]] <- c("txt")
fileExtension[["16s_phyl"]] <- c("nwk","tre","tree","treefile")
fileExtension[["18s_phyl"]] <- c("nwk","tre","tree","treefile")
downloadDataTypes <- list(`Field metrics` = c("Field metrics - All"="location-all","Date"="date",
                                              "Time"="time","Water temperature"="water_temp","pH"="ph"
                                              ,"Dissolved oxygen"="do","Dissolved oxygen saturation"="do_sat","Water CO2"="w_co2",
                                              "Conductivity"="conductivity","Turbidity"="turb","Rock DNA present"="rdna"),
                        `Glaciological metrics` = c("Glaciological metrics - All" ="glacio-all","Identification metrics"="gl_global","Point metrics"="gl_point","Line metrics"="gl_line","Area metrics"="gl_area","Inventory data"="gl_invent","Data sources"="gl_source"
                                                    # ,"Other metrics"="gl_other"
                                                    ),
                        `Microbial metrics` = c("Microbial metrics - All" ="micro-all","Chlorophyll-A"="chla","Extracelullar polymeric substances"="eps","Enzymes"="enzyme","Bacterial production"="bp","Bacterial abundance"="ba","Respiration"="respiration"),
                        `Biogeochemical metrics` = c("Biogeochemical metrics - All" ="biogeo-all",
                                                     "Dissolved organic matter indices"="indices",
                                                     "Dissolved organic carbon"="doc",
                                                     "Minerals" = "mineral",
                                                     "Ions" = "ion",
                                                     "Nutrients"="nutrient",
                                                     "Isotopes" = "isotope"))
typeList <- function(){
  l <- downloadDataTypes
  i <- 1
  for (metrics in l) {
    l[[i]] <- names(metrics)
    i <- i+1
  }
  l   
}

# Numeric validator
numValidator <- "function (value, callback) {
  if (/^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$|^(?![\\s\\S])|[\\s]+|^NA$|^na$/.test(value)) {return callback(true);} else {return callback(false);}}"


# List of column configuration for each groups
colConfig <- list()
colConfig[["location"]] <- list(list(col=4,type = "date",dateFormat = "DD.MM.YYYY",validator = "function (value, callback) {if (/^(?:(?:31(\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?![\\s\\S])|[\\s]+|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= 5, validator = "function (value, callback) {
              if (/(^\\d{1,2}:\\d{2}($|:\\d{2}$))|^(?![\\s\\S])|[\\s]+|^NA$|^na$/.test(value)) {callback(true)} else {callback(false)}}"),
                                list(col= c(6,7,8,9,10,11,12), validator = numValidator),
                                list(col= 13, type = "checkbox",default = FALSE, renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              td.style.textAlign = 'center';
              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              return td;}")
          )
colConfig[["gl_global"]] <- list()
colConfig[["gl_point"]] <- list(list(col= 4:11, validator = numValidator))
colConfig[["gl_line"]] <- list(list(col= 4:6, validator = numValidator))
colConfig[["gl_area"]] <- list(list(col= 4:6, validator = numValidator))
colConfig[["gl_invent"]] <- list()
colConfig[["gl_source"]] <- list()
colConfig[["indices"]] <- list(list(col=4:13, validator = numValidator))
colConfig[["ion"]] <- list(list(col= 4:9, validator = numValidator))
colConfig[["mineral"]] <- list(list(col= 4:12, validator = numValidator))
colConfig[["nutrient"]] <- list(list(col= 4:9, validator = numValidator))
colConfig[["isotope"]] <- list(list(col= 4:5, validator = numValidator))
colConfig[["enzyme"]] <- list(list(col= 4:8, validator = numValidator))
colConfig[["eps"]] <- list(list(col= 4, validator = numValidator))
colConfig[["bp"]] <- list(list(col= 4, validator = numValidator))
colConfig[["ba"]] <- list(list(col= 4:5, validator = numValidator))
colConfig[["chla"]] <- list(list(col=4,validator=numValidator))
colConfig[["respiration"]] <- list(list(col= 4, validator = numValidator))
colConfig[["doc"]] <- list(list(col= 4, validator = numValidator))

numValidatorCols <- list("gl_global"=c(),"gl_point"=c(4:11),"gl_line"=c(4:6),"gl_area"=c(4:6),"gl_invent"=c(),
                     "gl_source"=c(),"indices"=c(4:13),"ion"=c(4:9),"mineral"=c(4:12),"nutrient"=c(4:9),
                     "isotope"=c(4:5),"enzyme"=c(4:8),"eps"=c(4),"bp"=c(4),"ba"=c(4:5),"chla"=c(4),
                     "respiration"=c(4),"doc"=c(4),"location"=c(6:12))
# Fields to display in the progess table
summaryFullNameFields <- c("Abbreviation","Range","Field metrics","Glaciological metrics","DOC","DOM file","DOM indices","FT-ICR MS","Minerals","Ions","Nutrients","Isotopes","Trace elements","16S table","18S table","Chlorophyll-A","EPS","Enzymes","Bp","Ba","Respiration")