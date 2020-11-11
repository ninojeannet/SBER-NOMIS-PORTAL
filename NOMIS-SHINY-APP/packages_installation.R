install.packages(
  pkgs = c("shiny", 
           "shinyjs", 
           "shinyWidgets", 
           "shinybusy", 
           "shinycssloaders", 
           "sass", 
           "jsonlite", 
           "readr", 
           "stringr", 
           "ggplot2", 
           "Cairo", 
           "data.table", 
           "lubridate", 
           "forcats", 
           "tidyr", 
           "magrittr", 
           "dplyr",
           "DBI",
           "RMySQL",
           "pool",
           "dbplyr",
           "sodium",
           "DT",
           "rhandsontable",
           "lambda.r",
           "rlist",
           "formattable",
           "DataCombine",
           "staRdom",
           "kableExtra"
           ,
           "xlsx"
  ),
  repos = 'https://cran.rstudio.com/'
)

remotes::install_github("rstudio/shinyvalidate")