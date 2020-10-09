#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(shinycssloaders)
library(sass)
# library(mailR)
library(jsonlite)
library(readr)
library(stringr)
library(ggplot2)
library(Cairo)
library(data.table)
library(lubridate)
library(forcats)
library(tidyr)
library(magrittr)
library(dplyr)
library(DBI)
library(pool)
library(DT)
library(rhandsontable)
library(lambda.r)
library(rlist)
#install.packages("rlist")

## Compile CSS from Sass ##########################################################
sass(
    sass_file('assets/sass/main.scss'), 
    output = 'www/main.css',
    options = sass_options(output_style = 'compressed')
)

## Load helper functions ##########################################################
source('./utils/helper_functions.R')

## Compile and minify JavaScript ##################################################
js_parser()

## Load Shiny extensions functions ################################################
source('./utils/shiny_extensions.R')
source('app_config.R')

## Load tabs modules ##############################################################
source('./modules/management_tab/management_tab.R')
source('./modules/upload_tab/upload_tab.R')

options(shiny.maxRequestSize=100*1024^2)

pool <- dbPool(
    drv = RMySQL::MySQL(),
    dbname = DBName,
    host = hostname,
    username = username,
    password = password)

# conn <- poolCheckout(pool)
# # queryStatus <- dbWithTransaction(conn,{
# #     dataframe <-dbGetQuery(conn,query)
# # })
# print(dbQuoteLiteral(conn,1))
# poolReturn(conn)

# Define UI for application that draws a histogram
ui <- tagList(
    # Load shinyjs
    useShinyjs(),
    # Add stylesheet link and script tags to head
    tags$head(
        # Add link to main.css stylesheet
        tags$link(href = 'main.css', rel = 'stylesheet', type = 'text/css'),
        # Add link for js script
        tags$script(src = 'nomisportal.js')
    ),
    # Add a class to the body element to keep the footer at the bottom of the page
    tags$body(class = 'footer-to-bottom-container'),
    # Create the navbarPage using custom function to add a content-wrapper (defined in './utils/shiny_extensions.R')
    navbarPageWithWrapper(
        # Pass in the output of shiny navbarPage()
        navbarPage(
            # Load the custom logo for the navbar title
            htmlTemplate('./html_components/logo.html'),
            
            # Set a window browser window title
            windowTitle = 'NOMIS DATA PORTAL',
            # Create the home tab
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('home'),tags$span('Home', class = 'navbar-menu-name'))
            ),
            # Create the visualisation tab
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('chart-bar'),tags$span('Visualisation', class = 'navbar-menu-name')),
                # Load the visualisationTab module UI elements
                #visualisationTabUI('1', grabSampleDf, hfDf, sites, grabSampleParameters, hfParameters)
            ),
            # Create the data management tab
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('database'),tags$span('Data Management', class = 'navbar-menu-name')),
                # Load the managementTab module UI elements
                managementTabUI('management')
            ),
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('upload'),tags$span('Upload', class = 'navbar-menu-name')),
                # Load the uploadTab module UI elements
                uploadTabUI('upload')
            ),
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
                # Load the visualisationTab module UI elements
                #visualisationTabUI('1', grabSampleDf, hfDf, sites, grabSampleParameters, hfParameters)
            )
        ),
        
        # Add footer to navbarPageWithWrapper
        footer = htmlTemplate('html_components/footer.html')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    dimension <- reactive({input$dimension})
    callModule(uploadTab,"upload",pool,dimension)
    callModule(managementTab,"management",pool,dimension)
    onStop(function() poolClose(pool))
}

# Run the application 
shinyApp(ui = ui, server = server)
