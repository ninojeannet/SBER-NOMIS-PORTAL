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
library(DataCombine)
library(shinyvalidate)
library(formattable)
library(sodium)
library(zip)
library(future)
library(purrr)
library(waiter)

## Load Shiny extensions functions ################################################
source('./utils/shiny_extensions.R')
## Load tabs modules ##############################################################
source('./modules/management_tab/management_tab.R')
source('./modules/upload_tab/upload_tab.R')
source('./modules/download_tab/download_tab.R')
source('./modules/login/login.R')
source('./modules/portal_tab/portal_tab.R')
source('./modules/editableDT/editableDT.R')

source('app_config.R')

if (ENV == 'development') {
    # Compile CSS from Sass
    sass::sass(
        sass::sass_file('assets/sass/main.scss'), 
        output = 'www/main.css',
        options = sass::sass_options(output_style = 'compressed')
    )
    
    # Compile and minify JavaScript
    js_parser()
}

options(shiny.maxRequestSize=100*1024^2)

pool <- dbPool(
    drv = RMySQL::MySQL(),
    dbname = DB_NAME,
    host = HOSTNAME,
    port = DB_PORT,
    username = USERNAME,
    password = PASSWORD)

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
        # Create Navabar page with login
        withLoginAction(
        # Pass in the output of shiny navbarPage()
        navbarPage(
            id = 'main-nav',
            # Load the custom logo for the navbar title
            htmlTemplate('./html_components/logo.html'),
            
            # Set a window browser window title
            windowTitle = 'NOMIS DATA PORTAL',
            # Create the home tab
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('home'),tags$span('Home', class = 'navbar-menu-name')),
                # Load the home page template with some icons
                htmlTemplate(
                    './html_components/home.html',
                    dlTabLink = actionLink('aboutDlLink', 'Download tab'),
                    extLinkIcon = icon('external-link-alt', class = 'ext-link')
                ),
                value = 'aboutTab'
            ),
            # Create the visualisation tab
            tabPanel(
                # Create a tab title with an icon
                tags$span(icon('chart-bar'),tags$span('Visualisation', class = 'navbar-menu-name')),
               )
        ),
        # Add the login module UI
        loginUI('login')
        ),
        
        # Add footer to navbarPageWithWrapper
        footer = htmlTemplate('html_components/footer.html')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    user <- callModule(login, 'login', pool)
    dimension <- reactive({input$dimension})
    
    # onStop(function() poolClose(pool))
    
    observeEvent(user$role, {
        
        if (user$role %in% c('sber', 'admin')) {
            appendTab(
                'main-nav',
                # Create the upload tab
                tabPanel(
                    # Create a tab title with an icon
                    tags$span(icon('upload'),tags$span('Upload', class = 'navbar-menu-name')),
                    # Load the uploadTab module UI elements
                    uploadTabUI('upload')
                )
            )
            
            callModule(uploadTab,"upload",pool,dimension)
            ## Generate dataManagementTab #################################################
            # Create the data management tab
            appendTab(
                'main-nav',
                tabPanel(
                    # Create a tab title with an icon
                    tags$span(icon('database'),tags$span('Data Management', class = 'navbar-menu-name')),
                    # Load the managementTab module UI elements
                    managementTabUI('management')
                )
            )
            # Load data management server logic
            callModule(managementTab,"management",pool,dimension)
            
        }
        if (user$role %in% c('sber', 'admin','intern')) {
            appendTab(
                'main-nav',
                # Create the download tab
                tabPanel(
                    # Create a tab title with an icon
                    tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
                    # Load the visualisationTab module UI elements
                    downloadTabUI('download')
                    #visualisationTabUI('1', grabSampleDf, hfDf, sites, grabSampleParameters, hfParameters)
                )
            )
            callModule(downloadTab,"download",pool)
        }
        
        if (user$role == 'admin') {
            ## Generate usersTab ##########################################################
            # Create users tab
            appendTab(
                'main-nav',
                tabPanel(
                    # Create a tab title with an icon
                    tags$span(icon('user'), tags$span('Portal', class = 'navbar-menu-name')),
                    portalTabUI('portal'),
                    value = 'portal'
                )
            )
            # Load users tab server logic
            callModule(portalTab, 'portal', pool)
        }
    })
}

shinyApp(ui, server, onStart = function() {
    cat("Doing application setup\n")
    
    onStop(function() {
        cat("Doing application cleanup\n")
        poolClose(pool)
    })
})