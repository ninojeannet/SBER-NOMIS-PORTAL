## This module contains the UI and server code for the Management tab

source('./modules/management_tab/management_sidebar.R')
source('./modules/management_tab/management_main.R')

## Create module UI ###############################################################

managementTabUI <- function(id) {
# Create the UI for the downloadTab module
# Parameters:
#  - id: String, the module id
  ns <- NS(id)
  
  # Create namespace
  
  # Create the sidebarLayout
  sidebarLayout(
    # Create a sidebar with the innerModule first unit input UI elements inside
    sidebarPanel(
      id = ns('sidebar-management'),
      managementSidebarUI(ns("sidebar")),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main-management'),
      managementMainUI("1"),
      width = 8
    )
  )

}

managementTab <- function(input, output, session,pool){
  callModule(managementSidebar,"sidebar")
  callModule(managementMain,"1")
  
}

