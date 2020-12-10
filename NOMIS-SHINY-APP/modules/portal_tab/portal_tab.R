
source('./modules/portal_tab/portal_manage_tab.R')
source('./modules/portal_tab/portal_users_tab.R')


portalTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Manage users',
      usersTabUI(ns("users"))
      
    ),
    tabPanel(
      'Manage portal',
      portalManageUI(ns("manage"))
    )
  )
}

portalTab <- function(input, output, session,pool,dimension){
  callModule(usersTab,"users",pool)
  callModule(portalManage,"manage")

  
  
}


