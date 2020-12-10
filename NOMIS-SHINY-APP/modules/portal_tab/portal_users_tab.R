## This module contains the UI and server code for the users management tab

## Create module UI ###############################################################

usersTabUI <- function(id) {
  # Create the UI for the usersTab module
  # Parameters:
  #  - id: String, the module id
  # 
  # Returns a tagList with the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return the payout
  tagList(
    # Create a main header
    h1(class = 'global-header', 'Users Management'),
    htmlTemplate('./html_components/user_tab_info.html'),
    editableDTUI(ns('users'))
  )
}



## Create module server function ##################################################

usersTab <- function(input, output, session, pool) {
  # Create the logic for the usersTab module
  # Parameters:
  #  - input, output, session: Default needed parameters to create a module
  #  - pool: The pool connection to the database
  # 
  # Returns NULL
  
  # Call editableDT module
  callModule(editableDT, 'users', pool = pool, tableName = 'user', element = 'user',
             tableLoading = expression(
               getUsers(pool) %>%
                 # Cast data types
                 mutate(
                   role = as.factor(role),
                   across(ends_with('_at'), ymd_hms),
                   active = as.logical(active)
                 )
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(name, role, active) %>% mutate(password = character(0))
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id_user, name, role, active) %>% mutate(password = as.character(NA))
             ),
             creationExpr = expression(
               createUser(
                 pool = pool,
                 username = input$name,
                 password = input$password,
                 role = input$role,
                 active = input$active
               )
             ),
             updateExpr = expression(
               updateUser(
                 pool = pool,
                 user = editedRow(),
                 username = input$name,
                 password = input$password,
                 role = input$role,
                 active = input$active
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'user',
                 ids = selectedRowIds
               )
             ),
             validatorCreateFunction = function(names){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% names) "Username already exists")
               iv$add_rule("role", sv_required())
               iv$add_rule("password", sv_required())
               return(iv)
             },
             validatorUpdateFunction = function(names){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% names) "Username already exists")
               iv$add_rule("role", sv_required())
               return(iv)
             })
}
