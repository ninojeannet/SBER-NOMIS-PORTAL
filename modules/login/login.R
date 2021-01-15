## This module contains the UI and server code for the Login

## Create module UI ###############################################################

# Create the UI for the login module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
loginUI <- function(id) {
  # Create namespace
  ns <- NS(id)
  
  # Create and return a div
  div(
    class = 'navbar-login',
    # Create a login link
    actionLink(ns('showLoginForm'), 'Log In', class = 'custom-links'),
    # Create a HTML output that will contain the logged in user info and logout link
    # Hidden when logged out
    hidden(
      htmlOutput(ns('userInfo'))
    )
  )
}

## Create module server function ##################################################

# Create the logic for the login module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns the reactive values containing the user info
login <- function(input, output, session, pool) {
  ## Default User creation #########################################################
  
  # Create a some reactive values linked to the login status
  user <- reactiveValues(
    loggedin = FALSE,
    name = NULL,
    role = 'visitor',
    error = ''
  )
  
  ## Login modal form #############################################################
  
  # Create a modal dialog
  loginForm <- modalDialog(
    title = 'Log In', size = 's',
    div(
      class = 'login-form',
      # Add an text output to log the errors
      textOutput(session$ns('loginError')),
      # Username and password inputs
      textInput(session$ns('username'), 'Username'),
      passwordInput(session$ns('password'), 'Password'),
    ),
    # Action buttons
    footer = tagList(
      actionButton(session$ns('login'), 'Log In', class = 'custom-style custom-style--primary'),
      actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
    )
  )
  
  ## Login modal display ##########################################################
  
  # Create observeEvent that react to the showLoginForm button
  observeEvent(input$showLoginForm, ignoreInit = TRUE, {
    req(user$loggedin == FALSE)
    showModal(loginForm)
  })
  
  
  ## Login logic ##################################################################
  
  # Create an observeEvent that react to the login button
  observeEvent(input$login, ignoreInit = TRUE, {
    req(user$loggedin == FALSE)
    
    # Get user from db
    userResult <- loginUser(pool, input$username)
    
    # If a user was found, verify password
    if (nrow(userResult) == 1) {
      # Update user reactive values if correct password
      if (sodium::password_verify(userResult$password, input$password)) {
        user$loggedin <- TRUE
        user$name <- userResult$name
        user$role <- userResult$role
        
        # Remove login form
        removeModal()
        # Show login success notification
        showNotification('Logged in successfully!', type = 'message')
        
        # Stop here
        return()
      }
    }
    
    # Save errors
    user$error <- 'Incorrect username / password combination!'
  })
  

  ## Closing login modal logic ####################################################
  
  # Create an observeEvent that react to the cancel button
  observeEvent(input$cancel, ignoreInit = TRUE, {
    # Clear user loggin error
    user$error <- ''
    
    # Close modal
    removeModal()
  })
  
  
  ## Error display logic ##########################################################
  
  # Render the error with validate in a renderText
  output$loginError <- renderText(shiny::validate(
    errorClass = 'form',
    need(FALSE, message = user$error)
  ))

  
  ## Log In Status logic ##########################################################
  
  # Create an observeEvent that react to the log in status update
  observeEvent(user$loggedin, ignoreInit = TRUE, {
    # Show and hide the correct UI element depending on the login status
    toggleElement('showLoginForm', condition = !user$loggedin)
    toggleElement('userInfo', condition = user$loggedin)
    
  })
  
  # Render the loggin status if user is logged in
  output$userInfo <- renderUI({
    if(user$loggedin) {
      htmlTemplate(
        './html_components/user_status.html',
        username = user$name,
        role = user$role,
        logout = actionLink(session$ns('logout'), 'Log Out', class = 'custom-links')
      )
    }
  })
  

  ## Log out logic ################################################################
  
  # Create an observeEvent that react to the logout button
  observeEvent(input$logout, ignoreInit = TRUE, {
    req(user$loggedin)
    # Refresh browser
    session$reload()
  })
  
  ## Module return value ##########################################################
  
  # Return the the user status
  return(user)
}