## This module contains the UI and server code for the portal actions tab

## Create module UI ###############################################################

portalManageUI <- function(id) {
  # Create the UI for the portalActions module
  # Parameters:
  #  - id: String, the module id
  # 
  # Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'portal-actions',
    div(
      class = 'action',
      h2('Force restart app'),
      p('Create a new ', tags$code('restart.txt'), 'file in the app root directory.'),
      actionButton(ns('restart'), 'Restart', icon = icon('redo'), class = 'custom-style custom-style--primary')
    ),
    div(
      class = 'action',
      h2('Create SQL DB backup file'),
      p('Create a new DB backup in the ', tags$code('app_dir/db_backups'), ' directory.'),
      actionButton(ns('backup'), 'Backup', icon = icon('save'), class = 'custom-style custom-style--primary')
    ),
    div(
      class = 'action',
      h2('Manage backup files'),
      p('Download or delete a DB backup file.'),
      selectInput(ns('backupFile'), 'Backup file', choices = c('Choose file', list.files('./db_backups', pattern = '.sql'))),
      div(
        class = 'btn-group',
        # Create and return a downloadButton disabled by default
        disabled(
          # Add "onclick = 'return false;'" additional attribute to disable the button which is in reality a hyper link
          downloadButton(ns('download'), class = 'custom-style custom-style--primary', onclick = 'return false;')
        ),
        actionButton(ns('delete'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary'),
      )
    )
  )
}



## Create module server function ##################################################

portalManage <- function(input, output, session) {
  # Create the logic for the portalActions module
  # Parameters:
  #  - input, output, session: Default needed parameters to create a module
  # 
  # Returns NULL
  
  ## Track action to confirm #####################################################
  
  actionToConfirm <- reactiveVal('')
  
  
  
  ## Force restart logic ##########################################################
  
  # Restart button event
  observeEvent(input$restart, ignoreInit = TRUE, {
    # Save action to confirm
    actionToConfirm('restart')
    # print("ici")
    # Show confirmation modal
    confirmationModal('Force restart the App?', noAction = TRUE)
  })
  
  # Restart function
  restartApp <- function() {
    print("ici")
    tryCatch(
      {
        # Create a new restart.txt in the app root directory
        system2('touch', args = c('./restart.txt'), stdout = TRUE, stderr = TRUE)
        
        # Refresh the page
        session$reload()
      },
      warning = function(w) showNotification(
        paste(
          'Could not restart the app... ',
          w$message,
          sep = '\n'
        ),
        type = 'error'
      )
    )
  }
  
  
  
  
  
  ## Backup logic ################################################################
  
  # Backup button event
  observeEvent(input$backup, ignoreInit = TRUE, {
    # Save action to confirm
    actionToConfirm('backup')
    
    # Show confirmation modal
    confirmationModal('Create new backup?', noAction = TRUE)
  })
  
  # Backup function
  backupDB <- function() {
    # You will need an '.my.cnf' file in the home directory of the app user
    # With the user, password, host and port infos, e.i.:
    # [client]
    # user=<user_name>
    # password=<password>
    # host=<host>
    # port=<port>
    
    # Create file name
    backupFile <- paste0('./db_backups/', gsub('[ :-]', '', Sys.time()), '_', DB_NAME, '_dump.sql')
    errorFile <- tempfile()
    
    # Create command
    command <- 'mysqldump'
    if (Sys.info()["sysname"] == 'Darwin') command <- paste0('/usr/local/mysql/bin/', command)
    
    # Create The backup
    system2(command, args = c('--databases', DB_NAME, '--add-drop-database', '-y'),
            stdout = backupFile, stderr = errorFile)
    
    # If error
    if (file.size(errorFile) > 0) {
      showNotification(
        paste(
          'Could not backup the DB... ',
          readr::read_file(errorFile),
          sep = '\n'
        ),
        type = 'error'
      )
    } else {
      # Show success
      showNotification('DB backup successfully created!', type = 'message')
    }
  }
  
  
  
  
  ## Delete backup logic ################################################################
  
  # Delete button event
  observeEvent(input$delete, ignoreInit = TRUE, {
    req(input$backupFile, input$backupFile != 'Choose file', input$backupFile != '')
    
    # Save action to confirm
    actionToConfirm('delete')
    
    # Show confirmation modal
    confirmationModal(paste('Delete this backup', input$backupFile, '?'), noAction = TRUE)
  })
  
  # Backup function
  deleteBackup <- function(backupFile) {
    # Get file path
    filePath <- list.files('./db_backups', pattern = backupFile, full.names = TRUE)
    
    # Save removed status
    removed <- FALSE
    
    # If a file is found try to delete
    if (length(filePath) == 1) removed <- file.remove(filePath)
    
    if (removed) {
      # Show success
      showNotification('Backup file successfully deleted!', type = 'message')
    } else {
      showNotification('Could not delete the backup... ', type = 'error')
    }
  }
  
  
  
  
  
  ## Download backup logic ################################################################
  
  # Create an observeEvent that react to backupFile to set downloadButton state
  observeEvent(input$backupFile, ignoreInit = TRUE, {
    # Get backup file
    backupFile <- input$backupFile
    
    # Create message to send to client in as a list containing:
    #  - id: the downloadButton id defined in the UI
    #  - disable: boolean, indicate is the button is disabled
    messageList <- list(
      'id' = session$ns('download'),
      'disable' = FALSE
    )
    
    
    if (backupFile != 'Choose file' & backupFile != '') {
      # Style the button
      enable('download')
      # Inform UI that button needs to be enabled
      messageList$disable <- FALSE
    } else {
      # Style the button
      disable('download')
      # Inform UI that button needs disabled
      messageList$disable <- TRUE
    }
    
    # Convert the list message to JSON
    messageJSON <- toJSON(messageList, auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle downloadButton state
    # Linked to some JavaScript defined in './assets/js/download_button_state.js'
    session$sendCustomMessage('toggleDownloadButton', messageJSON)
  })
  
  # Create download handler
  output$download <- downloadHandler(
    filename = function() {
      input$backupFile
    },
    content = function(file) {
      # Get file path
      filePath <- list.files('./db_backups', pattern = input$backupFile, full.names = TRUE)
      
      # If a file is found
      if (length(filePath) == 1) {
        # Get content
        fileContent <- readr::read_file_raw(filePath)
        
        # Write file to download
        readr::write_file(fileContent, file)
      } else {
        # Otherwise return error file
        readr::write_file(paste0('Error: could not find file (', file, ')...'), file)
      }
    }
  )  
  
  
  
  
  
  ## Confirmation logic ##########################################################
  
  # YES button
  observeEvent(input$YES, ignoreInit = TRUE, {
    print("ici")
    req(actionToConfirm() != '')
    if (actionToConfirm() == 'restart') {
      restartApp()
    } else if (actionToConfirm() == 'backup') {
      backupDB()
    } else if (actionToConfirm() == 'delete') {
      deleteBackup(input$backupFile)
    }
    
    if (actionToConfirm() %in% c('backup', 'delete')) {
      updateSelectInput(session, 'backupFile', choices = c('Choose file', list.files('./db_backups', pattern = '.sql')))
    }
    
    # Reset action to confirm
    actionToConfirm('')
    
    # Remove modal
    removeModal()
  })
  
  # NO button
  observeEvent(input$NO, ignoreInit = TRUE, {
    # Reset action to confirm
    actionToConfirm('')
    
    # Close modal
    removeModal()
  })
  
  
  
  
  
  
  ## Upload sensor data logic #####################################################
  
  # Create an observeEvent that react to file upload
  observeEvent(input$upload, ignoreInit = TRUE, {
    # Show spinner
    show_modal_spinner(spin = 'cube-grid', color = '#e24727',
                       text = 'Parsing zip file...')
    
    # Get uploaded file info
    uploadedFile <- input$upload
    # Check requirements
    # Must be a zip file
    if (uploadedFile$type != 'application/zip') {
      showNotification('Please upload a zip archive file.', type = 'error')
      # Remove spinner
      remove_modal_spinner()
      return()
    }
    # Cannot be empty
    if (uploadedFile$size == 0) {
      showNotification('File cannot be empty!', type = 'error')
      # Remove spinner
      remove_modal_spinner()
      return()
    }
    
    # Get zip content
    content <- tryCatch(
      {
        system2('unzip', args = c('-l', uploadedFile$datapath), stdout = TRUE, stderr = TRUE)
      },
      warning = function(w) return(w$message)
    )
    
    # Check for error
    if (length(content) == 1) {
      showNotification(
        paste(
          'Could not check zip content...',
          content,
          sep = '\n'
        ),
        type = 'error'
      )
      # Remove spinner
      remove_modal_spinner()
      return()
    }
    
    # Parse content to matrix
    content <- content[c(-1, -3, -(length(content)-1), -length(content))] %>%
      str_trim() %>% str_split(pattern = '[:space:]+', simplify = TRUE)
    
    # Get content file names
    content <- `colnames<-`(content, content[1,])[-1, 'Name']
    
    # Check content
    if (!all(
      # Only one folder
      length(grep('/$', content)) == 1,
      # Correct folder name
      any(grepl('^HF_data/$', content)),
      # Correct files
      any(grepl('^HF_data/10min_data.csv$', content)),
      any(grepl('^HF_data/6H_data.csv$', content)),
      any(grepl('^HF_data/12H_data.csv$', content)),
      any(grepl('^HF_data/24H_data.csv$', content))
    )) {
      showNotification('Zip file does not have the correct content...', type = 'error')
      # Remove spinner
      remove_modal_spinner()
      return()
    }
    
    # Copy zip file into data directory
    # If the copy failed, notify and return
    if (!file.copy(uploadedFile$datapath, './data/HF_data.zip', overwrite = TRUE)) {
      showNotification('Could not copy the zip file to the data directory...', type = 'error')
      # Remove spinner
      remove_modal_spinner()
      return()
    }
    
    # Unzip
    tryCatch(
      {
        system2('unzip', args = c('-o', './data/HF_data.zip', '-d', './data'), stdout = TRUE, stderr = TRUE)
        showNotification('Successfully uploaded and unzipped new sensor data.', type = 'message')
      },
      warning = function(w) showNotification(
        paste(
          'Could not unzip file...',
          w$message,
          sep = '\n'
        ),
        type = 'error'
      )
    )
    
    # Remove spinner
    remove_modal_spinner()
  })
  
  
  
  
  
  
  ## Download sensor data logic ###################################################
  
  # Create download handler for sensor data
  output$sensorDownload <- downloadHandler(
    filename = 'HF_data.zip',
    content = function(file) {
      file.copy('./data/HF_data.zip', file)
    },
    contentType = 'application/zip'
  )  
}
