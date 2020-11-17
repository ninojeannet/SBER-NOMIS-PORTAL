## This module contains the UI and server code for the editable Data Table

## Create module UI ###############################################################

# Create the UI for the login module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
editableDTUI<- function(id) {

  # Create namespace
  ns <- NS(id)
  
  # Create and return a div
  div(
    class = 'table-with-controls',
    div(
      class = 'btn-group table-controls',
      actionButton(ns('create_top'), 'New', icon = icon('plus'), class = 'custom-style'),
      actionButton(ns('edit_top'), 'Edit', icon = icon('edit'), class = 'custom-style'),
      actionButton(ns('delete_top'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary')
    ),
    # Create a table of users
    DTOutput(ns('table'))
  )
}



## Create module server function ##################################################
# Create the logic for the editableDT module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - tableName: String, the name of the SQL table to display
#  - element: String, the name that characterize a single element of the table
#  - tableLoading: Expression, the expression to run in order to get the table from the SQL
#  - templateInputsCreate: Expression, the expression to run in order to create an empty df for inputs creation.
#                          The column names will be used as label and id and the column types for the input type selection.
#                          You can use the 'inputsTemplate' symbol in your expression which is an empty df with the same format as the 'tableLoading' output.
#  - templateInputsEdit: Expression, the expression to run in order to create a df containing only the row to be edited for inputs creation.
#                          The column names will be used as label and id and the column types for the input type selection.
#                          The values will be used to populate the inputs.
#                          You can use the 'selectedRow' symbol in your expression which is a df containing the selected row with the same format as the 'tableLoading' output.
#  - creationExpr: Expression, the expression to run in order to create a new row in the SQL database.
#                  You can refer to all the inputs created via 'templateInputsCreate' with input$<column name>.
#  - creationExpr: Expression, the expression to run in order to update a row in the SQL database.
#                  You can refer to all the inputs created via 'templateInputsEdit' with input$<column name>.
#                  You can use the editedRow() reactive value that return a df containing the row being edited with all the previous values and id.
#  - deleteExpr: Expression, the expression to run in order to delete rows from the SQL database
#                You can use the 'selectedRowIds' symbol in your expression which is the a numeric vector of the selected row ids.
#  - validatorCreateFunction :  Function, return a validator for the create modal form.
#  - validatorUpdateFunction : Function, return a validator for the update modal form.
#  - extraValidatorFunction : Function, return an extra validator for any modal form. Not Mandatory
#  - outputTableExpr: Expression, the expression to run in order to apply modification to the df before create the datatable
#                     You can use the 'loadedTable' symbol in your expression which is the loaded df.
# 
# Returns NULL
editableDT <- function(input, output, session, pool, tableName, element,
                       tableLoading, templateInputsCreate, templateInputsEdit,
                       creationExpr, updateExpr, deleteExpr,validatorCreateFunction,
                       validatorUpdateFunction,extraValidatorFunction=NULL, outputTableExpr = NULL) {

  
  ## Table loading ################################################################
  
  # Create a reactive value that will be used as a trigger to reload the table
  reloadTable <- reactiveVal(0)
  
  # Create a reactive expression that load and return the table
  loadTable <- reactive({
    # Call reactive value to trigger a reload if changed
    reloadTable()
    
    # Retrieve table
    eval(tableLoading)
  })
  
  
  
  ## Modal creation and error display ###############################################################
  
  # Create a function to create and show a modal for either new element creation or element editing
  showInputsModal <- function(type, pool, tableName, element, inputsTemplate, session = getDefaultReactiveDomain()) {
    # Take correct action button name
    btnName <- str_to_sentence(type)
    if (type == 'edit') btnName <- 'Update'
    
    # Take correct title
    if (type == 'create') {
      title <- paste('New', element)
    } else {
      title <- str_to_sentence(paste(type, element))
    }
    
    # Create and show modal
    showModal(modalDialog(
      title = title, size = 's',
      div(
        class = 'table-edit-form',
        textOutput(session$ns('form_error')),
        createInputs(df = inputsTemplate, pool = pool, table = tableName),
      ),
      footer = tagList(
        actionButton(session$ns(type), btnName, class = 'custom-style custom-style--primary'),
        actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
      )
    ))
  }
  
  # Create a reactive value to save error happening in the modal
  modalError <- reactiveVal('')
  
  
  # Create an observeEvent that react to the modal cancel button
  observeEvent(input$cancel, ignoreInit = TRUE, {
    iv_create()$disable()
    iv_update()$disable()
    # Close modal
    removeModal()
  })
  
  
  # Create a function that display the creation and edition errors and remove the modal if none
  handleModalResult <- function(error, type) {
    removeModal()
    reloadTable(reloadTable() + 1)
    showNotification(paste0('Row successfully ', type, 'ed!'), type = 'message')
  }
  
  ## Element creation #############################################################
  
  # Create an observe event that react to both create buttons
  observeEvent(input$create_top, ignoreInit = TRUE, {
    req(input$create_top != 0)
    # Create an empty table with the same format as the loaded one
    inputsTemplate <- loadTable() %>% head(0)
    # Rune the 'templateInputsCreate' expression to get the df template
    inputsTemplate <- eval(templateInputsCreate)
    # Show element creation modal
    showInputsModal(
      type = 'create',
      pool = pool,
      tableName = tableName,
      element= element,
      inputsTemplate = inputsTemplate
    )
  })
  
  iv_create <- reactiveVal(validatorCreateFunction())
  
  # Create an observeEvent that react to the modal create button
  observeEvent(input$create, ignoreInit = TRUE, {
    expeditions <- str_to_lower(loadTable()$name)
    iv_create(validatorCreateFunction(expeditions))
    if(is.function(extraValidatorFunction))
      iv_range <- extraValidatorFunction()
    
    if(iv_create()$is_valid()){
      iv_create()$disable()
      res <-eval(creationExpr)
      handleModalResult(res, 'create')
    }
    else{
      iv_create()$enable()
    }
  })
  
  ## Element editing #################################################################
  
  # Create a reactive value used to store the currently edited row
  editedRow <- reactiveVal(data.frame())
  
  # Create an observe event that react to both edit buttons
  observeEvent(input$edit_top, ignoreInit = TRUE, {
    req(input$edit_top != 0, length(input$table_rows_selected) > 0)
    
    # Allows edition only if there is one row selected
    if (length(input$table_rows_selected) > 1) {
      showModal(modalDialog(
        size = 's',
        p(class = 'error','You can only edit one row at the time.'),
        footer = NULL,
        easyClose = TRUE
      ))
    } else {
      # Get the selected row
      selectedRow <- loadTable()[input$table_rows_selected,]
      # Run the 'templateInputsEdit' expression to get the df template
      selectedRow <- eval(templateInputsEdit)
      # Store the edited row
      editedRow(selectedRow)
      id <- paste0("id_",tableName)
      # Remove the id
      selectedRow[[id]] <- NULL
      # Show element edition modal
      showInputsModal(
        type = 'edit',
        pool = pool,
        tableName = tableName,
        element= element,
        inputsTemplate = selectedRow
      )
    }
  })
  
  iv_update <- reactiveVal(validatorUpdateFunction())
  
  
  # Create an observeEvent that react to the modal edit button
  observeEvent(input$edit, ignoreInit = TRUE, {
    selectedName <- loadTable()[input$table_rows_selected,]$name
    expeditions <- str_to_lower(setdiff(loadTable()$name,selectedName))
    iv_update(validatorUpdateFunction(expeditions))
    if(iv_update()$is_valid()){
      iv_update()$disable()
      res <- eval(updateExpr)
      handleModalResult(res, 'edit')
    }
    else{
      iv_update()$enable()
    }
  })
  
  ## Element deletion #############################################################
  
  # Create an observeEvent that react to both delete buttons
  observeEvent(input$delete_top, ignoreInit = TRUE, {
    req(input$delete_top != 0, length(input$table_rows_selected) > 0)
    confirmationModal('You are about to permanently delete rows from this table. Please confirm your action.')
  })
  
  # Create an observeEvent linked to the YES button of the confirmation modal
  observeEvent(input$YES, ignoreInit = TRUE, {
    # Remove confirmation modal
    removeModal()
    # If rows are selected
    if (length(input$table_rows_selected) > 0) {
      # Get the selected row ids
      id <- paste0("id_",tableName)
      selectedRowIds <- loadTable()[input$table_rows_selected,][[id]]
      # Run the element deletion expression and retrieve the output message
      error <- eval(deleteExpr)
      # Reload table
      reloadTable(reloadTable() + 1)
    }
  })
  
  ## Table rendering ##############################################################
  
  # Render the DataTable
  output$table <- renderDT({
    loadedTable <- loadTable()
    
    if (!is.null(outputTableExpr)) loadedTable <- eval(outputTableExpr)
    
    loadedTable %>%
      datatable(rownames = FALSE, options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ))
  })
  
  rangeVector <- reactiveVal(vector())
  # ObserveEvent that react to the ranges input update.
  # Store the updated ranges in rangevector reactValue
  observeEvent(input$ranges,{
    rangeVector(input$ranges)
  },ignoreNULL  = FALSE)
  
  # ObserveEvent that react to the add input button click.
  # Add the given range to the ranges input
  observeEvent(input$add,{
    newRange <- isolate(input$range)
    tmp <- c(rangeVector(),paste0(newRange[1]," - ",newRange[2]))
    rangeVector(tmp)
    updateSelectizeInput(session,"ranges",choices = rangeVector(),selected = rangeVector())
  })
}
