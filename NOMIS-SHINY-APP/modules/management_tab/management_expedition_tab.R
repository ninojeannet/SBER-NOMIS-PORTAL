
## Create module UI ###############################################################

# Create the UI for the managementExpeditionTab module
# Parameters:
#  - id: String, the module id
# 
managementExpeditionTabUI <- function(id) {
  # Create namespace
  ns <- NS(id)
  
  # Create and return the payout
  tagList(
    # Create a main header
    div(
      class= 'main-inputs',
      h1("Expeditions Management"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
        
      )),
    # htmlTemplate('./html_components/user_tab_info.html'),
    editableDTUI(ns('expedition'))
  )
}



## Create module server function ##################################################

# Create the logic for the usersTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
managementExpeditionTab <- function(input, output, session, pool) {

  # Call editableDT module
  callModule(editableDT, 'expedition', pool = pool, tableName = 'expedition', element = 'expedition',
             tableLoading = expression(
               buildExpeditionTable(pool)
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(name, abbreviation, range)
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id_expedition,name, abbreviation, range)
             ),
             creationExpr = expression(
               saveExpeditionInDB(input$name,input$abbreviation,pool),
               for (range in input$ranges) {
                 print(range)
                 range <- str_split(range," - ")
                 saveRangeInDB(input$name,as.numeric(range[[1]][1]),as.numeric(range[[1]][2]),pool)
               }
             ),
             updateExpr = expression(
               updateExpeditionInDB(pool,editedRow(),input$name,input$abbreviation,input$ranges)
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'expedition',
                 ids = selectedRowIds
               )
             ),
             validatorCreateFunction = function(expeditions){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% expeditions) "Expedition's name already exists")
               iv$add_rule("abbreviation", sv_required())
               iv$add_rule("ranges", sv_required())
                return(iv)
             },
             validatorUpdateFunction = function(expeditions){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% expeditions) "Expedition's name already exists")
               iv$add_rule("abbreviation", sv_required())
               iv$add_rule("ranges", sv_required())
               return(iv)
             },
             extraValidatorFunction = function(){
              iv_range <-InputValidator$new()
              iv_range$add_rule("range", ~ if (!is_valid_range(.)) "Please insert a valid range of glacier")
              iv_range$enable()
              is_valid_range <- function(newRange) {
                b <- newRange[2] > newRange[1] & newRange[2] < MAX+1 & newRange[1] > MIN-1 & !is.na(newRange[1]) & !is.na(newRange[2])
                if(!b)
                  disable("add")
                else
                  enable("add")
                return(b)
              }
              return(iv_range)
          })
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    title <- 'Expeditions management help'
    template <-'./html_components/help_manage_expedition.html'
    
    showModal(modalDialog(
      title = title,
      htmlTemplate(template),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}
