
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
    h1(class = 'global-header', 'Expeditions Management'),
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
               buildExpeditionTable(pool)%>%
                 mutate(
                 range = as.list(range))
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(name, abreviation, range)
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id_expedition,name, abreviation, range)
             ),
             creationExpr = expression(
               saveExpeditionInDB(input$name,input$abreviation,pool),
               for (range in input$ranges) {
                 range <- str_split(range," - ")
                 saveRangeInDB(input$name,as.numeric(range[[1]][1]),as.numeric(range[[1]][2]),pool)
               }
             ),
             updateExpr = expression(
               updateExpeditionInDB(pool,editedRow(),input$name,input$abreviation,input$ranges)
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'expedition',
                 ids = selectedRowIds
               )
             ),
             validatorCreateFunction = function(){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("abreviation", sv_required())
               iv$add_rule("ranges", sv_required())
                return(iv)
             },
             validatorUpdateFunction = function(){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("abreviation", sv_required())
               iv$add_rule("ranges", sv_required())
               return(iv)
             },
             extraValidatorFunction = function(){
              iv_range <-InputValidator$new()
              iv_range$add_rule("range", ~ if (!is_valid_range(.)) "Please insert a valid range of glacier")
              iv_range$enable()
              is_valid_range <- function(newRange) {
                b <- newRange[2] > newRange[1] & newRange[2] > 0 & newRange[1] > 0 & !is.na(newRange[1]) & !is.na(newRange[2])
                if(!b)
                  disable("add")
                else
                  enable("add")
                return(b)
              }
              return(iv_range)
          })
}
