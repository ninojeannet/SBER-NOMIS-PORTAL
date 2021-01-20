


managementProtocolTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  # Create and return the payout
  tagList(
    # Create a main header
    div(
      class= 'main-inputs',
      h1("Protocols Management"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
        
      )),
    # htmlTemplate('./html_components/user_tab_info.html'),
    editableDTUI(ns('protocol'))
  )
}

managementProtocolTab <- function(input, output, session,pool){
  
  
  # Call editableDT module
  callModule(editableDT, 'protocol', pool = pool, tableName = 'protocol', element = 'protocol',
             tableLoading = expression(
               getTable("protocol",pool)
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(name, filename)
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id_protocol,name, filename)
             ),
             creationExpr = expression(
               saveFile(input$filename$name,input$filename$datapath,"www/","protocols"),
               df <- data.frame(name=input$name,filename=input$filename$name),
               saveData(df,"protocol",TRUE,pool)
             ),
             updateExpr = expression(
               if(!is.null(input$filename)){
                 saveFile(input$filename$name,input$filename$datapath,"www/","protocols")
                 df <- data.frame(id_protocol=editedRow()$id_protocol,name=input$name,filename=input$filename$name)
               }
               else{
                 df <- data.frame(id_protocol=editedRow()$id_protocol,name=input$name,filename=editedRow()$filename)
               },
               saveData(df,"protocol",TRUE,pool)
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'protocol',
                 ids = selectedRowIds
               ),
               for (filename in selectedRows[["filename"]]) {
                 deleteFile("www/protocols/",filename)
               }
               # 
             ),
             validatorCreateFunction = function(parameters){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% parameters) "Parameter's name already exists")
               iv$add_rule("filename", sv_required())
               return(iv)
             },
             validatorUpdateFunction = function(expeditions){
               iv <- InputValidator$new()
               iv$add_rule("name", sv_required())
               iv$add_rule("name", ~ if (str_to_lower(.) %in% expeditions) "Parameter's name already exists")
               return(iv)
             })
}


