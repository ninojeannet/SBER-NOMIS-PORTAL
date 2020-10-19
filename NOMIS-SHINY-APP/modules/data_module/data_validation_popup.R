
validation_popup <- function(button_id,text_id){
  
  showModal(
    modalDialog(
      div(id=("validate"),
          tags$head(tags$style(".modal-dialog{ width:600px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), 
          fluidPage(
            fluidRow(
              h2("Validate modifications"),
              div(class="text_out",textOutput(text_id)),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

show_validation_popup <- function(tablename,updatedValues,output,ns,isFileUpload,out=NULL){
  if (isFileUpload){
    l <- updatedValues
  }
  else{
    l <- out[updatedValues,1]
    l <- sort(l[!duplicated(l)])
  }
  # primaryKey <- tableOptions[[tablename]][["primary"]]
  validation_popup(ns("submit"),ns("updated_values"))
  output$updated_values <- renderText({l})
}
