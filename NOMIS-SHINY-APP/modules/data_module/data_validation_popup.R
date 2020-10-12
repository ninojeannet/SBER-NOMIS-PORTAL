
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